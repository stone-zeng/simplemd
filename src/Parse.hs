{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Parse
  ( InlineElem (..)
  , BlockElem  (..)
  , Markdown   (..)
  , parse
  ) where

import qualified Text.Regex as Regex
import Debug.Trace

#ifdef DEBUG
import Text.Pretty.Simple (pPrint)
#endif

-- | Markdown AST.
newtype Markdown = Markdown { markdown :: [BlockElem] }

instance Show Markdown where
  show Markdown {..} = "Markdown: " ++ show markdown

-- | All possible block elements in Markdown.
data BlockElem =
    Block   { elems :: [InlineElem] }
  | Hrule                                                            -- ^ HTML <hr />
  | Heading { level :: Int, elems :: [InlineElem] }                  -- ^ HTML <h1>, <h2>, ... <h6>
  | Para    { isOpen :: Bool, elems :: [InlineElem] }                -- ^ HTML <p>
  | Pre     { isOpen :: Bool, lang :: String, text :: String }       -- ^ HTML <pre>
  | Ulist   { isOpen :: Bool, elems' :: [BlockElem] }                -- ^ HTML <ul>
  | Olist   { isOpen :: Bool, start :: Int, elems' :: [BlockElem] }  -- ^ HTML <ol>
  | Quote   { isOpen :: Bool, elems' :: [BlockElem] }                -- ^ HTML <blockquote>

instance Show BlockElem where
  show Hrule        = "Hrule"
  show Block   {..} = "Block" ++ ": " ++ show elems
  show Heading {..} = "Heading (" ++ show level ++ "): " ++ show elems
  show Para    {..} = "Para"  ++ (if isOpen then "+" else "-") ++ ": " ++ show elems
  show Ulist   {..} = "Ulist" ++ (if isOpen then "+" else "-") ++ ": " ++ show elems'
  show Quote   {..} = "Quote" ++ (if isOpen then "+" else "-") ++ ": " ++ show elems'
  show Pre     {..} = "Pre"   ++ (if isOpen then "+" else "-") ++ " (" ++ show lang  ++ "): " ++ show text
  show Olist   {..} = "Olist" ++ (if isOpen then "+" else "-") ++ " (" ++ show start ++ "): " ++ show elems'

-- | All possible inline elements in Markdown.
data InlineElem = InlineElem { elemType :: ElemType, elemContent :: String }

instance Show InlineElem where
  show InlineElem {..} = case elemType of
    Plain     -> show $ "" ++ elemContent ++ ""
    Code      -> show $ "<code>"   ++ elemContent ++ "</code>"
    Emph      -> show $ "<em>"     ++ elemContent ++ "</em>"
    Strong    -> show $ "<strong>" ++ elemContent ++ "</strong>"
    Math      -> show $ "<math>"   ++ elemContent ++ "</math>"
    Link {..} -> show $ "<a href=" ++ url ++ ">" ++ elemContent ++ "</a>"

-- TODO: support nested elements
data ElemType =
    Plain
  | Code                    -- ^ HTML <code>
  | Emph                    -- ^ HTML <em>
  | Strong                  -- ^ HTML <strong>
  | Link { url :: String }  -- ^ HTML <a href="...">
  | Math                    -- ^ Use MathJax/KaTeX

parse :: [String] -> Markdown
parse = foldl parseMarkdown $ Markdown []

parseMarkdown :: Markdown -> String -> Markdown
parseMarkdown (Markdown []) s = result
  where Left result = parseHeading s
                  >>= parseHrule
                  >>= parsePre
                  >>= parseUlist
                  >>= parseOlist
                  >>= parseQuote
                  >>= parsePara
parseMarkdown (Markdown mdElements) s =
  case last mdElements of
    Pre   { isOpen = True, .. } -> Markdown $ init mdElements ++ nextPre   lang text    s
    Para  { isOpen = True, .. } -> Markdown $ init mdElements ++ nextPara  elems        s
    Ulist { isOpen = True, .. } -> Markdown $ init mdElements ++ nextUlist elems'       s
    Olist { isOpen = True, .. } -> Markdown $ init mdElements ++ nextOlist start elems' s
    -- BlockQuote quote Open ->
    --   case s of
    --     "" -> Markdown $ init mdElements ++ [BlockQuote quote Closed]
    --     _ -> undefined
    Block {..} -> let nextMd = markdown $ parseMarkdown (Markdown []) s in
      case last nextMd of
        Ulist {..} -> Markdown [Ulist { isOpen = True, elems' = [Block elems] ++ elems' }]
        Olist {..} -> Markdown [Olist { isOpen = True, start = start, elems' = [Block elems] ++ elems' }]
        _          -> Markdown $ mdElements ++ nextMd
    _ ->
      case s of
        "" -> Markdown mdElements
        _  -> Markdown $ mdElements ++ (markdown $ parseMarkdown (Markdown []) s)


nextPre :: String -> String -> String -> [BlockElem]
nextPre preLang preText s = case s of
  "```" -> [Pre { isOpen = False, lang = preLang, text = preText }]
  _     -> case preText of
    "" -> [Pre { isOpen = True, lang = preLang, text = s }]
    _  -> [Pre { isOpen = True, lang = preLang, text = preText ++ "\n" ++ s }]

nextPara :: [InlineElem] -> String -> [BlockElem]
nextPara para s = case s of
  "" -> [Para { isOpen = False, elems = para }]
  _  -> let result = parseHeading s
                 >>= parseHrule
                 >>= parsePre
                 >>= parseUlist
                 >>= parseOlist
                 >>= parseQuote in
    case result of
      Left (Markdown x) -> [Para { isOpen = True, elems = para }] ++ x
      Right _           -> [Para { isOpen = True, elems = para ++ parseInline ("\n" ++ s) }]

nextUlist :: [BlockElem] -> String -> [BlockElem]
nextUlist xs s = case s of
  "" -> [Ulist { isOpen = False, elems' = xs }]
  _  -> case detectIndent s of
    (0, _) -> let result = parseHeading s
                       >>= parseHrule
                       >>= parsePre
                       >>= parseOlist
                       >>= parseQuote in
      case result of
        -- Close <ul>
        Left (Markdown x) -> [Ulist { isOpen = False, elems' = xs }] ++ x
        Right _           -> case Regex.matchRegex ulistPattern s of
          -- Append a new <li>
          Just x  -> [Ulist { isOpen = True, elems' = xs .+ Block lastItem }]
            where lastItem = parseInline $ last x
          -- Append s to the last <li>
          Nothing -> appendAt xs 0 (Block $ parseInline $ "\n" ++ s)
    (indent, s') -> let indentDepth = indent `div` 2 - 1
                        depth       = detectDepth xs in
      if 0 <= indentDepth && indentDepth <= depth
        then let Markdown md = parseMarkdown (Markdown [last xs]) s' in
          case last md of
            Para {..} -> appendAt xs indentDepth $ Block elems
            _         -> updateAt xs indentDepth $ last md
            -- _         -> updateAt (trace ("\n--> xs: " ++ show xs) xs)
            --                       indentDepth
            --                       (trace ("--> x: " ++ (show $ last md)) (last md))
        else appendAt xs depth (Block $ parseInline $ "\n" ++ s')

nextOlist :: Int -> [BlockElem] -> String -> [BlockElem]
nextOlist k xs s = case s of
  "" -> [Olist { isOpen = False, start = k, elems' = xs }]
  _  -> undefined
    -- TODO: indent
    -- case Regex.matchRegex olistPattern s of
    --   Just result -> init mdElements `mdAppend` Olist Open olStart (listItems ++ [newListItem])
    --     where newListItem = ListInlineItem (parseInline $ last result)
    --   Nothing -> mdElements <:> parseMarkdown (Markdown []) s

-- | Return the number of leading spaces and the sub-string with leading spaces removed.
detectIndent :: String -> (Int, String)
detectIndent "" = (0, "")
detectIndent s  = case head s of
  ' ' -> (n + 1, s')
    where (n, s') = detectIndent $ tail s
  _  -> (0, s)

detectDepth :: [BlockElem] -> Int
detectDepth [] = 0
detectDepth xs = case last xs of
  Ulist {..} -> 1 + detectDepth elems'
  Olist {..} -> 1 + detectDepth elems'
  _          -> 0

appendAt :: [BlockElem] -> Int -> BlockElem -> [BlockElem]
appendAt xs 0 x = [Ulist { isOpen = True, elems' = init xs ++ lastItems }]
  where lastItems = case last xs of
                    Block { elems = e } -> case x of
                      Block {..} -> [Block $ e ++ elems]
                      _          -> [last xs, x]
                    _ -> [last xs, x]
appendAt xs k x = [Ulist { isOpen = True, elems' = init xs ++ lastItems }]
  where lastItems = appendAt (elems' $ last xs) (k - 1) x

updateAt :: [BlockElem] -> Int -> BlockElem -> [BlockElem]
updateAt xs 0 x = [Ulist { isOpen = True, elems' = init xs ++ lastItems }]
  where lastItems = case last xs of
                    Block {..} -> case x of
                      Ulist {..} -> [Block $ elems, Ulist { isOpen = True, elems' = [last elems'] }]
                      _          -> [Block $ elems, x]
                    _ -> [x]
updateAt xs k x = [Ulist { isOpen = True, elems' = init xs ++ lastItems }]
  where lastItems = updateAt (elems' $ last xs) (k - 1) x

atDepth :: [BlockElem] -> Int -> [BlockElem]
atDepth _  0 = []
atDepth xs k = iterate (elems' . last) xs !! k

-- appendAt xs k x = [Ulist { isOpen = True, elems' = init xs .+ Block lastItem }]
--   where lastItem = undefined
-- appendAt Ulist {..} 1 x = Ulist { isOpen = True, elems' = init elems' .+ lastItem }
--   where lastItem = Block $ (elems $ last elems') ++ x
-- appendAt Ulist {..} k x = Ulist { isOpen = True, elems' = init elems' .+ lastItem }
--   where lastItem = appendAt (last elems') (k - 1) x
-- appendAt _ _ _          = undefined

-- | Parse string into Markdown elements.
parseHeading, parseHrule, parsePre, parseUlist, parseOlist, parseQuote, parsePara
  :: String -> Either Markdown String
parseHeading = generateParser headingPattern headingParser
parseHrule   = generateParser hrulePattern   hruleParser
parsePre     = generateParser prePattern     preParser
parseUlist   = generateParser ulistPattern   ulistParser
parseOlist   = generateParser olistPattern   olistParser
parseQuote   = generateParser quotePattern   quoteParser
parsePara s  = Left $ Markdown [Para { isOpen = True, elems = parseInline s }]

-- | Use `pattern` and `parser` to generate a parsing function.
generateParser :: Regex.Regex -> ([String] -> BlockElem) -> (String -> Either Markdown String)
generateParser pattern parser = \s -> case Regex.matchRegex pattern s of
  Just x  -> Left $ Markdown [parser x]
  Nothing -> Right s

-- | Regex patterns for Markdown elements.
-- Note that `<p>` does not need a pattern.
hrulePattern, headingPattern, prePattern, ulistPattern, olistPattern, quotePattern
  :: Regex.Regex
hrulePattern   = Regex.mkRegex "^(\\-{3,}|\\*{3,}|_{3,}) *$"  -- [(---|***|___)]
headingPattern = Regex.mkRegex "^(#{1,6}) (.*)"               -- [(#), (content)]
prePattern     = Regex.mkRegex "^```(.*)"                     -- [(preLang)]
ulistPattern   = Regex.mkRegex "^(\\-|\\*|\\+) (.*)"          -- [(-|*|+), (content)]
olistPattern   = Regex.mkRegex "^([0-9]+)\\. (.*)"            -- [(number), (content)]
quotePattern   = Regex.mkRegex "^> *(.*)"                     -- [(content)]

-- | Parsers for Markdown elements.
-- Note that `<p>` does not need a parser.
hruleParser, headingParser, preParser, ulistParser, olistParser, quoteParser
   :: [String] -> BlockElem
headingParser result = Heading
  { level = length $ head result
  , elems = parseInline $ last result
  }
hruleParser   _      = Hrule
preParser     result = Pre
  { isOpen = True
  , lang = head result
  , text = ""
  }
ulistParser   result = Ulist
  { isOpen = True
  , elems' = [Block $ parseInline $ last result]
  }
olistParser   result = Olist
  { isOpen = True
  , start  = read $ head result
  , elems' = [Block $ parseInline $ last result]
  }
quoteParser   result = Quote
  { isOpen = True
  , elems' = [Para { isOpen = True, elems = parseInline $ head result }]
  }


parseInline :: String -> [InlineElem]
parseInline s = [InlineElem {elemType = Plain, elemContent = s}]  -- TODO: placeholder


-- | Append element to a list.
(.+) :: [a] -> a -> [a]
xs .+ x = xs ++ [x]


#ifdef DEBUG

test_parse :: IO ()
test_parse = pPrint $ map (foldl parseMarkdown $ Markdown [])
    [ ["```python"]
    -- , ["```c", "int"]
    -- , ["```c", "int main () {", "    return 0;", "}", "```"]
    -- , ["```python", "def f:", "\tpass", "```endpython", "```"]
    -- , ["```python", "def f:", "\tpass", "```", "something after", "more thing after"]
    -- , ["# Title", "```python", "# Comment", "def f:", "\tpass", "```", "something after", "## title2"]
    -- , ["Word1", "Word2", "Word3"]
    -- , ["text", "text", "- list1", "- list2", "  - indent", "- item next", "text3"]
    -- , ["Text", "- ul1", "- ul2", "- ul3", "1. ol1", "2. ol2", "1. ol3", "- ul another", "3. ol another"]
    -- , ["> quote"]
    -- , [">q"]
    -- , [">> dq"]
    -- , [">>dq"]
    -- , [">"]
    -- , [">>>"]
    -- , ["> Quote", "> Quote cont.", "> - List in quote 1", "> - List in quote 2", "1. List", "1. List2"]
    -- , ["## Title", "# Title2", "```c", "- help", "1. First", "22. Twenty-two", "1.invalie ol", "-invalid ul", "> quote", ">> invalid quote", ">2invalid quote", "``invalid code"]
    , ["- a", "- b", "- c"]
    , ["- a", "b", "- c"]
    , ["- a", "  ```python", "  def", "  1+1", "  ```"]
    , ["- A", "  - B"]
    , ["- a", "  - b", "  - c"]
    , ["- A1", "  BBBB"]
    , ["- A2", "  CCCC", "- X"]
    , ["- A", "  - B", "  - C", "D"]
    , ["- 1", "  - 2", "    - 3"]
    , ["- 1", "    - 2", "    - 3"]
    -- , ["- 1", "  1. x", "  1. y"]  -- FIXME: Exception: Prelude.undefined
    ]

test_detectDepth :: IO ()
test_detectDepth = do
  let item  = Block $ parseInline "a"
      item' = [item]
      _data = [ [ Ulist { isOpen = True, elems' = item' } ]
              , [ Ulist { isOpen = True, elems' = [ item, item ] } ]
              , [ Ulist { isOpen = True, elems' = [ Ulist { isOpen = True,  elems' = item' } ] } ]
              , [ Ulist { isOpen = True, elems' = [ Ulist { isOpen = False, elems' = item' }, item ] } ]
              ]
  pPrint _data
  pPrint $ map detectDepth _data

test_appendAt :: IO ()
test_appendAt = do
  let item  = Block $ parseInline "a"
      item' = [item]
      -- _data = [ Ulist { isOpen = True, elems' = item' }
      --         , Ulist { isOpen = True, elems' = [ item, item ] }
      --         , Ulist { isOpen = True, elems' = [ Ulist { isOpen = True,  elems' = item' } ] }
      --         , Ulist { isOpen = True, elems' = [ Ulist { isOpen = False, elems' = item' }, item ] }
      --         ]
  -- pPrint _data
  pPrint $ (\xs -> appendAt xs 1 $ Block $ parseInline "Added!") item'
  pPrint $ (\xs -> appendAt xs 1 $ Block $ parseInline "Added!") [ item, item ]
  pPrint $ (\xs -> appendAt xs 1 $ Block $ parseInline "Added!") [ Ulist { isOpen = True,  elems' = item' } ]
  pPrint $ (\xs -> appendAt xs 2 $ Block $ parseInline "Added!") [ Ulist { isOpen = True,  elems' = item' } ]

#endif
