{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Parse (
    InlineElem (..)
  , BlockElem  (..)
  , Markdown   (..)
  , parse
  ) where

import qualified Text.Regex as Regex

-- | Markdown AST.
newtype Markdown = Markdown { markdown :: [BlockElem] }
  deriving (Show)

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
  deriving (Show)

-- | All possible inline elements in Markdown.
data InlineElem = InlineElem { elemType :: ElemType, elemContent :: String }
  deriving (Show)

-- TODO: support nested elements
data ElemType =
    Plain
  | Code                    -- ^ HTML <code>
  | Emph                    -- ^ HTML <em>
  | Strong                  -- ^ HTML <strong>
  | Link { url :: String }  -- ^ HTML <a href="...">
  | Math                    -- ^ Use MathJax/KaTeX
  deriving (Show)

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
        Left (Markdown x) -> [Ulist { isOpen = False, elems' = xs }] ++ x
        Right _           -> case Regex.matchRegex ulistPattern s of
          Just x  -> [Ulist { isOpen = True, elems' = xs .+ Block lastItem }]
            where lastItem = parseInline $ last x
          Nothing -> [Ulist { isOpen = True, elems' = init xs .+ Block lastItem }]
            where lastItem = (elems $ last xs) ++ (parseInline $ "\n" ++ s)
    -- TODO: indent
    _ -> undefined

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
headingParser result = Heading {
    level = length $ head result
  , elems = parseInline $ last result }
hruleParser   _      = Hrule
preParser     result = Pre {
    isOpen = True
  , lang = head result
  , text = "" }
ulistParser   result = Ulist {
    isOpen = True
  , elems' = [Block $ parseInline $ last result] }
olistParser   result = Olist {
    isOpen = True
  , start  = read $ head result
  , elems' = [Block $ parseInline $ last result] }
quoteParser   result = Quote {
    isOpen = True
  , elems' = [Para { isOpen = True, elems = parseInline $ head result }] }


parseInline :: String -> [InlineElem]
parseInline s = [InlineElem {elemType = Plain, elemContent = s}]  -- TODO: placeholder


-- | Append element to a list.
(.+) :: [a] -> a -> [a]
xs .+ x = xs ++ [x]


#ifdef DEBUG
import Text.Pretty.Simple (pPrint)

testParse :: IO ()
testParse = let f = foldl parseMarkdown $ Markdown []
  in pPrint $ map f [
      ["```python"]
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
    , ["- a", "```python", "```"]
    -- , ["- A", "  - B", "  - C"]
  ]
#endif
