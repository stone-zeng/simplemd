{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Parse
  ( InlineElem (..)
  , BlockElem  (..)
  , Markdown   (..)
  , parse
  ) where

import qualified Data.Map as Map
import qualified Text.Regex as Regex
import Text.RawString.QQ
-- import Debug.Trace

import Emoji

#ifdef DEBUG
import Text.Pretty.Simple (pPrint)
#endif

-- | Markdown AST.
newtype Markdown = Markdown { markdown :: [BlockElem] }
  deriving (Eq)

instance Show Markdown where
  show Markdown {..} = "Markdown: " ++ show markdown

-- | All possible block elements in Markdown.
data BlockElem =
    Hrule                                                            -- ^ HTML <hr />
  | Heading { level :: Int, elems :: [InlineElem] }                  -- ^ HTML <h1>, <h2>, ... <h6>
  | Para    { isOpen :: Bool, elems :: [InlineElem] }                -- ^ HTML <p>
  | Pre     { isOpen :: Bool, lang :: String, text :: String }       -- ^ HTML <pre>
  | Ulist   { isOpen :: Bool, elems' :: [BlockElem] }                -- ^ HTML <ul>
  | Olist   { isOpen :: Bool, start :: Int, elems' :: [BlockElem] }  -- ^ HTML <ol>
  | Quote   { isOpen :: Bool, elems' :: [BlockElem] }                -- ^ HTML <blockquote>
  deriving (Eq)

instance Show BlockElem where
  show Hrule        = "Hrule"
  show Heading {..} = "Heading (" ++ show level ++ "): " ++ show elems
  show Para    {..} = "Para"  ++ (if isOpen then "+" else "-") ++ ": " ++ show elems
  show Ulist   {..} = "Ulist" ++ (if isOpen then "+" else "-") ++ ": " ++ show elems'
  show Quote   {..} = "Quote" ++ (if isOpen then "+" else "-") ++ ": " ++ show elems'
  show Pre     {..} = "Pre"   ++ (if isOpen then "+" else "-") ++ " (" ++ show lang  ++ "): " ++ show text
  show Olist   {..} = "Olist" ++ (if isOpen then "+" else "-") ++ " (" ++ show start ++ "): " ++ show elems'

closeBlockElem :: BlockElem -> BlockElem
closeBlockElem Para  {..} = Para  { isOpen = False, ..}
closeBlockElem Pre   {..} = Pre   { isOpen = False, ..}
closeBlockElem Ulist {..} = Ulist { isOpen = False, ..}
closeBlockElem Olist {..} = Olist { isOpen = False, ..}
closeBlockElem Quote {..} = Quote { isOpen = False, ..}
closeBlockElem x = x

-- | All possible inline elements in Markdown.
data InlineElem =
    Plain      { content :: String }
  | Code       { content :: String }                 -- ^ HTML <code>
  | Del        { content :: String }                 -- ^ HTML <del>
  | Emph       { content :: String }                 -- ^ HTML <em>
  | Strong     { content :: String }                 -- ^ HTML <strong>
  | EmphStrong { content :: String }                 -- ^ HTML <em><strong>
  | Link       { content :: String, url :: String }  -- ^ HTML <a href="...">
  deriving (Eq, Show)


parse :: [String] -> Markdown
parse = foldl parseMarkdown $ Markdown []

parseMarkdown :: Markdown -> String -> Markdown
parseMarkdown (Markdown []) s = Markdown [result]
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
    {-
    Block {..} -> let nextMd = markdown $ parseMarkdown (Markdown []) s in
      case last nextMd of
        Ulist {..} -> Markdown [Ulist { isOpen = True, elems' = [Block elems] ++ elems' }]
        Olist {..} -> Markdown [Olist { isOpen = True, start = start, elems' = [Block elems] ++ elems' }]
        _          -> Markdown $ mdElements ++ nextMd
    -}
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
      Left  x -> [Para { isOpen = False, elems = para }] .+ x
      Right _ -> [Para { isOpen = True,  elems = para ++ parseInline ("\n" ++ s) }]

nextUlist :: [BlockElem] -> String -> [BlockElem]
nextUlist xs s = case s of
  -- An empty line will close <ul>.
  "" -> closeUlist xs
  _  -> case detectIndent s of
    -- No indent afterwards.
    -- If `s` begins with `- ...`, then append a new <li>; otherwise close the <ul>.
    (0, _) -> let result = parseHeading s
                       >>= parseHrule
                       >>= parsePre
                       >>= parseOlist
                       >>= parseQuote in
      case result of
        -- Other objects will close <ul>.
        Left block -> closeUlist xs .+ block
        Right _    -> case Regex.matchRegex ulistPattern s of
          -- Add a new <li>, close previous things.
          Just x  -> [Ulist { isOpen = True, elems' = init xs .+ lastItem .+ nextItem }]
            where lastItem = case last xs of
                    Para { isOpen = True, .. } -> Para { isOpen = False, elems = elems }
                    _                          -> last xs
                  nextItem = Para { isOpen = True, elems = parseInline $ last x}
          -- Append `s` to the last <li> if this <li> ends with a <p>; otherwise close it.
          Nothing -> [Ulist { isOpen = True, elems' = init xs ++ lastItems }]
            where lastItems = case last xs of
                    Para { isOpen = True, .. } ->
                      [Para { isOpen = False, elems = elems ++ (parseInline $ "\n" ++ s) }]
                    _ ->
                      [last xs, Para { isOpen = True, elems = parseInline s} ]
    (indent, s') -> let indentDepth = indent `div` 2 - 1
                        depth       = detectDepth xs in
      if 0 <= indentDepth && indentDepth <= depth
        then case Regex.matchRegex ulistPattern s' of
          -- Append element to `indentDepth` level as <ul>
          Just x -> appendUlistTo xs indentDepth (ulistParser x)
          _ -> case Regex.matchRegex olistPattern s' of
            -- Append element to `indentDepth` level as <ol>
            Just x' -> appendOlistTo xs indentDepth (olistParser x')
            -- Append text to deepest <li>
            _ -> appendTextTo xs depth s'
        -- Append text to deepest <li>
        else appendTextTo xs depth s'


appendUlistTo :: [BlockElem] -> Int -> BlockElem -> [BlockElem]
appendUlistTo xs 0 x = init xs .+ Ulist
  { isOpen = True
  , elems' = [closeBlockElem $ last xs, x]
  }
appendUlistTo xs k x = init xs .+ Ulist
  { isOpen = True
  , elems' = appendUlistTo (elems' $ last xs) (k - 1) x
  }

appendOlistTo :: [BlockElem] -> Int -> BlockElem -> [BlockElem]
appendOlistTo xs 0 x = init xs .+ Olist
  { isOpen = True
  , start  = start x
  , elems' = [closeBlockElem $ last xs, x]
  }
appendOlistTo xs k x = init xs .+ Olist
  { isOpen = True
  , start  = start x
  , elems' = appendOlistTo (elems' $ last xs) (k - 1) x
  }


appendTextTo :: [BlockElem] -> Int -> String -> [BlockElem]
appendTextTo xs 0 x = init xs .+ Para { isOpen = True, elems = (elems $ last xs) ++ parseInline x }
appendTextTo xs k x = init xs .+ Para { isOpen = True, elems = (elems $ last xs) ++ parseInline x }

-- appendTo _ _ _ = undefined

-- [Ulist { isOpen = True, elems' = init xs ++ lastItems }]
--   where lastItems = [last xs, x]


          -- where appendToUlist = [Ulist { isOpen = True, elems' = init xs ++ lastItems }]
          --       lastItems     = case last xs of
          --         Para { isOpen = True, .. } ->
          --           [Para { isOpen = True, elems = elems ++ (parseInline $ "\n[FLAG-3.1]" ++ s') }]
          --         _ ->
          --           [last xs, Para { isOpen = True, elems = parseInline $ "[FLAG-3.2]" ++ s'} ]
      {-
      if 0 <= indentDepth && {- traceShowId -} indentDepth <= {- trace ("\n<" ++ show depth ++ ">\n") -} depth
        then let Markdown md = parseMarkdown (Markdown {- $ trace ("\n[xs]: " ++ show xs) -} xs) s' in
          case last $ trace ("\n[md]: " ++ show md ++ "\n") md of
            Para {..} -> [Ulist { isOpen = True, elems' = md }] -- appendAt xs indentDepth $ Para True elems
            _         -> [Ulist { isOpen = True, elems' = md }] {- updateAt xs indentDepth $ last md -}
        -- One-space or more-than-depth indent, treat it as normal text.
        else [Ulist { isOpen = True, elems' = init xs ++ lastItems }]
            where lastItems = case last xs of
                    Para { isOpen = True, .. } ->
                      [Para { isOpen = True, elems = elems ++ (parseInline $ "\n[FLAG-3.1]" ++ s) }]
                    _ ->
                      [last xs, Para { isOpen = True, elems = parseInline $ "[FLAG-3.2]" ++ s} ]
      -}

-- | Close <ul>, as well as the last <p> element inside this <ul>.
closeUlist :: [BlockElem] -> [BlockElem]
closeUlist xs = [Ulist { isOpen = False, elems' = init xs .+ lastItem }]
  where lastItem = case last xs of
            Para { isOpen = True, .. } -> Para { isOpen = False, elems = elems }
            _                          -> last xs

-- TODO: Unify code with `nextUlist`
nextOlist :: Int -> [BlockElem] -> String -> [BlockElem]
nextOlist k xs s = case s of
  -- An empty line will close <ol>.
  "" -> closeOlist k xs
  _  -> case detectIndent s of
    -- No indent afterwards.
    -- If `s` begins with `- ...`, then append a new <li>; otherwise close the <ol>.
    (0, _) -> let result = parseHeading s
                       >>= parseHrule
                       >>= parsePre
                       >>= parseUlist
                       >>= parseQuote in
      case result of
        -- Other objects will close <ol>.
        Left block -> closeOlist k xs .+ block
        Right _    -> case Regex.matchRegex olistPattern s of
          -- Add a new <li>, close previous things.
          -- The `start` of <ol> will not be affected by the following numbers.
          Just x  -> [Olist { isOpen = True, start = k, elems' = init xs .+ lastItem .+ nextItem }]
            where lastItem = case last xs of
                    Para { isOpen = True, .. } -> Para { isOpen = False, elems = elems }
                    _                          -> last xs
                  nextItem = Para { isOpen = True, elems = parseInline $ last x}
          -- Append `s` to the last <li> if this <li> ends with a <p>; otherwise close it.
          Nothing -> [Olist { isOpen = True, start = k, elems' = init xs ++ lastItems }]
            where lastItems = case last xs of
                    Para { isOpen = True, .. } ->
                      [Para { isOpen = False, elems = elems ++ (parseInline $ "\n" ++ s) }]
                    _ ->
                      [last xs, Para { isOpen = True, elems = parseInline s} ]
    (indent, s') -> undefined

-- | Close <ol>, as well as the last <p> element inside this <ol>.
closeOlist :: Int -> [BlockElem] -> [BlockElem]
closeOlist k xs = [Olist { isOpen = False, start = k, elems' = init xs .+ lastItem }]
  where lastItem = case last xs of
            Para { isOpen = True, .. } -> Para { isOpen = False, elems = elems }
            _                          -> last xs


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
                    {-
                    Block { elems = e } -> case x of
                      Block {..} -> [Block $ e ++ elems]
                      _          -> [last xs, x]
                    -}
                    _ -> [last xs, x]
appendAt xs k x = [Ulist { isOpen = True, elems' = init xs ++ lastItems }]
  where lastItems = appendAt (elems' $ last xs) (k - 1) x

updateAt :: [BlockElem] -> Int -> BlockElem -> [BlockElem]
updateAt xs 0 x = [Ulist { isOpen = True, elems' = init xs ++ lastItems }]
  where lastItems = case last xs of
                    {-
                    Block {..} -> case x of
                      Ulist {..} -> [Block $ elems, Ulist { isOpen = True, elems' = [last elems'] }]
                      _          -> [Block $ elems, x]
                    -}
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
  :: String -> Either BlockElem String
parseHeading = generateParser headingPattern headingParser
parseHrule   = generateParser hrulePattern   hruleParser
parsePre     = generateParser prePattern     preParser
parseUlist   = generateParser ulistPattern   ulistParser
parseOlist   = generateParser olistPattern   olistParser
parseQuote   = generateParser quotePattern   quoteParser
parsePara s  = Left $ Para { isOpen = True, elems = parseInline s }

-- | Use `pattern` and `parser` to generate a parse function.
generateParser :: Regex.Regex -> ([String] -> a) -> (String -> Either a String)
generateParser pattern parser = \s -> case Regex.matchRegex pattern s of
  Just x  -> Left $ parser x
  Nothing -> Right s

-- | Regex patterns for Markdown elements.
-- Note that `<p>` does not need a pattern.
hrulePattern, headingPattern, prePattern, ulistPattern, olistPattern, quotePattern
  :: Regex.Regex
hrulePattern   = Regex.mkRegex [r|^(\-{3,}|\*{3,}|_{3,}) *$|]  -- [(---|***|___)]
headingPattern = Regex.mkRegex [r|^(#{1,6}) (.*)|]             -- [(#), (content)]
prePattern     = Regex.mkRegex [r|^```(.*)|]                   -- [(preLang)]
ulistPattern   = Regex.mkRegex [r|^(-|\*|\+) (.*)|]            -- [(-|*|+), (content)]
olistPattern   = Regex.mkRegex [r|^([0-9]+)\. (.*)|]           -- [(number), (content)]
quotePattern   = Regex.mkRegex [r|^> *(.*)|]                   -- [(content)]

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
  , elems' = [Para { isOpen = True, elems = parseInline $ last result }]
  }
olistParser   result = Olist
  { isOpen = True
  , start  = read $ head result
  , elems' = [Para { isOpen = True, elems = parseInline $ last result }]
  }
quoteParser   result = Quote
  { isOpen = True
  , elems' = [Para { isOpen = True, elems = parseInline $ head result }]
  }

-- | Parse inline
parseInline :: String -> [InlineElem]
parseInline "" = []
parseInline s = result
  where Left result = parseCode s      -- Should be the first
                  >>= parseEmoji
                  >>= parseDel
                  >>= parseEmphStrong  -- Should before strong and emph
                  >>= parseStrong      -- Should before emph
                  >>= parseEmph
                  >>= parseLink
                  >>= parseAutoLink
                  >>= parsePlain

parseCode, parseDel, parseStrong, parseEmph, parseEmphStrong, parseLink, parseAutoLink, parseEmoji, parsePlain
  :: String -> Either [InlineElem] String
parseCode       = generateParser codePattern       codeParser
parseDel        = generateParser delPattern        delParser
parseStrong     = generateParser strongPattern     strongParser
parseEmph       = generateParser emphPattern       emphParser
parseEmphStrong = generateParser emphStrongPattern emphStrongParser
parseLink       = generateParser linkPattern       linkParser
parseAutoLink   = generateParser autoLinkPattern   autoLinkParser
parseEmoji      = generateParser emojiPattern      emojiParser
parsePlain s    = Left [Plain s]

codeParser, delParser, strongParser, emphParser, emphStrongParser, linkParser, autoLinkParser, emojiParser
  :: [String] -> [InlineElem]
codeParser       result = parseInlineAux result $ Code       { content = result !! 1 }
delParser        result = parseInlineAux result $ Del        { content = result !! 1 }
strongParser     result = parseInlineAux result $ Strong     { content = result !! 2 ++ result !! 3 }
emphParser       result = parseInlineAux result $ Emph       { content = result !! 2 ++ result !! 3 }
emphStrongParser result = parseInlineAux result $ EmphStrong { content = result !! 2 ++ result !! 3 }
linkParser       result = parseInlineAux result $ Link       { content = result !! 1, url = result !! 2 }
autoLinkParser   result = parseInlineAux result $ Link       { content = result !! 1, url = result !! 1 }
emojiParser      result = parseInlineAux result $ Plain      { content = emoji }
  where name  = result !! 1
        emoji = case Map.lookup name emojiMap of
          Just x  -> x
          Nothing -> ":" ++ name ++ ":"

parseInlineAux :: [String] -> InlineElem -> [InlineElem]
parseInlineAux result e = (parseInline $ head result) ++ [e] ++ (parseInline $ last result)

strongPattern, emphPattern, emphStrongPattern, delPattern, codePattern, linkPattern, autoLinkPattern, emojiPattern
  :: Regex.Regex
strongPattern     = Regex.mkRegex [r|(.*)(\*\*(.+)\*\*|__(.+)__)(.*)|]        -- **...**   | __...__
emphPattern       = Regex.mkRegex [r|(.*)(\*(.+)\*|_(.+)_)(.*)|]              -- *...*     | _..._
emphStrongPattern = Regex.mkRegex [r|(.*)(\*\*\*(.+)\*\*\*|___(.+)___)(.*)|]  -- ***...*** | ___...___
delPattern        = Regex.mkRegex [r|(.*)~~(.+)~~(.*)|]                       -- ~~...~~
codePattern       = Regex.mkRegex [r|(.*)`(.+)`(.*)|]                         -- `...`
linkPattern       = Regex.mkRegex [r|(.*)\[(.*)\]\((.+)\)(.*)|]               -- [...](...)
autoLinkPattern   = Regex.mkRegex [r|(.*)<(.+)>(.*)|]                         -- <...>
emojiPattern      = Regex.mkRegex [r|(.*):(\w+?|\+1|-1):(.*)|]                -- :...:

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
    -- , ["- XXX_1"]
    -- , ["- XXX_2", "  - YYY"]
    -- , ["- XXX_3", "  yyy"] --
    -- , ["- XXX_4", "  - YYY", "    ZZZ", "  - ww"]
    -- , ["- XXX_5", "  yyy", "    - ZZZ"]
    -- , ["- a", "- b", "- c"]
    -- , ["- First 1", "- First 2", "- First 3", "", "- Second"]
    -- , ["- a", "b", "- c"]
    -- , ["- a", " b'", "- c"]
    -- , ["- a", "  ```python"]
    -- , ["- a", "  ```python", "  def", "  1+1", "  ```"]
    -- , ["- a", "  ```python", "  def", "  1+1", "  ```", "- next item"]
    -- , ["- a", "  ```python", "  def", "  1+1", "  ```", "  next text"]
    -- , ["- A", "  - B"]
    -- , ["- a", "  - b", "  - c"] --
    -- , ["- A1", "  BBBB"] --
    -- , ["- A2", "  CCCC", "- X"] --
    -- , ["- A", "  - B", "  - C", "D"] --
    -- , ["- 1", "  - 2", "111"] --
    -- , ["- 1", "  - 2", "111", "- x"] --
    -- , ["- 1", "  - 2", "  222"] --
    -- , ["- 1", "  - 2", "    333"] --
    -- , ["- 1", "  - 2", "    - 444"] --
    -- , ["- 1", "    - 2", "    - 555"] --
    -- , ["- a", "  - b", "    ccc"]
    -- , ["- 1", "  1. x", "  1. y"]  -- FIXME: Exception: Prelude.undefined
    -- , ["*emph*"]
    -- , ["_emph_"]
    -- , ["``not `common`mark`` sadly"]
    -- , ["[Fudan University](https://www.fudan.edu.cn/)"]  -- FIXME:/
    -- , ["[Fudan University](www.fudan.edu.cn)"]
    -- , ["[Fudan University](<https://www.fudan.edu.cn/>)"]
    , ["***ss*** and *s1* and __s2__"]
    , ["~~del~~"]
    , [":bowtie: and :+1:"]
    , [":+1:, :-1:, are you OK:"]
    ]

test_detectDepth :: IO ()
test_detectDepth = do
  let item  = Para True (parseInline "a")
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
  let item  = Para True (parseInline "a")
      item' = [item]
      -- _data = [ Ulist { isOpen = True, elems' = item' }
      --         , Ulist { isOpen = True, elems' = [ item, item ] }
      --         , Ulist { isOpen = True, elems' = [ Ulist { isOpen = True,  elems' = item' } ] }
      --         , Ulist { isOpen = True, elems' = [ Ulist { isOpen = False, elems' = item' }, item ] }
      --         ]
  -- pPrint _data
  pPrint $ (\xs -> appendAt xs 1 $ Para True (parseInline "Added!")) item'
  pPrint $ (\xs -> appendAt xs 1 $ Para True (parseInline "Added!")) [ item, item ]
  pPrint $ (\xs -> appendAt xs 1 $ Para True (parseInline "Added!")) [ Ulist { isOpen = True,  elems' = item' } ]
  pPrint $ (\xs -> appendAt xs 2 $ Para True (parseInline "Added!")) [ Ulist { isOpen = True,  elems' = item' } ]

#endif
