{-# LANGUAGE RecordWildCards #-}

module Parse (
    InlineElem (..)
  , BlockElem  (..)
  , Markdown   (..)
  , parse
  ) where

import qualified Text.Regex as Regex
import Text.Pretty.Simple (pPrint)

parse :: [String] -> Markdown
parse = foldl parseMarkdown $ Markdown []

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

-- TODO: support nested elements
data ElemType =
    Plain
  | Code                    -- ^ HTML <code>
  | Emph                    -- ^ HTML <em>
  | Strong                  -- ^ HTML <strong>
  | Link { url :: String }  -- ^ HTML <a href="...">
  | Math                    -- ^ Use MathJax/KaTeX
  deriving (Show)

data InlineElem = InlineElem { elemType :: ElemType, elemContent :: String }
  deriving (Show)

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

newtype Markdown = Markdown [BlockElem] deriving (Show)

(.+) :: [a] -> a -> [a]
xs .+ x = xs ++ [x]

(<:>) :: [BlockElem] -> Markdown -> Markdown
list1 <:> Markdown list2 = Markdown $ list1 ++ list2

-- (<+>) :: Markdown -> Markdown -> Markdown
-- Markdown list1 <+> Markdown list2 = Markdown (list1 ++ list2)

-- (<++>) :: [BlockElem] -> [BlockElem] -> Markdown
-- list1 <++> list2 = Markdown (list1 ++ list2)

-- mdAppend :: [BlockElem] -> BlockElem -> Markdown
-- mdAppend list x = Markdown $ list .+ x

hrulePattern
  , headingPattern
  , prePattern
  , ulistPattern
  , olistPattern
  , quotePattern
  :: Regex.Regex
hrulePattern   = Regex.mkRegex "^(\\-{3,}|\\*{3,}|_{3,}) *$"  -- [(---|***|___)]
headingPattern = Regex.mkRegex "^(#{1,6}) (.*)"               -- [(#), (content)]
prePattern     = Regex.mkRegex "^```(.*)"                     -- [(preLang)]
ulistPattern   = Regex.mkRegex "^(\\-|\\*|\\+) (.*)"          -- [(-|*|+), (content)]
olistPattern   = Regex.mkRegex "^([0-9]+)\\. (.*)"            -- [(number), (content)]
quotePattern   = Regex.mkRegex "^> *(.*)"                     -- [(content)]

-- | Parse a matched string list into `BlockElem`.
parseHrule'
  , parseHeading'
  , parsePre'
  , parseUlist'
  , parseOlist'
  , parseQuote'
   :: [String] -> BlockElem
parseHeading' result = Heading {
    level = length $ head result
  , elems = parseInline $ last result }
parseHrule'   _      = Hrule
parsePre'     result = Pre {
    isOpen = True
  , lang = head result
  , text = "" }
parseUlist'   result = Ulist {
    isOpen = True
  , elems' = [Block { elems = parseInline $ last result }] }
parseOlist'   result = Olist {
    isOpen = True
  , start  = read $ head result
  , elems' = [Block { elems = parseInline $ last result }] }
parseQuote'   result = Quote {
    isOpen = True
  , elems' = [Para { isOpen = True, elems = parseInline $ head result }] }

parseInline :: String -> [InlineElem]
parseInline s = [InlineElem {elemType = Plain, elemContent = s}]  -- TODO: placeholder

type MarkdownWrapper = Either Markdown String

generateParser :: Regex.Regex -> ([String] -> BlockElem) -> (String -> MarkdownWrapper)
generateParser pattern parser = \s -> case Regex.matchRegex pattern s of
  Just x  -> Left $ Markdown [parser x]
  Nothing -> Right s

parseHeading
  , parseHrule
  , parsePre
  , parseUlist
  , parseOlist
  , parseQuote
  , parsePara
  :: String -> MarkdownWrapper
parseHeading = generateParser headingPattern parseHeading'
parseHrule   = generateParser hrulePattern   parseHrule'
parsePre     = generateParser prePattern     parsePre'
parseUlist   = generateParser ulistPattern   parseUlist'
parseOlist   = generateParser olistPattern   parseOlist'
parseQuote   = generateParser quotePattern   parseQuote'
parsePara s  = Left $ Markdown [Para { isOpen = True, elems = parseInline s }]

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
          Just x  -> [Ulist {
              isOpen = True
            , elems' = xs .+ Block { elems = parseInline $ last x } }]
          Nothing -> [Ulist {
              isOpen = True
            , elems' = init xs .+ Block { elems = lastItem } }]
            where lastItem = (elems $ last xs) ++ (parseInline $ "\n" ++ s)
    -- TODO: indent
    _ -> undefined


-- | Return the number of leading spaces and the sub-string with leading spaces removed.
detectIndent :: String -> (Int, String)
detectIndent "" = (0, "")
detectIndent s  = case head s of
  ' ' -> (n + 1, s')
    where (n, s') = detectIndent $ tail s
  _  -> (0, s)


-- appendAtLast :: [[a]] -> a -> [[a]]
-- appendAtLast []     _ = []
-- appendAtLast [xs]   x = [init xs ++ [last xs, x]]

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
    Pre   { isOpen = True, .. } -> Markdown $ init mdElements ++ nextPre lang text s
    Para  { isOpen = True, .. } -> Markdown $ init mdElements ++ nextPara elems s
    Ulist { isOpen = True, .. } -> Markdown $ init mdElements ++ nextUlist elems' s
    Olist { isOpen = True, .. } ->
      case s of
        "" -> Markdown $ init mdElements ++ [Olist { isOpen = False, start = start, elems' = elems' }]
        _  ->
          undefined
          -- -- TODO: indent
          -- case Regex.matchRegex olistPattern s of
          --   Just result -> init mdElements `mdAppend` Olist Open olStart (listItems ++ [newListItem])
          --     where newListItem = ListInlineItem (parseInline $ last result)
          --   Nothing -> mdElements <:> parseMarkdown (Markdown []) s
    -- BlockQuote quote Open ->
    --   case s of
    --     "" -> Markdown $ init mdElements ++ [BlockQuote quote Closed]
    --     _ -> undefined
    _ ->
      case s of
        "" -> Markdown mdElements
        _  -> mdElements <:> parseMarkdown (Markdown []) s
