module Parse (
    InlineElem (..)
  , BlockElem  (..)
  , ListItem   (..)
  , ListElem   (..)
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
      ["```c"]
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
data ElemType = Plain
              | Code      -- HTML <code>
              | Emph      -- HTML <em>
              | Strong    -- HTML <strong>
              | Link Url  -- HTML <a href="...">
              | Math      -- Use MathJax/KaTeX
  deriving (Show)
type Url = String

data InlineElem = InlineElem {
    elemType    :: ElemType
  , elemContent :: String
  } deriving (Show)

data BlockState = Open | Closed deriving (Show)

data BlockElem = Para    BlockState        [InlineElem]   -- HTML <p>
               | Heading            Int    [InlineElem]   -- HTML <h1>, <h2>, ... <h6>
               | Hrule                                    -- HTML <hr />
               | Pre     BlockState String String         -- HTML <pre>
               | Ulist   BlockState        [ListItem]     -- HTML <ul>
               | Olist   BlockState Int    [ListItem]     -- HTML <ol>
               | Quote   BlockState        [BlockElem]    -- HTML <blockquote>
  deriving (Show)

-- HTML <li>
newtype ListItem = ListItem [ListElem]
  deriving (Show)
data ListElem = ParaInList  BlockState     [InlineElem]
              | UlistInList BlockState     [ListItem]
              | OlistInList BlockState Int [ListItem]
  deriving (Show)

newtype Markdown = Markdown [BlockElem] deriving (Show)

-- (<+>) :: Markdown -> Markdown -> Markdown
-- Markdown list1 <+> Markdown list2 = Markdown (list1 ++ list2)

(.+) :: [a] -> a -> [a]
xs .+ x = xs ++ [x]

(<:>) :: [BlockElem] -> Markdown -> Markdown
list1 <:> Markdown list2 = Markdown $ list1 ++ list2

-- (<++>) :: [BlockElem] -> [BlockElem] -> Markdown
-- list1 <++> list2 = Markdown (list1 ++ list2)

mdAppend :: [BlockElem] -> BlockElem -> Markdown
mdAppend list x = Markdown $ list .+ x

headingPattern
  , hrulePattern
  , prePattern
  , ulistPattern
  , olistPattern
  , quotePattern
  :: Regex.Regex
headingPattern = Regex.mkRegex "^(#{1,6}) (.*)"               -- [(#), (content)]
hrulePattern   = Regex.mkRegex "^(\\-{3,}|\\*{3,}|_{3,}) *$"  -- [(---|***|___)]
prePattern     = Regex.mkRegex "^```(.*)"                     -- [(preLang)]
ulistPattern   = Regex.mkRegex "^(\\-|\\*|\\+) (.*)"          -- [(-|*|+), (content)]
olistPattern   = Regex.mkRegex "^([0-9]+)\\. (.*)"            -- [(number), (content)]
quotePattern   = Regex.mkRegex "^> *(.*)"                     -- [(content)]

parseHeading'
  , parseHrule'
  , parsePre'
  , parseUlist'
  , parseOlist'
  , parseQuote'
   :: [String] -> BlockElem
parseHeading' result = Heading headingLevel headingText
  where headingLevel = length $ head result
        headingText  = parseInline $ last result
parseHrule' _        = Hrule
parsePre' result     = Pre Open preLang preText
  where preLang      = head result
        preText      = ""
parseUlist' result   = Ulist Open [ListItem listItems]
  where listItems    = [ParaInList Open $ parseInline $ last result]
parseOlist' result   = Olist Open start [ListItem listItems]
  where start        = read $ head result
        listItems    = [ParaInList Open $ parseInline $ last result]
parseQuote' result   = Quote Open quote
  where quote        = [Para Open (parseInline $ head result)]

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
parsePara s  = Left $ Markdown [Para Open $ parseInline s]

nextPre :: String -> String -> String -> [BlockElem]
nextPre preLang preText s = case s of
  "```" -> [Pre Closed preLang preText]
  _     -> case preText of
    "" -> [Pre Open preLang s]
    _  -> [Pre Open preLang $ preText ++ "\n" ++ s]

nextPara :: [InlineElem] -> String -> [BlockElem]
nextPara para s = case s of
  "" -> [Para Closed para]
  _  -> let result = parseHeading s
                 >>= parseHrule
                 >>= parsePre
                 >>= parseUlist
                 >>= parseOlist
                 >>= parseQuote in
    case result of
      Left (Markdown x) -> [Para Open para] ++ x
      Right s'          -> [Para Open $ para ++ parseInline ("\n" ++ s')]

nextUlist :: [ListItem] -> String -> [BlockElem]
nextUlist items s = case s of
  "" -> [Ulist Closed items]
  _  -> case detectIndent s of
    (0, _) -> let result = parseHeading s
                       >>= parseHrule
                       >>= parsePre
                       >>= parseOlist
                       >>= parseQuote in
      case result of
        Left (Markdown x) -> [Ulist Closed items] ++ x
        Right s'          -> case Regex.matchRegex ulistPattern s' of
          Just x  -> [Ulist Closed $ items .+ ListItem [ParaInList Open $ parseInline $ last x]]
          Nothing -> [Ulist Closed $ init items .+ ListItem lastItem']
            where ListItem lastItem = last items
                  lastItem' = lastItem .+ (ParaInList Open $ parseInline $ "\n" ++ s')
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
    Pre Open preLang preText -> Markdown $ init mdElements ++ nextPre preLang preText s
    Para Open para           -> Markdown $ init mdElements ++ nextPara para s
    Ulist Open listItems     -> Markdown $ init mdElements ++ nextUlist listItems s
      -- case s of
      --   "" -> init mdElements `mdAppend` Ulist Closed listItems
      --   _  ->
      --     -- TODO: indent
      --     case Regex.matchRegex ulistPattern s of
      --       Just result -> init mdElements `mdAppend` Ulist Open (listItems ++ [newListItem])
      --         where newListItem = ListInlineItem (parseInline $ last result)
      --       Nothing -> mdElements <:> parseMarkdown (Markdown []) s
    Olist Open olStart listItems ->
      case s of
        "" -> Markdown $ init mdElements ++ [Olist Closed olStart listItems]
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
