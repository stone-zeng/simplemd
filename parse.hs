module Main where

-- import Data.List
-- import qualified Data.Text as Text
import Text.Regex

import Text.Pretty.Simple (pPrint)

main :: IO ()
main = let f = foldl parseMarkdown $ Markdown []
  in pPrint $ map f [
      [
          "```c"
        , "int main () {"
        , "    return 0;"
        , "}"
        , "```"
      ]
    -- , [
    --       "```python"
    --     , "def f:"
    --     , "\tpass"
    --     , "```endpython"
    --     , "```"
    --   ]
    -- , [
    --       "```python"
    --     , "def f:"
    --     , "\tpass"
    --     , "```"
    --     , "something after"
    --     , "more thing after"
    --   ]
    -- , [
    --       "# Title"
    --     , "```python"
    --     , "# Comment"
    --     , "def f:"
    --     , "\tpass"
    --     , "```"
    --     , "something after"
    --     , "## title2"
    --   ]
    -- , ["Word1", "Word2", "Word3"]
    -- , ["text", "text", "- list1", "- list2", "  - indent", "- item next", "text3"]
    -- , [
    --     "Text"
    --   , "- ul1"
    --   , "- ul2"
    --   , "- ul3"
    --   , "1. ol1"
    --   , "2. ol2"
    --   , "1. ol3"
    --   , "- ul another"
    --   , "3. ol another"
    --   ]
    , ["> quote"]
    , [">q"]
    , [">> dq"]
    , [">>dq"]
    , [">"]
    , [">>>"]
    -- , [
    --     "> Quote"
    --   , "> Quote cont."
    --   , "> - List in quote 1"
    --   , "> - List in quote 2"
    --   , "1. List"
    --   , "1. List2"
    --   ]
    -- , [
    --       "## Title"
    --     , "# Title2"
    --     , "```c"
    --     , "- help"
    --     , "1. First"
    --     , "22. Twenty-two"
    --     , "1.invalie ol"
    --     , "-invalid ul"
    --     , "> quote"
    --     , ">> invalid quote"
    --     , ">2invalid quote"
    --     , "``invalid code"
    --   ]
  ]

--------------------------------------------------------------------------------

-- TODO: support nested elements
data ElemType = Plain
              | Code      -- ^ HTML <code>
              | Emph      -- ^ HTML <em>
              | Strong    -- ^ HTML <strong>
              | Link Url  -- ^ HTML <a href="...">
              | Math      -- ^ Use MathJax/KaTeX
  deriving (Show)
type Url = String

data InlineElem = InlineElem {
    elemType    :: ElemType
  , elemContent :: String
  } deriving (Show)

data BlockState = Open | Closed deriving (Show)

data BlockElem = Para     BlockState          [InlineElem]   -- ^ HTML <p>
               | Heading  BlockState  Int     [InlineElem]   -- ^ HTML <h1>, <h2>, ... <h6>
               | Pre      BlockState  String  String         -- ^ HTML <pre>
               | Ulist    BlockState          [ListItem]     -- ^ HTML <ul>
               | Olist    BlockState  Int     [ListItem]     -- ^ HTML <ol>
               | Quote    BlockState          [BlockElem]    -- ^ HTML <blockquote>
  deriving (Show)

-- HTML <li>
data ListItem = ListInlineItem [InlineElem]
              | ListBlockItem  [ListBlockElem]
  deriving (Show)
data ListBlockElem = ListBlockPara  [InlineElem]
                   | ListBlockUlist [ListItem]
                   | ListBlockOlist [ListItem]
  deriving (Show)

newtype Markdown = Markdown [BlockElem] deriving (Show)

(<+>) :: Markdown -> Markdown -> Markdown
Markdown list1 <+> Markdown list2 = Markdown (list1 ++ list2)

(<:>) :: [BlockElem] -> Markdown -> Markdown
list1 <:> Markdown list2 = Markdown (list1 ++ list2)

(<++>) :: [BlockElem] -> [BlockElem] -> Markdown
list1 <++> list2 = Markdown (list1 ++ list2)

mdAppend :: [BlockElem] -> BlockElem -> Markdown
mdAppend list e = Markdown (list ++ [e])

--------------------------------------------------------------------------------

headingPattern, prePattern, ulistPattern, olistPattern, quotePattern :: Regex
headingPattern = mkRegex "^(#{1,6}) (.*)"       -- [<#>, <content>]
prePattern     = mkRegex "^```(.*)"             -- [<pre lang>]
ulistPattern   = mkRegex "^\\- (.*)"            -- [<content>]
olistPattern   = mkRegex "^([0-9]+)\\. (.*)"    -- [<number>, <content>]
quotePattern   = mkRegex "^> *(.*)"             -- [<content>]

parseHeading, parsePre, parseUlist, parseOlist, parseQuote :: [String] -> BlockElem
parseHeading result   = Heading Closed headingLevel headingText
  where headingLevel  = length $ head result
        headingText   = parseInline $ last result
parsePre result       = Pre Open preLang preText
  where preLang       = head result
        preText       = ""
parseUlist result     = Ulist Open listItems
  where listItems     = [ListInlineItem $ parseInline $ last result]
parseOlist result     = Olist Open olStart listItems
  where olStart       = read $ head result
        listItems     = [ListInlineItem $ parseInline $ last result]
parseQuote result     = Quote Open quote
  where quote         = [Para Open (parseInline $ head result)]

parseInline :: String -> [InlineElem]
parseInline s = [InlineElem {elemType = Plain, elemContent = s}]

parseMarkdown :: Markdown -> String -> Markdown
parseMarkdown (Markdown []) s = Markdown [
    case matchRegex headingPattern s of
      Just result -> parseHeading result
      Nothing ->
        case matchRegex prePattern s of
          Just result -> parsePre result
          Nothing ->
            case matchRegex ulistPattern s of
              Just result -> parseUlist result
              Nothing ->
                case matchRegex olistPattern s of
                  Just result -> parseOlist result
                  Nothing ->
                    case matchRegex quotePattern s of
                      Just result -> parseQuote result
                      Nothing -> Para Open (parseInline s)
  ]

parseMarkdown (Markdown mdElements) s =
  case last mdElements of
    Pre Open preLang preText ->
      case s of
        "```" -> init mdElements `mdAppend` Pre Closed preLang preText
        _     -> init mdElements `mdAppend` Pre Open preLang (preText ++ "\n" ++ s)
    Para Open para ->
      case s of
        "" -> init mdElements `mdAppend` Para Closed para
        _  ->
          case matchRegex headingPattern s of
            Just result -> mdElements `mdAppend` parseHeading result
            Nothing ->
              case matchRegex prePattern s of
                Just result -> mdElements `mdAppend` parsePre result
                Nothing ->
                  case matchRegex ulistPattern s of
                    Just result -> mdElements `mdAppend` parseUlist result
                    Nothing ->
                      case matchRegex olistPattern s of
                        Just result -> mdElements `mdAppend` parseOlist result
                        Nothing ->
                          case matchRegex quotePattern s of
                            Just result -> Markdown [parseQuote result]
                            Nothing ->
                              init mdElements `mdAppend` Para Open (para ++ parseInline s)
    Ulist Open listItems ->
      case s of
        "" -> init mdElements `mdAppend` Ulist Closed listItems
        _  ->
          -- TODO: indent
          case matchRegex ulistPattern s of
            Just result -> init mdElements `mdAppend` Ulist Open (listItems ++ [newListItem])
              where newListItem = ListInlineItem (parseInline $ last result)
            Nothing -> mdElements <:> parseMarkdown (Markdown []) s
    Olist Open olStart listItems ->
      case s of
        "" -> Markdown $ init mdElements ++ [Olist Closed olStart listItems]
        _  ->
          -- TODO: indent
          case matchRegex olistPattern s of
            Just result -> init mdElements `mdAppend` Olist Open olStart (listItems ++ [newListItem])
              where newListItem = ListInlineItem (parseInline $ last result)
            Nothing -> mdElements <:> parseMarkdown (Markdown []) s
    -- BlockQuote quote Open ->
    --   case s of
    --     "" -> Markdown $ init mdElements ++ [BlockQuote quote Closed]
    --     _ -> undefined
    _ ->
      mdElements <:> parseMarkdown (Markdown []) s
