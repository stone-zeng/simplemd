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
    , [
          "```python"
        , "def f:"
        , "\tpass"
        , "```endpython"
        , "```"
      ]
    , [
          "```python"
        , "def f:"
        , "\tpass"
        , "```"
        , "something after"
      ]
    , [
          "# Title"
        , "```python"
        , "# Comment"
        , "def f:"
        , "\tpass"
        , "```"
        , "something after"
        , "## title2"
      ]
    , [
          "## Title"
        , "# Title2"
        , "```c"
        , "- help"
        , "1. First"
        , "22. Twenty-two"
        , "1.invalie ol"
        , "-invalid ul"
        , "> quote"
        , ">> invalid quote"
        , ">2invalid quote"
        , "``invalid code"
      ]
  ]

--------------------------------------------------------------------------------


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

data BlockElem = BlockPara    Para    BlockState -- ^ HTML <p>
               | BlockHeading Heading BlockState -- ^ HTML <h1>, <h2>, ... <h6>
               | BlockPre     Pre     BlockState -- ^ HTML <pre>
               | BlockUlist   Ulist   BlockState -- ^ HTML <ul>
               | BlockOlist   Olist   BlockState -- ^ HTML <ol>
               | BlockQuote   Quote   BlockState -- ^ HTML <blockquote>
  deriving (Show)
data BlockState = Open | Closed deriving (Show)

-- HTML <p>
newtype Para = Para [InlineElem] deriving (Show)

-- HTML <h1>, <h2>, ... <h6>
data Heading = Heading {
    headingLevel   :: Int
  , headingContent :: [InlineElem]
  } deriving (Show)

-- HTML <pre>
data Pre = Pre {
    preLang    :: String
  , preContent :: String
  } deriving (Show)

-- HTML <ul>
newtype Ulist = Ulist [ListItem] deriving (Show)

-- HTML <ol>
data Olist = Olist {
    olStart   :: Int
  , olContent :: [ListItem]
  } deriving (Show)

-- HTML <li>
data ListItem = ListInlineItem [InlineElem]
              | ListBlockItem  [ListBlockElem]
  deriving (Show)
data ListBlockElem = ListBlockPara  Para
                   | ListBlockUlist Ulist
                   | ListBlockOlist Olist
  deriving (Show)

-- HTML <blockquote>
newtype Quote = Quote [BlockElem] deriving (Show)

newtype Markdown = Markdown [BlockElem] deriving (Show)

--------------------------------------------------------------------------------

headingPattern, prePattern, ulistPattern, olistPattern, quotePattern :: Regex
headingPattern = mkRegex "^(#{1,6}) (.*)"       -- [<#>, <content>]
prePattern     = mkRegex "^```(.*)"             -- [<pre lang>]
ulistPattern   = mkRegex "^\\- (.*)"            -- [<content>]
olistPattern   = mkRegex "^([0-9]+)\\. (.*)"    -- [<number>, <content>]
quotePattern   = mkRegex "^> (.*)"              -- [<content>]

parseHeading, parsePre, parseUlist, parseOlist, parseQuote :: [String] -> Markdown
parseHeading result = Markdown [BlockHeading heading Closed]
  where heading = Heading {
    headingLevel   = length $ head result
  , headingContent = parseInline $ last result
  }
parsePre result = Markdown [BlockPre pre Open]
  where pre = Pre {
    preLang    = head result
  , preContent = ""
  }
parseUlist result = Markdown [BlockUlist ul Open]
  where ul = Ulist [ListInlineItem $ parseInline $ last result]
parseOlist result = Markdown [BlockOlist ol Open]
  where ol = Olist {
    olStart   = read $ head result
  , olContent = [ListInlineItem $ parseInline $ last result]
  }
parseQuote result = Markdown [BlockQuote quote Open]
  where quote = Quote [BlockPara (Para $ parseInline $ head result) Open]

parseInline :: String -> [InlineElem]
parseInline s = [InlineElem {elemType = Plain, elemContent = s}]

parseMarkdown :: Markdown -> String -> Markdown
parseMarkdown (Markdown []) s =
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
                    Nothing -> Markdown [BlockPara (Para $ parseInline s) Open]

parseMarkdown (Markdown mdElements) s =
  case last mdElements of
    BlockPre pre Open ->
      case s of
        "```" -> Markdown $ init mdElements ++ [BlockPre pre Closed]
        _     -> Markdown $ init mdElements ++ [BlockPre newPre Open]
        where newPre = Pre {
          preLang    = preLang pre
        , preContent = preContent pre ++ "\n" ++ s
        }
    BlockPara para Open ->
      case s of
        "" -> Markdown $ init mdElements ++ [BlockPara para Closed]
        _  ->
          Markdown $ mdElements ++ newMdElems
            where Markdown newMdElems = parseMarkdown (Markdown []) s
    _ ->
      -- BlockPre _ Closed
      -- BlockHeading _ Closed
      Markdown $ mdElements ++ newMdElems
        where Markdown newMdElems = parseMarkdown (Markdown []) s
