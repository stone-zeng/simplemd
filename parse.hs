module Main where

-- import Data.List
-- import qualified Data.Text as Text
import Text.Regex

import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
    pPrint $ foldl parseString "XX" [1, 2, 33, 444]
    pPrint $ foldl parseMarkdown (Markdown []) [
            "## Title",
            "# Title2",
            "```c",
            "- help",
            "1. First",
            "22. Twenty-two",
            "1.invalie ol",
            "-invalid ul",
            "> quote",
            ">> invalid quote",
            ">2invalid quote",
            "``invalid code"
        ]

type Result = String
parseString :: Result -> Int -> Result
parseString result x = "(" ++ result ++ "+" ++ show x ++ ")"

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
        elemType    :: ElemType,
        elemContent :: String
    } deriving (Show)

data BlockElem = BlockPara    Para     -- ^ HTML <p>
               | BlockHeading Heading  -- ^ HTML <h1>, <h2>, ... <h6>
               | BlockPre     Pre      -- ^ HTML <pre>
               | BlockUlist   Ulist    -- ^ HTML <ul>
               | BlockOlist   Olist    -- ^ HTML <ol>
               | BlockQuote   Quote    -- ^ HTML <blockquote>
    deriving (Show)

-- HTML <p>
newtype Para = Para [InlineElem] deriving (Show)

-- HTML <h1>, <h2>, ... <h6>
data Heading = Heading {
        headingLevel   :: Int,
        headingContent :: [InlineElem]
    } deriving (Show)

-- HTML <pre>
data Pre = Pre {
        preLang    :: String,
        preContent :: String
    } deriving (Show)

-- HTML <ul>
newtype Ulist = Ulist [ListItem] deriving (Show)

-- HTML <ol>
data Olist = Olist {
        olStart   :: Int,
        olContent :: [ListItem]
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
parseHeading result = Markdown [BlockHeading heading]
    where heading = Heading {
        headingLevel   = length $ head result,
        headingContent = parseInline $ last result
    }
parsePre result = Markdown [BlockPre pre]
    where pre = Pre {
        preLang    = head result,
        preContent = ""
    }
parseUlist result = Markdown [BlockUlist ul]
    where ul = Ulist [ListInlineItem $ parseInline $ last result]
parseOlist result = Markdown [BlockOlist ol]
    where ol = Olist {
        olStart   = read $ head result,
        olContent = [ListInlineItem $ parseInline $ last result]
    }
parseQuote result = Markdown [BlockQuote quote]
    where quote = Quote [BlockPara $ Para $ parseInline $ head result]

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
                                        Nothing -> Markdown [BlockPara $ Para $ parseInline s]

parseMarkdown (Markdown mdElements) s =
    Markdown (mdElements ++ newMdElems)
    where Markdown newMdElems = parseMarkdown (Markdown []) s
