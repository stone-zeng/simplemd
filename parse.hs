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


data ElemType = Plain | Code | Emph | Strong | Link String | Math
    deriving (Show)

data InlineElem = InlineElem {
        elemType    :: ElemType,
        elemContent :: String
    } deriving (Show)

newtype Para = Para [InlineElem] deriving (Show)

data Heading = Heading {
        headingLevel   :: Int,
        headingContent :: [InlineElem]
    } deriving (Show)

data Pre = Pre {
        preLang    :: String,
        preContent :: String
    } deriving (Show)

newtype Ulist = Ulist [ListItem] deriving (Show)
data Olist = Olist {
        olStart   :: Int,
        olContent :: [ListItem]
    } deriving (Show)
newtype ListItem = ListItem [InlineElem] deriving (Show)

newtype Quote = Quote [QuoteElem]
    deriving (Show)
data QuoteElem  = QuotePara    Para
                | QuoteHeading Heading
                | QuotePre     Pre
                | QuoteUlist   Ulist
                | QuoteOlist   Olist
                | QuoteQuote   Quote
    deriving (Show)

newtype Markdown = Markdown [MdElem] deriving (Show)
data MdElem = MdPara    Para
            | MdHeading Heading
            | MdPre     Pre
            | MdUlist   Ulist
            | MdOlist   Olist
            | MdQuote   Quote
    deriving (Show)

--------------------------------------------------------------------------------

headingPattern, prePattern, ulistPattern, olistPattern, quotePattern :: Regex
headingPattern = mkRegex "^(#{1,6}) (.*)"       -- [<#>, <content>]
prePattern     = mkRegex "^```(.*)"             -- [<pre lang>]
ulistPattern   = mkRegex "^\\- (.*)"            -- [<content>]
olistPattern   = mkRegex "^([0-9]+)\\. (.*)"    -- [<number>, <content>]
quotePattern   = mkRegex "^> (.*)"              -- [<content>]

parseHeading, parsePre, parseUlist, parseOlist, parseQuote :: [String] -> Markdown
parseHeading result = Markdown [MdHeading heading]
    where heading = Heading {
        headingLevel   = length $ head result,
        headingContent = parseInline $ last result
    }
parsePre result = Markdown [MdPre pre]
    where pre = Pre {
        preLang    = head result,
        preContent = ""
    }
parseUlist result = Markdown [MdUlist ul]
    where ul = Ulist [ListItem $ parseInline $ last result]
parseOlist result = Markdown [MdOlist ol]
    where ol = Olist {
        olStart   = read $ head result,
        olContent = [ListItem $ parseInline $ last result]
    }
parseQuote result = Markdown [MdQuote quote]
    where quote = Quote [QuotePara $ Para $ parseInline $ head result]

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
                                        Nothing -> Markdown [MdPara $ Para $ parseInline s]

parseMarkdown (Markdown mdElements) s =
    Markdown (mdElements ++ newMdElems)
    where Markdown newMdElems = parseMarkdown (Markdown []) s


--------------------------------------------------------------------------------

{-

parseHeaders :: String -> String
parseHeaders s = unwrap $ matchRegexAll pattern s
    where
        pattern = mkRegex "^ {0,3}(#{1,6}) +"

        unwrap Nothing = s
        unwrap (Just (_, _, _title, hashes)) = tag hx $ trimSpaces _title
            where
                hx = "h" ++ (show $ length $ head hashes)

parseHeaders_test :: IO ()
parseHeaders_test = mapM_ (print . parseHeaders) [
        -- Valid headers
        "# foo",
        "## foo",
        "### foo",
        "#### foo",
        "##### foo",
        "###### foo",
        "####### foo",
        " ### foo",
        "  ## foo",
        "   # foo",
        -- Invalid headers
        "#5 bolt",
        "#hashtag",
        "    # foo   "
    ]


trimSpaces :: String -> String
trimSpaces = Text.unpack . Text.strip . Text.pack

tag :: String -> String -> String
tag name content = "<" ++ name ++ ">" ++ content ++ "</" ++ name ++ ">"

-}
