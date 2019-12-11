module Main where

-- import Data.List
import qualified Data.Text as Text
import Text.Regex

main :: IO ()
main = parseHeaders_test

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
