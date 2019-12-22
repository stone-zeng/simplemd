module Html (splitLine, markdownToHtml) where

import qualified Text.Regex as Regex
import Text.Pretty.Simple (pShowNoColor)

import Parse

splitLine :: String -> [String]
splitLine = Regex.splitRegex $ Regex.mkRegex "\n"

type HTML = String

astToHtml :: Markdown -> HTML
astToHtml (Markdown xs) = concatMap blockToHtml xs

addTag :: String -> String -> HTML
addTag tag s = begin ++ s ++ end
  where
    begin = "<"  ++ tag ++ ">"
    end   = "</" ++ tag ++ ">"

addTag' :: String -> String -> String -> HTML
addTag' tag attr s = begin ++ s ++ end
  where
    begin = "<"  ++ tag ++ " " ++ attr ++ ">"
    end   = "</" ++ tag ++ ">"

inlineToHtml :: InlineElem -> HTML
inlineToHtml = elemContent  -- TODO:placeholder

inlineListToHtml :: [InlineElem] -> HTML
inlineListToHtml = concatMap inlineToHtml

ulistToHtml :: [ListItem] -> HTML
ulistToHtml x = addTag  "ul"      $ concatMap listItemToHtml x

olistToHtml :: Int -> [ListItem] -> HTML
olistToHtml start x = addTag' "ol" attr $ concatMap listItemToHtml x
  where attr = "start='" ++ show start ++ "'"

listItemToHtml :: ListItem -> HTML
listItemToHtml (ListItem xs) = addTag "li" $ concatMap listElemToHtml xs
  where listElemToHtml (ParaInList  _       x) = inlineListToHtml x
        listElemToHtml (UlistInList _       x) = ulistToHtml x
        listElemToHtml (OlistInList _ start x) = olistToHtml start x

blockToHtml :: BlockElem -> HTML
blockToHtml (Para    _       content) = addTag "p" $ inlineListToHtml content
blockToHtml (Heading   level content) = addTag tag $ inlineListToHtml content
  where tag = "h" ++ show level
blockToHtml  Hrule                    = "<hr />"
blockToHtml (Pre     _ lang  content) = addTag "pre" $ addTag' "code" attr content
  where attr = "class='lang-" ++ lang ++ "'"
blockToHtml (Ulist   _       content) = ulistToHtml content
blockToHtml (Olist   _ start content) = olistToHtml start content
blockToHtml (Quote   _       content) = addTag "blockquote" $ concatMap blockToHtml content

markdownToHtml :: HTML -> HTML
-- markdownToHtml = addTag "pre" . addTag "code" . postParse . parse . splitLine
--   where postParse  = init . tail . unlines . splitLine' . santize . show . pShowNoColor
--         splitLine' = Regex.splitRegex (Regex.mkRegex "\\\\n")
--         santize x  = Regex.subRegex (Regex.mkRegex "\\\\\"") x "\""
markdownToHtml = astToHtml . parse . splitLine
