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

listItemToHtml :: ListItem -> HTML
listItemToHtml (ListInlineItem xs) = addTag "li" $ inlineListToHtml xs
listItemToHtml (ListBlockItem  xs) = addTag "li" (concatMap listBlockToHtml xs)
  where listBlockToHtml (ListBlockPara  ys) = inlineListToHtml ys
        listBlockToHtml (ListBlockUlist ys) = concatMap listItemToHtml ys
        listBlockToHtml (ListBlockOlist ys) = concatMap listItemToHtml ys

blockToHtml :: BlockElem -> HTML
blockToHtml (Para    _       content) = addTag "p" $ inlineListToHtml content
blockToHtml (Heading   level content) = addTag tag $ inlineListToHtml content
  where tag = "h" ++ show level
blockToHtml  Hrule                    = "<hr />"
blockToHtml (Pre     _ lang  content) = addTag "pre" $ addTag' "code" attr content
  where attr = "class='lang-" ++ lang ++ "'"
blockToHtml (Ulist   _       content) = addTag "ul" (concatMap listItemToHtml content)
blockToHtml (Olist   _ start content) = addTag' "ol" attr (concatMap listItemToHtml content)
  where attr = "start='" ++ show start ++ "'"
blockToHtml (Quote   _       content) = addTag "blockquote" (concatMap blockToHtml content)

markdownToHtml :: HTML -> HTML
markdownToHtml = addTag "pre" . addTag "code" . postParse . parse . splitLine
  where postParse  = init . tail . unlines . splitLine' . santize . show . pShowNoColor
        splitLine' = Regex.splitRegex (Regex.mkRegex "\\\\n")
        santize x  = Regex.subRegex (Regex.mkRegex "\\\\\"") x "\""
-- markdownToHtml = astToHtml . parse . splitLine
