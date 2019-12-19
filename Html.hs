module Html (
    splitLine
  , markdownToHtml
  ) where

import Text.Regex

import Parse

splitLine :: String -> [String]
splitLine s = removeEmptyLine $ map replaceBr $ splitRegex pattern s
  where
    pattern = mkRegex "</div><div>|<div>|</div>"

    replaceBr "<br>" = ""
    replaceBr x = x

    removeEmptyLine [] = []
    removeEmptyLine ("":xs) = removeEmptyLine xs
    removeEmptyLine x = if (last x == "") then removeEmptyLine (init x) else x

type HTML = String

astToHtml :: Markdown -> HTML
astToHtml (Markdown xs) = concat $ map blockToHtml xs

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
inlineListToHtml xs = concat $ map inlineToHtml xs

listItemToHtml :: ListItem -> HTML
listItemToHtml (ListInlineItem xs) = addTag "li" $ inlineListToHtml xs
listItemToHtml (ListBlockItem  xs) = addTag "li" (concat $ map listBlockToHtml xs)
  where listBlockToHtml (ListBlockPara  ys) = inlineListToHtml ys
        listBlockToHtml (ListBlockUlist ys) = concat $ map listItemToHtml ys
        listBlockToHtml (ListBlockOlist ys) = concat $ map listItemToHtml ys

blockToHtml :: BlockElem -> HTML
blockToHtml (Para    _       content) = addTag "p" $ inlineListToHtml content
blockToHtml (Heading _ level content) = addTag tag $ inlineListToHtml content
  where tag = "h" ++ show level
blockToHtml (Pre     _ lang  content) = addTag' "pre" attr content
  where attr = "class='lang-" ++ lang ++ "'"
blockToHtml (Ulist   _       content) = addTag "ul" (concat $ map listItemToHtml content)
blockToHtml (Olist   _ start content) = addTag' "ol" attr (concat $ map listItemToHtml content)
  where attr = "start='" ++ show start ++ "'"
blockToHtml (Quote   _       content) = addTag "blockquote" (concat $ map blockToHtml content)

markdownToHtml :: HTML -> HTML
markdownToHtml = astToHtml . parse . splitLine
