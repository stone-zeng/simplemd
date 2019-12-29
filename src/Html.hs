{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Html (markdownToHtml) where

#ifdef DEBUG
import qualified "regex-compat-tdfa" Text.Regex as Regex
import Text.Pretty.Simple (pShowNoColor)
#endif

import Parse

type HTML = String

markdownToHtml :: String -> HTML
#ifdef DEBUG
markdownToHtml = addTag "pre" . addTag "code" . postParse . parse . lines
  where postParse  = init . tail . unlines . splitLine' . santize . show . pShowNoColor
        splitLine' = Regex.splitRegex (Regex.mkRegex "\\\\n")
        santize x  = Regex.subRegex (Regex.mkRegex "\\\\\"") x "\""
#else
markdownToHtml md = concatMap blockToHtml md'
  where Markdown md' = parse $ lines md
#endif

blockToHtml :: BlockElem -> HTML
blockToHtml Para    {..} = addTag "p" $ inlineToHtml elems
blockToHtml Heading {..} = addTag ("h" ++ show level) $ inlineToHtml elems
blockToHtml Hrule        = "<hr />"
blockToHtml Pre     {..} = addTag "pre" $ addTag' "code" attr text
  where attr = "class='lang-" ++ lang ++ "'"
blockToHtml Ulist   {..} = addTag "ul" $ concatMap listItemToHtml elems'
blockToHtml Olist   {..} = addTag' "ol" attr $ concatMap listItemToHtml elems'
  where attr = "start='" ++ show start ++ "'"
blockToHtml Quote   {..} = addTag "blockquote" $ concatMap blockToHtml elems'
-- blockToHtml Block   {..} = inlineToHtml elems

-- | Add HTML tag: `something` -> `<tag>something</tag>`.
addTag :: String -> String -> HTML
addTag tag s = begin ++ s ++ end
  where
    begin = "<"  ++ tag ++ ">"
    end   = "</" ++ tag ++ ">"

-- | Add HTML tag with attributes: `something` -> `<tag attr>something</tag>`.
addTag' :: String -> String -> String -> HTML
addTag' tag attr s = begin ++ s ++ end
  where
    begin = "<"  ++ tag ++ " " ++ attr ++ ">"
    end   = "</" ++ tag ++ ">"

inlineToHtml :: [InlineElem] -> HTML
inlineToHtml [] = ""
inlineToHtml (x:xs) = (inlineElemToHtml x) ++ inlineToHtml xs

inlineElemToHtml :: InlineElem -> HTML
inlineElemToHtml Plain      {..} = "" ++ content ++ ""
inlineElemToHtml Code       {..} = addTag "code"   content
inlineElemToHtml Del        {..} = addTag "del"    content
inlineElemToHtml Emph       {..} = addTag "em"     content
inlineElemToHtml Strong     {..} = addTag "strong" content
inlineElemToHtml EmphStrong {..} = addTag "em" $ addTag "strong" content
inlineElemToHtml Link       {..} = addTag' attr "a" content
  where attr = "href=" ++ url

listItemToHtml :: BlockElem -> HTML
listItemToHtml x = case x of
  Para {..} -> addTag "li" $ blockToHtml x
  _         -> blockToHtml x
