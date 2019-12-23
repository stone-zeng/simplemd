{-# LANGUAGE RecordWildCards #-}

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

blockToHtml :: BlockElem -> HTML
blockToHtml Para    {..} = addTag "p" $ inlineListToHtml elems
blockToHtml Heading {..} = addTag ("h" ++ show level) $ inlineListToHtml elems
blockToHtml Hrule        = "<hr />"
blockToHtml Pre     {..} = addTag "pre" $ addTag' "code" attr text
  where attr = "class='lang-" ++ lang ++ "'"
blockToHtml Ulist   {..} = addTag "ul" $ concatMap listItemToHtml elems'
blockToHtml Olist   {..} = addTag' "ol" attr $ concatMap listItemToHtml elems'
  where attr = "start='" ++ show start ++ "'"
blockToHtml Quote   {..} = addTag "blockquote" $ concatMap blockToHtml elems'
blockToHtml Block   {..} = inlineListToHtml elems

listItemToHtml :: BlockElem -> HTML
listItemToHtml = addTag "li" . blockToHtml

markdownToHtml :: HTML -> HTML
-- markdownToHtml = addTag "pre" . addTag "code" . postParse . parse . splitLine
--   where postParse  = init . tail . unlines . splitLine' . santize . show . pShowNoColor
--         splitLine' = Regex.splitRegex (Regex.mkRegex "\\\\n")
--         santize x  = Regex.subRegex (Regex.mkRegex "\\\\\"") x "\""
markdownToHtml = astToHtml . parse . splitLine
