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
blockToHtml Para { elems = xs } = addTag "p" $ inlineListToHtml xs
blockToHtml Heading { level = k, elems = xs } = addTag tag $ inlineListToHtml xs
  where tag = "h" ++ show k
blockToHtml Hrule = "<hr />"
blockToHtml Pre { lang = preLang, text = preText} = addTag "pre" $ addTag' "code" attr preText
  where attr = "class='lang-" ++ preLang ++ "'"
blockToHtml Ulist { elems' = xs } = addTag "ul" $ concatMap blockToHtml xs
blockToHtml Olist { start = k, elems' = xs } = addTag' "ol" attr $ concatMap blockToHtml xs
  where attr = "start='" ++ show k ++ "'"
blockToHtml Quote { elems' = xs } = addTag "blockquote" $ concatMap blockToHtml xs
blockToHtml Block { elems = xs } = inlineListToHtml xs

markdownToHtml :: HTML -> HTML
-- markdownToHtml = addTag "pre" . addTag "code" . postParse . parse . splitLine
--   where postParse  = init . tail . unlines . splitLine' . santize . show . pShowNoColor
--         splitLine' = Regex.splitRegex (Regex.mkRegex "\\\\n")
--         santize x  = Regex.subRegex (Regex.mkRegex "\\\\\"") x "\""
markdownToHtml = astToHtml . parse . splitLine
