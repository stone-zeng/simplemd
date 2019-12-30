{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Html (markdownToHtml) where

import qualified "regex-compat-tdfa" Text.Regex as Regex
import Text.RawString.QQ

#ifdef DEBUG
import Text.Pretty.Simple (pShowNoColor)
#endif

import Parse

type HTML = String

markdownToHtml :: String -> HTML
#ifdef DEBUG
markdownToHtml = addTag "pre" . addTag "code" . postParse . parse . lines
  where postParse  = init . tail . unlines . splitLine' . santize . show . pShowNoColor
        splitLine' = Regex.splitRegex (Regex.mkRegex [r|\\n|])
        santize    = mkSanitize
          [ ([r|<|], "&lt;")
          , ([r|>|], "&gt;")
          , ([r|&|], "&amp;")
          , ([r|\\"|], [r|"|])
          ]
#else
markdownToHtml md = concatMap blockToHtml md'
  where Markdown md' = parse $ lines md
#endif

blockToHtml :: BlockElem -> HTML
blockToHtml Para    {..} = addTag "p" $ inlineToHtml elems
blockToHtml Heading {..} = addTag tag $ inlineToHtml elems
  where tag = "h" ++ show level
blockToHtml Hrule        = "<hr />"
blockToHtml Pre     {..} = addTag "pre" $ addTag' "code" attr text
  where attr = "class='lang-" ++ lang ++ "'"
blockToHtml Ulist   {..} = addTag  "ul"      $ concatMap listItemToHtml items
blockToHtml Olist   {..} = addTag' "ol" attr $ concatMap listItemToHtml items
  where attr = "start='" ++ show start ++ "'"
blockToHtml Quote   {..} = addTag "blockquote" $ concatMap blockToHtml elems'

listItemToHtml :: ListItem -> HTML
listItemToHtml = addTag "li" . concatMap blockToHtml'
  where blockToHtml' Para {..} = inlineToHtml elems
        blockToHtml' x = blockToHtml x

inlineToHtml :: [InlineElem] -> HTML
inlineToHtml = concatMap inlineElemToHtml
  where
    inlineElemToHtml Plain    {..} = "" ++ htmlSanitize content ++ ""
    inlineElemToHtml Code     {..} = addTag  "code" content
    inlineElemToHtml Em       {..} = addTag  "em"     $ htmlSanitize content
    inlineElemToHtml Strong   {..} = addTag  "strong" $ htmlSanitize content
    inlineElemToHtml EmStrong {..} = addTag  "em"     $ addTag "strong" $ htmlSanitize content
    inlineElemToHtml Del      {..} = addTag  "del"    $ htmlSanitize content
    inlineElemToHtml Ins      {..} = addTag  "ins"    $ htmlSanitize content
    inlineElemToHtml Mark     {..} = addTag  "mark"   $ htmlSanitize content
    inlineElemToHtml Link     {..} = addTag' "a" attr $ htmlSanitize content
      where attr = "href='" ++ url ++ "'"
    inlineElemToHtml Img      {..} = "<img " ++ attr ++ " />"
      where attr = "alt='" ++ content ++ "' src='" ++ url ++ "'"

-- | Add HTML tag: `something` -> `<tag>something</tag>`.
addTag :: String -> String -> HTML
addTag tag s = begin ++ s ++ end
  where begin = "<"  ++ tag ++ ">"
        end   = "</" ++ tag ++ ">"

-- | Add HTML tag with attributes: `something` -> `<tag attr>something</tag>`.
addTag' :: String -> String -> String -> HTML
addTag' tag attr s = begin ++ s ++ end
  where begin = "<"  ++ tag ++ " " ++ attr ++ ">"
        end   = "</" ++ tag ++ ">"

-- | Sanitize `<` and `>` in HTML string; replace `(c)`, `(r)`, etc.
htmlSanitize :: String -> String
htmlSanitize = mkSanitize
  [ ([r|\\<|],         "&lt;")
  , ([r|\\>|],         "&gt;")
  , ([r|\((c|C)\)|],   "&copy;")
  , ([r|\((r|R)\)|],   "&reg;")
  , ([r|\((p|P)\)|],   "&sect;")
  , ([r|\((tm|TM)\)|], "&trade;")
  ]

mkSanitize :: [(String, String)] -> (String -> String)
mkSanitize subList = foldr (.) id (map mkSub subList)
  where mkSub (pattern, replace) = \x -> Regex.subRegex (Regex.mkRegex pattern) x replace
