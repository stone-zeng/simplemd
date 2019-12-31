{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Parse
  ( InlineElem (..)
  , BlockElem  (..)
  , ListItem
  , Markdown   (..)
  , parse
  ) where

import qualified Data.Map as Map
import qualified "regex-compat-tdfa" Text.Regex as Regex
import Text.RawString.QQ

import Emoji

#ifdef DEBUG

import Debug.Trace
import Text.Pretty.Simple (pPrint)

myTraceId :: Show a => a -> a
myTraceId x = trace ("\nDEBUG: <" ++ show x ++ ">\n") x

#endif

-- | Markdown AST.
newtype Markdown = Markdown { unMarkdown :: [BlockElem] }
  deriving (Eq)

instance Show Markdown where
  show Markdown {..} = "Markdown: " ++ show unMarkdown

-- | All possible block elements in Markdown.
data BlockElem =
    Hrule                                                           -- ^ HTML <hr />
  | Heading { level  :: Int,  elems  :: [InlineElem] }              -- ^ HTML <h1>, <h2>, ... <h6>
  | Para    { isOpen :: Bool, elems  :: [InlineElem] }              -- ^ HTML <p>
  | Pre     { isOpen :: Bool, lang   :: String, text :: String }    -- ^ HTML <pre>
  | Ulist   { isOpen :: Bool, items  :: [ListItem] }                -- ^ HTML <ul>
  | Olist   { isOpen :: Bool, start  :: Int, items :: [ListItem] }  -- ^ HTML <ol>
  | Quote   { isOpen :: Bool, elems' :: [BlockElem] }               -- ^ HTML <blockquote>
  deriving (Eq)

instance Show BlockElem where
  show Hrule        = "Hrule"
  show Heading {..} = "Heading (" ++ show level ++ "): " ++ show elems
  show Para    {..} = "Para"  ++ (if isOpen then "+" else "-") ++ ": " ++ show elems
  show Ulist   {..} = "Ulist" ++ (if isOpen then "+" else "-") ++ ": " ++ show items
  show Quote   {..} = "Quote" ++ (if isOpen then "+" else "-") ++ ": " ++ show elems'
  show Pre     {..} = "Pre"   ++ (if isOpen then "+" else "-") ++ " (" ++ show lang  ++ "): " ++ show text
  show Olist   {..} = "Olist" ++ (if isOpen then "+" else "-") ++ " (" ++ show start ++ "): " ++ show items

type ListItem = [BlockElem]

closeBlockElem :: BlockElem -> BlockElem
closeBlockElem Para  {..} = Para  { isOpen = False, ..}
closeBlockElem Pre   {..} = Pre   { isOpen = False, ..}
closeBlockElem Ulist {..} = Ulist { isOpen = False, ..}
closeBlockElem Olist {..} = Olist { isOpen = False, ..}
closeBlockElem Quote {..} = Quote { isOpen = False, ..}
closeBlockElem x = x

-- | All possible inline elements in Markdown.
data InlineElem =
    Plain    { content :: String }
  | Code     { content :: String }                 -- ^ HTML <code>
  | Em       { content :: String }                 -- ^ HTML <em>
  | Strong   { content :: String }                 -- ^ HTML <strong>
  | EmStrong { content :: String }                 -- ^ HTML <em><strong>
  | Del      { content :: String }                 -- ^ HTML <del>
  | Ins      { content :: String }                 -- ^ HTML <ins>
  | Mark     { content :: String }                 -- ^ HTML <mark>
  | Sup      { content :: String }                 -- ^ HTML <sup>
  | Sub      { content :: String }                 -- ^ HTML <sub>
  | Link     { content :: String, url :: String }  -- ^ HTML <a href="...">
  | Img      { content :: String, url :: String }  -- ^ HTML <img src="..." alt="...">
  deriving (Eq, Show)

parse :: [String] -> Markdown
parse = foldl parseMarkdown $ Markdown []

parseMarkdown :: Markdown -> String -> Markdown
parseMarkdown (Markdown []) s = Markdown [result]
  where Left result = parseHeading s
                  >>= parseHrule
                  >>= parsePre
                  >>= parseUlist
                  >>= parseOlist
                  >>= parseQuote
                  >>= parsePara
parseMarkdown (Markdown mdElements) s =
  case last mdElements of
    Pre   { isOpen = True, .. } -> Markdown $ init mdElements ++ nextPre   lang text   s
    Para  { isOpen = True, .. } -> Markdown $ init mdElements ++ nextPara  elems       s
    Ulist { isOpen = True, .. } -> Markdown $ init mdElements ++ nextUlist items       s
    Olist { isOpen = True, .. } -> Markdown $ init mdElements ++ nextOlist start items s
    Quote { isOpen = True, .. } -> Markdown $ init mdElements ++ nextQuote elems'      s
    _ -> Markdown $ if null s
      then mdElements
      else mdElements ++ (unMarkdown $ parseMarkdown (Markdown []) s)

nextPre :: String -> String -> String -> [BlockElem]
nextPre preLang preText s = case s of
  "```" -> [Pre { isOpen = False, lang = preLang, text = preText }]
  _     -> [Pre { isOpen = True,  lang = preLang, text = s' }]
    where s' = if null preText then s else preText ++ "\n" ++ s

nextPara :: [InlineElem] -> String -> [BlockElem]
nextPara para s = if null s
  then [Para { isOpen = False, elems = para }]
  else case result of
    Left  x -> [Para { isOpen = False, elems = para }] .+ x
    Right _ -> [Para { isOpen = True,  elems = para ++ parseInline ("\n" ++ s) }]
    where result = parseHeading s
               >>= parseHrule
               >>= parsePre
               >>= parseUlist
               >>= parseOlist
               >>= parseQuote

nextUlist :: [ListItem] -> String -> [BlockElem]
nextUlist xs s = case s of
  -- An empty line will close <ul>.
  "" -> closeUlist xs
  _  -> case detectIndent s of
    -- No indent afterwards.
    -- If `s` begins with `- ...`, then append a new <li>; otherwise close the <ul>.
    (0, _) -> case result of
      -- Other objects will close <ul>.
      Left block -> closeUlist xs .+ block
      Right _    -> case Regex.matchRegex ulistPatt s of
        -- Add a new <li>.
        Just x  -> [Ulist { isOpen = True, items = (init xs) .+ lastItem .+ nextItem }]
          where lastItem = (init $ last xs) .+ closeBlockElem (last $ last xs)
                nextItem = [Para { isOpen = True, elems = parseInline $ last x}]
        -- Update the last <li>.
        Nothing -> fallback
      where result = parseHeading s
                 >>= parseHrule
                 >>= parsePre
                 >>= parseOlist
                 >>= parseQuote
    (2, s') -> case detectDepth xs of
      0 -> case Regex.matchRegex ulistPatt s' of
        -- Add a new <ul>.
        Just x  -> [Ulist { isOpen = True, items = items }]
          where items    = (init xs) .+ (lastItem .+ ulist)
                lastItem = (init $ last xs) .+ closeBlockElem (last $ last xs)
                ulist    = Ulist { isOpen = True
                                 , items  = [[ Para { isOpen = True
                                                    , elems  = parseInline $ last x
                                                    } ]]
                                 }
        Nothing -> case Regex.matchRegex olistPatt s' of
          -- Add a new <ol>.
          Just x  -> [Ulist { isOpen = True, items = items }]
            where items    = (init xs) .+ (lastItem .+ olist)
                  lastItem = (init $ last xs) .+ closeBlockElem (last $ last xs)
                  olist    = Olist { isOpen = True
                                   , start  = read $ head x
                                   , items  = [[ Para { isOpen = True
                                                      , elems  = parseInline $ last x
                                                      } ]]
                                   }
          Nothing -> fallback
      1 -> case Regex.matchRegex ulistPatt s' of
        -- Add a new <ul> inside the last <li>.
        Just x  -> [Ulist { isOpen = True, items = items' }]
          where items' = (init xs) .+ ((init $ last xs) .+ ulist)
                ulist  = Ulist { isOpen = True
                               , items = (items $ last $ last xs)
                                  .+ [Para { isOpen = True, elems = parseInline $ last x }]
                               }
        Nothing -> case Regex.matchRegex olistPatt s' of
          -- Add a new <ol> inside the last <li>.
          Just x  -> [Ulist { isOpen = True, items = items' }]
            where items' = (init xs) .+ ((init $ last xs) .+ olist)
                  olist  = Olist { isOpen = True
                                 , start = 1  -- FIXME: use correct number
                                 , items = (items $ last $ last xs)
                                    .+ [Para { isOpen = True, elems = parseInline $ last x }]
                                 }
          Nothing -> fallback
      _ -> fallback
    _ -> fallback
    where fallback = [updateUlist Ulist { isOpen = True, items = xs } $ "\n" ++ s]

nextOlist :: Int -> [ListItem] -> String -> [BlockElem]
nextOlist start xs s = case s of
  -- An empty line will close <ol>.
  "" -> closeOlist start xs
  _  -> case detectIndent s of
    -- No indent afterwards.
    -- If `s` begins with `x. ...`, then append a new <li>; otherwise close the <ol>.
    (0, _) -> case result of
      -- Other objects will close <ol>.
      Left block -> closeOlist start xs .+ block
      Right _    -> case Regex.matchRegex olistPatt s of
        -- Add a new <li>, do not change `start`.
        Just x  -> [Olist { isOpen = True, start = start, items = (init xs) .+ lastItem .+ nextItem }]
          where lastItem = (init $ last xs) .+ closeBlockElem (last $ last xs)
                nextItem = [Para { isOpen = True, elems = parseInline $ last x}]
        -- Update the last <li>.
        Nothing -> fallback
      where result = parseHeading s
                 >>= parseHrule
                 >>= parsePre
                 >>= parseUlist
                 >>= parseQuote
    (2, s') -> case detectDepth xs of
      0 -> case Regex.matchRegex ulistPatt s' of
        -- Add a new <ul>.
        Just x  -> [Olist { isOpen = True, start = start, items = items }]
          where items    = (init xs) .+ (lastItem .+ ulist)
                lastItem = (init $ last xs) .+ closeBlockElem (last $ last xs)
                ulist    = Ulist { isOpen = True
                                 , items  = [[ Para { isOpen = True
                                                    , elems  = parseInline $ last x
                                                    } ]]
                                 }
        Nothing -> case Regex.matchRegex olistPatt s' of
          -- Add a new <ol>.
          Just x  -> [Olist { isOpen = True, start = start, items = items }]
            where items    = (init xs) .+ (lastItem .+ olist)
                  lastItem = (init $ last xs) .+ closeBlockElem (last $ last xs)
                  olist    = Olist { isOpen = True
                                   , start  = read $ head x
                                   , items  = [[ Para { isOpen = True
                                                      , elems  = parseInline $ last x
                                                      } ]]
                                   }
          Nothing -> fallback
      1 -> case Regex.matchRegex ulistPatt s' of
        -- Add a new <ul> inside the last <li>.
        Just x  -> [Olist { isOpen = True, start = start, items = items' }]
          where items' = (init xs) .+ ((init $ last xs) .+ ulist)
                ulist  = Ulist { isOpen = True
                               , items = (items $ last $ last xs)
                                  .+ [Para { isOpen = True, elems = parseInline $ last x }]
                               }
        Nothing -> case Regex.matchRegex olistPatt s' of
          -- Add a new <ol> inside the last <li>.
          Just x  -> [Olist { isOpen = True, start = start, items = items' }]
            where items' = (init xs) .+ ((init $ last xs) .+ olist)
                  olist  = Olist { isOpen = True
                                 , start = 1  -- FIXME: use correct number
                                 , items = (items $ last $ last xs)
                                    .+ [Para { isOpen = True, elems = parseInline $ last x }]
                                 }
          Nothing -> fallback
      _ -> fallback
    _ -> fallback
    where fallback = [updateOlist Olist { isOpen = True, start = start, items = xs } $ "\n" ++ s]

-- | Update the deepest element in a <ul>/<ol>.
-- Examples:
--
-- updateUlist (Ulist { isOpen = True, items = [[Para { isOpen = True, elems = parseInline "text" }]]}) "extra"
-- -> Ulist+: [[Para+: [Plain {content = "text"},Plain {content = "extra"}]]]
--
-- updateUlist
--   (Ulist
--     { isOpen = True
--     , items  = [ [ Para  { isOpen = False, elems = parseInline "text1" } ]
--                , [ Para  { isOpen = False, elems = parseInline "text2" }
--                  , Ulist { isOpen = True, items = [[Para { isOpen = True, elems = parseInline "text3"}]] }
--                  ]
--                ] })
--   "extra"
-- -> Ulist+: [ [ Para-: [Plain {content = "text1"}] ]
--            , [ Para-: [Plain {content = "text2"}]
--              , Ulist+: [[Para+: [Plain {content = "text3"},Plain {content = "extra"}]]]
--              ] ]
updateUlist :: BlockElem -> String -> BlockElem
updateUlist Ulist { isOpen = True, .. } s =
  Ulist { isOpen = True, items = initItems .+ lastItem' }
    where initItems = init items
          lastItem  = last items
          lastItem' = (init lastItem) .+ case last lastItem of
            Para  { isOpen = True, .. } ->
              Para { isOpen = True, elems = elems ++ parseInline s }
            Ulist { isOpen = True, items = _ } ->
              updateUlist (last lastItem) s
            Olist { isOpen = True, items = _, .. } ->
              updateOlist (last lastItem) s
            _ -> last lastItem  -- Do not change
updateUlist x _ = x

updateOlist :: BlockElem -> String -> BlockElem
updateOlist Olist { isOpen = True, .. } s =
  Olist { isOpen = True, start = start, items = initItems .+ lastItem' }
    where initItems = init items
          lastItem  = last items
          lastItem' = (init lastItem) .+ case last lastItem of
            Para  { isOpen = True, .. } ->
              Para { isOpen = True, elems = elems ++ parseInline s }
            Ulist { isOpen = True, items = _ } ->
              updateUlist (last lastItem) s
            Olist { isOpen = True, items = _, start = _ } ->
              updateOlist (last lastItem) s
            _ -> last lastItem  -- Do not change
updateOlist x _ = x

-- | Close <ul>/<ol>, as well as the last <p> element inside this <ul>/<ol>.
closeUlist :: [ListItem] -> [BlockElem]
closeUlist xs = [Ulist { isOpen = False, items = (init xs) .+ lastItem }]
  where lastItem  = (init $ last xs) .+ lastItem'
        lastItem' = closeBlockElem (last $ last xs)

closeOlist :: Int -> [ListItem] -> [BlockElem]
closeOlist start xs = [Olist { isOpen = False, start = start, items = (init xs) .+ lastItem }]
  where lastItem  = (init $ last xs) .+ lastItem'
        lastItem' = closeBlockElem (last $ last xs)

-- | Return the number of leading spaces and the sub-string with leading spaces removed.
detectIndent :: String -> (Int, String)
detectIndent "" = (0, "")
detectIndent s  = case head s of
  ' ' -> (n + 1, s')
    where (n, s') = detectIndent $ tail s
  _  -> (0, s)

-- | Return the depth of a <ul> or an <ol>.
-- Examples:
--
-- [[ Ulist { isOpen = True, items = item' } ]]
-- -> 1
--
-- [[ Ulist { isOpen = True, items = [[item], [item]] } ]]
-- -> 1
--
-- [[ Ulist { isOpen = True, items = [[Ulist { isOpen = True,  items = item' }]] } ]]
-- -> 2
--
-- [[ Ulist { isOpen = True, items = [[Ulist { isOpen = False, items = item' }], [item]] } ]]
-- -> 1
detectDepth :: [ListItem] -> Int
detectDepth [] = 0
detectDepth xs = case last $ last xs of
  Ulist {..} -> 1 + detectDepth items
  Olist {..} -> 1 + detectDepth items
  _          -> 0

nextQuote :: [BlockElem] -> String -> [BlockElem]
nextQuote xs s = case s of
  ""  -> [Quote { isOpen = False, elems' = xs }]
  ">" -> [Quote { isOpen = True,  elems' = (init xs) .+ (closeBlockElem $ last xs) }]
  _   -> case result of
    Left x  -> [Quote { isOpen = False, elems' = xs }] .+ x
    Right _ -> [Quote { isOpen = True,  elems' = elems' }]
    where
      result = parseHeading s
           >>= parseHrule
           >>= parsePre
           >>= parseUlist
           >>= parseOlist
      elems' = case Regex.matchRegex quotePatt s of
        Just x' -> unMarkdown $ parseMarkdown (Markdown xs) $ last x'
        Nothing -> case last xs of
          Para { elems = elems_, ..} ->
            (init xs) .+ Para { isOpen = True, elems = elems_ ++ nextElems }
          _ -> xs .+ Para { isOpen = True, elems = nextElems }
      nextElems = parseInline $ "\n" ++ s

-- | Parse string into Markdown elements.
parseHeading
  , parseHrule
  , parsePre
  , parseUlist
  , parseOlist
  , parseQuote
  , parsePara :: String -> Either BlockElem String
parseHeading = mkParser headingPatt parseHeading_
parseHrule   = mkParser hrulePatt   parseHrule_
parsePre     = mkParser prePatt     parsePre_
parseUlist   = mkParser ulistPatt   parseUlist_
parseOlist   = mkParser olistPatt   parseOlist_
parseQuote   = mkParser quotePatt   parseQuote_
parsePara s  = Left $ Para { isOpen = True, elems = parseInline s }

-- | Use `pattern` and `parser` to make a parse function.
mkParser :: Regex.Regex -> ([String] -> a) -> (String -> Either a String)
mkParser pattern parser = \s -> case Regex.matchRegex pattern s of
  Just x  -> Left $ parser x
  Nothing -> Right s

-- | Regex patterns for Markdown elements.
-- Note that `<p>` does not need a pattern.
hrulePatt
  , headingPatt
  , prePatt
  , ulistPatt
  , olistPatt
  , quotePatt :: Regex.Regex
hrulePatt   = Regex.mkRegex [r|^(\-{3,}|\*{3,}|_{3,}) *$|]  -- [(---|***|___)]
headingPatt = Regex.mkRegex [r|^(#{1,6}) (.*)|]             -- [(#), (content)]
prePatt     = Regex.mkRegex [r|^```(.*)|]                   -- [(preLang)]
ulistPatt   = Regex.mkRegex [r|^(-|\*|\+) (.*)|]            -- [(-|*|+), (content)]
olistPatt   = Regex.mkRegex [r|^([0-9]+)\. (.*)|]           -- [(number), (content)]
quotePatt   = Regex.mkRegex [r|^> (.*)|]                    -- [(content)]

-- | Parsers for Markdown elements.
-- Note that `<p>` does not need a parser.
parseHeading_
  , parseHrule_
  , parsePre_
  , parseUlist_
  , parseOlist_
  , parseQuote_ :: [String] -> BlockElem
parseHeading_ x = Heading { level = length $ head x , elems = parseInline $ last x }
parseHrule_ _ = Hrule
parsePre_ x = Pre { isOpen = True, lang = head x, text = "" }
parseUlist_ x = Ulist
  { isOpen = True, items  = [[Para { isOpen = True, elems = parseInline $ last x }]] }
parseOlist_ x = Olist
  { isOpen = True
  , start  = read $ head x
  , items  = [[Para { isOpen = True, elems = parseInline $ last x }]]
  }
parseQuote_ x = Quote { isOpen = True, elems' = unMarkdown $ parseMarkdown (Markdown []) $ head x }


-- | Parse inline
parseInline :: String -> [InlineElem]
parseInline "" = []
parseInline s = result
  where Left result = parseCode s    -- Should be the first
                  >>= parseEmoji
                  >>= parseImg       -- Should before link
                  >>= parseLink
                  >>= parseAutoLink
                  >>= parseEmStrong  -- Should before strong and em
                  >>= parseStrong    -- Should before em
                  >>= parseEm
                  >>= parseDel       -- Should before <sub>
                  >>= parseIns
                  >>= parseMark
                  >>= parseSup
                  >>= parseSub
                  >>= parsePlain

parseCode
  , parseLink
  , parseImg
  , parseAutoLink
  , parseEm
  , parseStrong
  , parseEmStrong
  , parseDel
  , parseIns
  , parseMark
  , parseSup
  , parseSub
  , parseEmoji
  , parsePlain :: String -> Either [InlineElem] String
parseCode     = mkParser codePatt     parseCode_
parseLink     = mkParser linkPatt     parseLink_
parseImg      = mkParser imgPatt      parseImg_
parseAutoLink = mkParser autoLinkPatt parseAutoLink_
parseEm       = mkParser emPatt       parseEm_
parseStrong   = mkParser strongPatt   parseStrong_
parseEmStrong = mkParser emStrongPatt parseEmStrong_
parseDel      = mkParser delPatt      parseDel_
parseIns      = mkParser insPatt      parseIns_
parseMark     = mkParser markPatt     parseMark_
parseSup      = mkParser supPatt      parseSup_
parseSub      = mkParser subPatt      parseSub_
parseEmoji    = mkParser emojiPatt    parseEmoji_
parsePlain s  = Left [Plain s]

parseCode_
  , parseLink_
  , parseImg_
  , parseAutoLink_
  , parseEm_
  , parseStrong_
  , parseEmStrong_
  , parseDel_
  , parseIns_
  , parseMark_
  , parseSup_
  , parseSub_
  , parseEmoji_ :: [String] -> [InlineElem]
parseCode_     x = parseInline_ x $ Code     { content = x !! 1 }
parseLink_     x = parseInline_ x $ Link     { content = x !! 1, url = x !! 2 }
parseImg_      x = parseInline_ x $ Img      { content = x !! 1, url = x !! 2 }
parseAutoLink_ x = parseInline_ x $ Link     { content = x !! 1, url = x !! 1 }
parseEm_       x = parseInline_ x $ Em       { content = x !! 2 ++ x !! 3 }
parseStrong_   x = parseInline_ x $ Strong   { content = x !! 2 ++ x !! 3 }
parseEmStrong_ x = parseInline_ x $ EmStrong { content = x !! 2 ++ x !! 3 }
parseDel_      x = parseInline_ x $ Del      { content = x !! 1 }
parseIns_      x = parseInline_ x $ Ins      { content = x !! 1 }
parseMark_     x = parseInline_ x $ Mark     { content = x !! 1 }
parseSup_      x = parseInline_ x $ Sup      { content = x !! 1 }
parseSub_      x = parseInline_ x $ Sub      { content = x !! 1 }
parseEmoji_    x = parseInline_ x $ Plain    { content = emoji }
  where name  = x !! 1
        emoji = case Map.lookup name emojiMap of
          Just y  -> y
          Nothing -> ":" ++ name ++ ":"

parseInline_ :: [String] -> InlineElem -> [InlineElem]
parseInline_ result e = (parseInline $ head result) ++ [e] ++ (parseInline $ last result)

codePatt
  , linkPatt
  , imgPatt
  , autoLinkPatt
  , emPatt
  , strongPatt
  , emStrongPatt
  , delPatt
  , insPatt
  , markPatt
  , supPatt
  , subPatt
  , emojiPatt :: Regex.Regex
codePatt     = Regex.mkRegex [r|(.*)`(.+)`(.*)|]                         -- `...`
linkPatt     = Regex.mkRegex [r|(.*)\[(.*)\]\(([^ ]+)\)(.*)|]            -- [...](...)
imgPatt      = Regex.mkRegex [r|(.*)!\[(.*)\]\(([^ ]+)\)(.*)|]           -- ![...](...)
autoLinkPatt = Regex.mkRegex [r|(.*)<([a-z]+:[^ ]+)>(.*)|]               -- <xxx:...>
emPatt       = Regex.mkRegex [r|(.*)(\*(.+)\*|_(.+)_)(.*)|]              -- *...*     | _..._
strongPatt   = Regex.mkRegex [r|(.*)(\*\*(.+)\*\*|__(.+)__)(.*)|]        -- **...**   | __...__
emStrongPatt = Regex.mkRegex [r|(.*)(\*\*\*(.+)\*\*\*|___(.+)___)(.*)|]  -- ***...*** | ___...___
delPatt      = Regex.mkRegex [r|(.*)~~(.+)~~(.*)|]                       -- ~~...~~
insPatt      = Regex.mkRegex [r|(.*)\+\+(.+)\+\+(.*)|]                   -- ++...++
markPatt     = Regex.mkRegex [r|(.*)==(.+)==(.*)|]                       -- ==...==
supPatt      = Regex.mkRegex [r|(.*)\^([a-zA-Z0-9_+\-]+)\^(.*)|]         -- ^...^
subPatt      = Regex.mkRegex [r|(.*)~([a-zA-Z0-9_+\-]+)~(.*)|]           -- ~...~
emojiPatt    = Regex.mkRegex [r|(.*):([a-z0-9_]+|\+1|-1):(.*)|]          -- :...:

-- | Append element to a list.
(.+) :: [a] -> a -> [a]
xs .+ x = xs ++ [x]
