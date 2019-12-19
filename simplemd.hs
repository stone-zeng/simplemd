{-# OPTIONS_GHC -Wno-unused-do-bind #-}

import Control.Monad
import Text.Regex
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Parse

main :: IO ()
main = startGUI defaultConfig
  { jsCustomHTML = Just "index.html"
  , jsStatic     = Just "static"
  } setup

setup :: Window -> UI ()
setup rootWindow = void $ do
  return rootWindow # set title "SimpleMD"

  heading <- UI.h1
    # set text "Welcome to SimpleMD!"

  description <- UI.h2
    # set text "Project homepage: "
    #+
      [UI.a # set text "https://github.com/stone-zeng/simplemd"
            # set href "https://github.com/stone-zeng/simplemd"]

  mdInput <- UI.div
    # set contenteditable "true"
    # set cssClass "md-input"
    #+
      [ UI.div # set text "# Heading 1"
      , UI.div # set text "## Heading 2"
      ]

  mdOutput <- UI.div
    # set text ""
    # set cssClass "md-output"

  wrapper <- UI.div
    # set cssClass "wrapper"
    #+ map element [mdInput, mdOutput]

  getBody rootWindow
    #+ map element [heading, description, wrapper]

  on UI.valueChange mdInput $ \_ -> do
    markdownText <- get myText mdInput
    -- element textarea # set html (highlight markdownText)
    element mdOutput   # set text (show $ parse $ splitLine markdownText)

generateAttr :: String -> WriteAttr Element String
generateAttr = mkWriteAttr . set' . attr

contenteditable, href, cssClass :: WriteAttr Element String
contenteditable = generateAttr "contenteditable"
href            = generateAttr "href"
cssClass        = generateAttr "class"

myText :: Attr Element String
myText = mkReadWriteAttr _get _set
  where
    _get   el = callFunction $ ffi "$(%1).html()" el
    _set v el = runFunction  $ ffi "$(%1).html(%2)" el v

-- highlight :: String -> String
-- highlight s = s



-- parse :: String -> String
-- parse htmlStr = htmlStr ++ "  ......  " ++ (intercalate " + " $ map (\x -> "\"" ++ x ++ "\"") $ splitLine htmlStr)


splitLine :: String -> [String]
splitLine s = removeEmptyLine $ map replaceBr $ splitRegex pattern s
  where
    pattern = mkRegex "</div><div>|<div>|</div>"

    replaceBr "<br>" = ""
    replaceBr x = x

    removeEmptyLine [] = []
    removeEmptyLine ("":xs) = removeEmptyLine xs
    removeEmptyLine x = if (last x == "") then removeEmptyLine (init x) else x
