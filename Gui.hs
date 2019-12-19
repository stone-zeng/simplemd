{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Gui (
    gui
  ) where

import Control.Monad
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Html

gui :: Window -> UI ()
gui rootWindow = void $ do
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
    # set htmlClass "md-input"
    #+
      [ UI.div # set text "# Heading 1"
      , UI.div # set text "## Heading 2"
      ]

  mdOutput <- UI.div
    # set text ""
    # set htmlClass "md-output"

  wrapper <- UI.div
    # set htmlClass "wrapper"
    #+ map element [mdInput, mdOutput]

  getBody rootWindow
    #+ map element [heading, description, wrapper]

  on UI.valueChange mdInput $ \_ -> do
    -- _ <- callFunction jsPrint
    markdownText <- get htmlText mdInput
    element mdOutput
      # set html (markdownToHtml markdownText)

updateDom :: String -> JSFunction String
updateDom = ffi "document.querySelector('.md-output').innerHTML=%1"

generateAttr :: String -> WriteAttr Element String
generateAttr = mkWriteAttr . set' . attr

contenteditable, href, htmlClass :: WriteAttr Element String
contenteditable = generateAttr "contenteditable"
href            = generateAttr "href"
htmlClass       = generateAttr "class"


htmlText :: Attr Element String
htmlText = mkReadWriteAttr get_ set_
  where
    get_   el = callFunction $ ffi "$(%1).html()" el
    set_ v el = runFunction  $ ffi "$(%1).html(%2)" el v
