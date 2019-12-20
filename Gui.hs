{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Gui (gui) where

import Control.Monad
import Graphics.UI.Threepenny ((#), (#+))
import qualified Graphics.UI.Threepenny as UI

import Html

gui :: UI.Window -> UI.UI ()
gui rootWindow = void $ do
  return rootWindow # UI.set UI.title "SimpleMD"

  heading <- UI.h1
    # UI.set UI.text "Welcome to SimpleMD!"

  description <- UI.h2
    # UI.set UI.text "Project homepage: "
    #+
      [UI.a # UI.set UI.text "https://github.com/stone-zeng/simplemd"
            # UI.set UI.href "https://github.com/stone-zeng/simplemd"]

  mdInput <- UI.div
    # UI.set contenteditable "true"
    # UI.set UI.class_ "md-input"
    #+
      [ UI.div # UI.set UI.text "# Heading 1"
      , UI.div # UI.set UI.text "## Heading 2"
      ]

  mdOutput <- UI.div
    # UI.set UI.text ""
    # UI.set UI.class_ "md-output"

  wrapper <- UI.div
    # UI.set UI.class_ "wrapper"
    #+ map UI.element [mdInput, mdOutput]

  UI.getBody rootWindow
    #+ map UI.element [heading, description, wrapper]

  UI.on UI.valueChange mdInput $ \_ -> do
    markdownText <- UI.get htmlText mdInput
    UI.element mdOutput
      # UI.set UI.html (markdownToHtml markdownText)

generateAttr :: String -> UI.WriteAttr UI.Element String
generateAttr = UI.mkWriteAttr . UI.set' . UI.attr

contenteditable :: UI.WriteAttr UI.Element String
contenteditable = generateAttr "contenteditable"

htmlText :: UI.Attr UI.Element String
htmlText = UI.mkReadWriteAttr get set
  where
    get   el = UI.callFunction $ UI.ffi "$(%1).html()" el
    set v el = UI.runFunction  $ UI.ffi "$(%1).html(%2)" el v
