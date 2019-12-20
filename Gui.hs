{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Gui (gui) where

import Control.Monad
import Graphics.UI.Threepenny ((#), (#.), (#+))
import qualified Graphics.UI.Threepenny as UI

import Html
import Svg

gui :: UI.Window -> UI.UI ()
gui rootWindow = void $ do
  return rootWindow # UI.set UI.title "SimpleMD"

  title <- UI.h1
    # UI.set UI.text "Welcome to SimpleMD!"

  description <- UI.h2
    # UI.set UI.text "Project homepage: "
    #+ [UI.a # UI.set UI.text "https://github.com/stone-zeng/simplemd"
             # UI.set UI.href "https://github.com/stone-zeng/simplemd"]

  header <- UI.div
    #. "header-wrapper"
    #+ [UI.h3 # UI.set UI.text "MARKDOWN"
              #. "header-input"
              #+ [iconMarkdown #. "icon"]]
    #+ [UI.h3 # UI.set UI.text "PREVIEW"
              #. "header-output"
              #+ [iconPreview #. "icon"]]

  mdInput <- UI.div
    #. "md-input"
    # UI.set contenteditable "true"
    #+ initMdInput

  mdOutput <- UI.div
    #. "md-output"
    # UI.set UI.text ""

  wrapper <- UI.div
    #. "wrapper"
    #+ map UI.element [mdInput, mdOutput]

  UI.getBody rootWindow
    #+ map UI.element [title, description, header, wrapper]

  UI.on UI.valueChange mdInput $ \_ -> do
    markdownText <- UI.get html_ mdInput
    UI.element mdOutput
      # UI.set html_ (markdownToHtml markdownText)
    UI.runFunction $ UI.ffi
      "document.querySelectorAll('pre code').forEach((e) => { hljs.highlightBlock(e); });"

contenteditable :: UI.WriteAttr UI.Element String
contenteditable = UI.mkWriteAttr $ UI.set' $ UI.attr "contenteditable"

html_ :: UI.Attr UI.Element String
html_ = UI.mkReadWriteAttr get set
  where
    get   el = UI.callFunction $ UI.ffi "$(%1).html()" el
    set v el = UI.runFunction  $ UI.ffi "$(%1).html(%2)" el v

initMdInput :: [UI.UI UI.Element]
initMdInput =
  [ UI.div # UI.set UI.text "# Heading 1"
  , UI.div #+ [UI.br]
  , UI.div # UI.set UI.text "## Heading 2"
  , UI.div #+ [UI.br]
  , UI.div # UI.set UI.text "```c"
  , UI.div # UI.set UI.text "int main() {"
  , UI.div # UI.set UI.text "  return 0;"
  , UI.div # UI.set UI.text "}"
  , UI.div # UI.set UI.text "```"
  ]
