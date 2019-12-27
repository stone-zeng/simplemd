{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Gui (gui) where

import Control.Monad
import Graphics.UI.Threepenny ((#), (#.), (#+))
import qualified Graphics.UI.Threepenny as UI

import Html
import Svg

gui :: UI.Window -> UI.UI ()
gui window = void $ do
  return window # UI.set UI.title "SimpleMD"

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

  mdInput <- UI.textarea
    #. "md-input"
    # UI.set UI.value initMdInput

  mdOutput <- UI.div
    #. "md-output"
    # UI.set UI.text ""

  wrapper <- UI.div
    #. "wrapper"
    #+ map UI.element [mdInput, mdOutput]

  UI.getBody window
    #+ map UI.element [title, description, header, wrapper]

  UI.on UI.valueChange mdInput $ \_ -> do
    markdownText <- UI.get UI.value mdInput
    UI.element mdOutput
      # UI.set html (markdownToHtml markdownText)
    UI.runFunction $ UI.ffi jsCode

html :: UI.Attr UI.Element String
html = UI.mkReadWriteAttr get set
  where
    get   el = UI.callFunction $ UI.ffi "$(%1).html()" el
    set v el = UI.runFunction  $ UI.ffi "$(%1).html(%2)" el v

initMdInput :: String
initMdInput = foldr (\x y -> x ++ "\n" ++ y) ""
  [ "# Heading 1"
  , ""
  , "- This is a `code` span."
  , "- We can *emphasis* and make words **strong**."
  , "- Even math functions: $\\sin^2\\alpha+\\cos^2\\alpha=1$"
  , ""
  , "## Heading 2"
  , ""
  , "```haskell"
  , "class Monad m where"
  , "  return :: a -> m a"
  , "  (>>=)  :: m a -> (a -> m b) -> m b"
  , "```"
  ]

jsCode :: String
jsCode = "document.querySelectorAll('pre code').forEach((e) => { hljs.highlightBlock(e); });\
        \ renderMathInElement(document.body, {\
        \   'delimiters':\
        \     [{ left: '$$', right: '$$', display: true }, { left: '$', right: '$', display: false }]\
        \ });"
