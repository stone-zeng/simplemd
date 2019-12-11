{-# OPTIONS_GHC -Wno-unused-do-bind #-}

import Control.Monad

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup rootWindow = void $ do
    return rootWindow # set title "SVG"

    heading <- UI.h1 # set text "SVG Two Ways"

    textarea <- (UI.div # set contenteditable "true") #+ [
            UI.div # set text "# Heading 1",
            UI.div # set text "## Heading 2"
        ]
    result <- UI.div # set text ""

    getBody rootWindow #+ [
            element heading,
            element textarea,
            element result
        ]

    on UI.valueChange textarea $ \_ -> do
        markdownText <- get myText textarea
        element textarea # set text (highlight markdownText)
        element result   # set text (parse markdownText)

contenteditable :: WriteAttr Element String
contenteditable = mkWriteAttr (set' (attr "contenteditable"))

myText :: Attr Element String
myText = mkReadWriteAttr _get _set
    where
    _get   el = callFunction $ ffi "$(%1).html()" el
    _set v el = runFunction  $ ffi "$(%1).html(%2)" el v

highlight :: String -> String
highlight = undefined

parse :: String -> String
parse = undefined
