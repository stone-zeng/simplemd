import Graphics.UI.Threepenny.Core

import Gui

main :: IO ()
main = startGUI defaultConfig
  { jsCustomHTML = Just "index.html"
  , jsStatic     = Just "../static"
  } gui
