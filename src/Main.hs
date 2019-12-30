import Graphics.UI.Threepenny.Core
import System.Directory

import Gui

main :: IO ()
main = do
  dirExist <- doesDirectoryExist "../static"
  let dirPath  = if dirExist then "../static" else "./static"
  let htmlPath = "index.html"
  startGUI defaultConfig
    { jsStatic     = Just dirPath
    , jsCustomHTML = Just htmlPath
    } gui
