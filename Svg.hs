module Svg (iconMarkdown, iconPreview) where

import Graphics.UI.Threepenny ((#), (#+))
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.SVG as SVG

iconMarkdown :: UI.UI UI.Element
iconMarkdown = do
  context <- SVG.svg
    # UI.set SVG.width  "16"
    # UI.set SVG.height "16"
    # UI.set SVG.viewBox "0 0 16 16"
    # UI.set SVG.fill "none"
  path <- SVG.path
    # UI.set SVG.d "M4.708 5.578L2.061 8.224L4.708 10.87L4 11.578L1 8.578V7.87L4 4.87L4.708 5.578ZM11.708 4.87L11 5.578L13.647 8.224L11 10.87L11.708 11.578L14.708 8.578V7.87L11.708 4.87ZM4.908 13L5.802 13.448L10.802 3.448L9.908 3L4.908 13Z"
    # UI.set SVG.fill "#9E9E9E"
  return context #+ [UI.element path]

iconPreview :: UI.UI UI.Element
iconPreview = do
  context <- SVG.svg
    # UI.set SVG.width  "16"
    # UI.set SVG.height "16"
    # UI.set SVG.viewBox "0 0 16 16"
    # UI.set SVG.fill "none"
  path <- SVG.path
    # UI.set SVG.fill_rule "evenodd"
    # UI.set SVG.clip_rule "evenodd"
    # UI.set SVG.d "M2 2H14L15 3V13L14 14H2L1 13V3L2 2ZM2 13H14V3H2V13ZM13 4H3V7H13V4ZM12 6H4V5H12V6ZM9 12H13V8H9V12ZM10 9H12V11H10V9ZM7 8H3V9H7V8ZM3 11H7V12H3V11Z"
    # UI.set SVG.fill "#9E9E9E"
  return context #+ [UI.element path]
