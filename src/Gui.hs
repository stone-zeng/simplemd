{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE QuasiQuotes #-}

module Gui (gui) where

import Control.Monad
import Graphics.UI.Threepenny ((#), (#.), (#+))
import qualified Graphics.UI.Threepenny as UI
import Text.RawString.QQ

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
initMdInput = [r|# SimpleMD

*A simple Markdown editor, written in Haskell*

---

## Basic syntax

### Unordered list

- This is a `code` span
- Links:
  - This is a link to
    [Fudan University](https://www.fudan.edu.cn/) :mortar_board:
  - Autolink: <mailto:xdzeng18@fudan.edu.cn>
- We can *emphasize text* and make them **strong** :muscle:

### Ordered list

1. Wikipedia, the free encyclopedia that anyone can edit
  1. 维基百科 - 海纳百川，有容乃大
  1. 維基百科 - 海納百川，有容乃大
1. ウィキペディアは誰でも編集できるフリー百科事典です
1. 위키백과 - 우리 모두가 만들어가는 자유 백과사전

### Code blocks

```haskell
class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
```

### Blockquote

> Markdown is a **lightweight markup language** with plain text formatting syntax. Its design allows it to be converted to many output formats, but the original tool by the same name only supports HTML.
>
> Bullet list:
>
> * apples
>   * 红富士 :apple:
> * oranges
> * pears
>
> > C++ "Hello World":
> >
> > ```cpp
> > int main() {
> >   std::cout << "Hello, 2020!" << std:: endl;
> > }
> > ```

### Images

![haskell](https://www.haskell.org/img/haskell-logo.svg)

## Extended features

- Math formulas:
  - $\mathrm{e}^{\mathrm{i}\pi} + 1 = 0$
  - $\sin^2\alpha + \cos^2\alpha = 1$
- Emoji:
  - :grinning: :date: :cn: :+1:

$$
  \int :mouse:^2 \mathrm{d} :mouse:
= \frac13 :mouse:^3 + \mathbb{C}
$$

- Typographic replacements:
  - (c) (p) (tm)
  - En-dash: Curry--Howard Correspondence; em-dash --- like this
  - "Smart quotes" vs `"raw quotes"`
- HTML tags:

<table>
 <tr>
  <th>Name</th>
  <th>Favorite Color</th>
 </tr>
 <tr>
  <td>Bob</td>
  <td>Yellow</td>
 </tr>
 <tr>
  <td>Michelle</td>
  <td>Purple</td>
 </tr>
</table>
|]

jsCode :: String
jsCode = [r|document.querySelectorAll('pre code').forEach((e) => { hljs.highlightBlock(e); });
renderMathInElement(document.body, {
  'delimiters':
    [{ left: '$$', right: '$$', display: true }, { left: '$', right: '$', display: false }]
});
|]
