import Test.HUnit

import Parse

main :: IO ()
main = mapM_
  (\(s, result) -> (parse [s]) @?= Markdown [Para { isOpen = True, elems = result }])
  [ ("Plain text"                   , [ Plain  "Plain text" ])
  , ("Plain text, *Emphasis1*"      , [ Plain  "Plain text, "
                                      , Emph   "Emphasis1" ])
  , ("Plain text, _Emphasis2_"      , [ Plain  "Plain text, "
                                      , Emph   "Emphasis2" ])
  , ("Plain text, _Not emph1*"      , [ Plain  "Plain text, _Not emph1*" ])
  , ("Plain text, *Not emph2_"      , [ Plain  "Plain text, *Not emph2_" ])
  , ("Plain text, **Strong1**"      , [ Plain  "Plain text, "
                                      , Strong "Strong1" ])
  , ("Plain text, __Strong2__"      , [ Plain  "Plain text, "
                                      , Strong "Strong2" ])
  , ("Plain text, __Not strong1**"  , [ Plain  "Plain text, __Not strong1**" ])
  , ("Plain text, **Not strong2__"  , [ Plain  "Plain text, **Not strong2__" ])
  , ("`code`"                       , [ Code   "code" ])
  , ("`*not emph*`"                 , [ Code   "*not emph*" ])
  , ("``not `common`mark`` sadly"   , [ Plain  "`"
                                      , Code   "not "
                                      , Plain  "common"
                                      , Code   "mark`"
                                      , Plain  " sadly" ])
  ]

testInline :: (String, [InlineElem]) -> Assertion
testInline (s, result) = (parse [s]) @?= (Markdown [Para { isOpen = True, elems = result }])
