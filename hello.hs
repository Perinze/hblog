-- hello.hs

import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title"
    ( flat_
      [ h1_ "Heading"
      , p_ "Paragraph #1"
      , p_ "Paragraph #2"
      , ul_
        [ p_ "item 1"
        , p_ "item 2"
        , p_ "item 3"
        ]
      , code_ "Haskell"
      ]
    )
