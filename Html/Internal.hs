-- Html/Internal.hs

module Html.Internal where

-- * Types

newtype Html
  = Html String

newtype Structure
  = Structure String

type Title
  = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title (Structure body) =
  Html
    ( el "html"
      ( el "head" (el "title" (escape title))
        <> el "body" body
      )
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

li_ :: Structure -> Structure
li_ = Structure . el "li" . getStructureString

ul_ :: [Structure] -> Structure
ul_ = tag_ "ul" . flat_ . map li_

ol_ :: [Structure] -> Structure
ol_ = tag_ "ol" . flat_ . map li_

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

flat_ :: [Structure] -> Structure
flat_ = Structure . concat . map getStructureString

tag_ :: String -> Structure -> Structure
tag_ tag = Structure . el tag . getStructureString

-- * Render

render :: Html -> String
render (Html s) = s

-- * Utilities

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString (Structure str) = str

escape :: String -> String
escape =
  let
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
  in
    concat . map escapeChar