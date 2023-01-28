-- Markup.hs

-- Types

type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]

main = ex2

ex1 :: Document
ex1 = Document
  [ Paragraph "Hello, world!"
  ]

ex2 :: Document
ex2 = [Structure]
  [ Heading 1 "Welcome"
  , Paragraph "To this tutorial about Haskell."
  ]
