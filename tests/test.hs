-- test.hs

import Markup

main :: IO ()
main = (putStrLn . testAll) testcases

testAll :: [Test] -> String
testAll = concat . (map (show . test))

test :: Test -> Result
test (Case string document) =
  let
    result = parse string
  in
    if parse string == document then
      Ok
    else
      Fail string document result

data Test
  = Case String Document

data Result
  = Ok
  | Fail String Document Document

instance Show Result where
  show result =
    case result of
      Ok -> "Ok.\n"
      Fail string correct wrong ->
        "Fail.\n" <>
        "Input:\n" <>
        string <>
        "\nSupposed result:\n" <>
        show correct <>
        "\nActual result:\n" <>
        show wrong

testcases :: [Test]
testcases =
  [ Case
    "Hello, world!"
    [ Paragraph "Hello, world!"
    ]
  , Case
    "* Welcome\n\
    \\n\
    \To this tutorial about Haskell."
    [ Heading 1 "Welcome"
    , Paragraph "To this tutorial about Haskell."
    ]
  , Case
    "Remember that multiple lines with no separation\n\
    \are grouped together to a single paragraph\n\
    \but list items remain separate.\n\
    \\n\
    \# Item 1 of a list\n\
    \# Item 2 of the same list"
    [ Paragraph "Remember that multiple lines with no separation are grouped together to a single paragraph but list items remain separate."
    , OrderedList
      [ "Item 1 of a list"
      , "Item 2 of the same list"
      ]
    ]
  , Case
    "* Compiling programs with ghc\n\
    \\n\
    \Running ghc invokes the Glasgow Haskell Compiler (GHC),\n\
    \and can be used to compile Haskell modules and programs into native\n\
    \executables and libraries.\n\
    \\n\
    \Create a new Haskell source file named hello.hs, and write\n\
    \the following code in it:\n\
    \\n\
    \> main = putStrLn \"Hello, Haskell!\"\n\
    \\n\
    \Now, we can compile the program by invoking ghc with the file name:\n\
    \\n\
    \> ➜ ghc hello.hs\n\
    \> [1 of 1] Compiling Main             ( hello.hs, hello.o )\n\
    \> Linking hello ...\n\
    \\n\
    \GHC created the following files:\n\
    \\n\
    \- hello.hi - Haskell interface file\n\
    \- hello.o - Object file, the output of the compiler before linking\n\
    \- hello (or hello.exe on Microsoft Windows) - A native runnable executable.\n\
    \\n\
    \GHC will produce an executable when the source file satisfies both conditions:\n\
    \\n\
    \# Defines the main function in the source file\n\
    \# Defines the module name to be Main, or does not have a module declaration\n\
    \\n\
    \Otherwise, it will only produce the .o and .hi files."
    [ Heading 1 "Compiling programs with ghc"
    , Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries."
    , Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:"
    , CodeBlock
      [ "main = putStrLn \"Hello, Haskell!\""
      ]
    , Paragraph "Now, we can compile the program by invoking ghc with the file name:"
    , CodeBlock
      [ "➜ ghc hello.hs"
      , "[1 of 1] Compiling Main             ( hello.hs, hello.o )"
      , "Linking hello ..."
      ]
    , Paragraph "GHC created the following files:"
    , UnorderedList
      [ "hello.hi - Haskell interface file"
      , "hello.o - Object file, the output of the compiler before linking"
      , "hello (or hello.exe on Microsoft Windows) - A native runnable executable."
      ]
    , Paragraph "GHC will produce an executable when the source file satisfies both conditions:"
    , OrderedList
      [ "Defines the main function in the source file"
      , "Defines the module name to be Main, or does not have a module declaration"
      ]
    , Paragraph "Otherwise, it will only produce the .o and .hi files."
    ]
  ]
