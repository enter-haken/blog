---
title: Haskell
---

Haskell is about types.
It is about functions and their composition.
It is about dividing pure and impure code.
The type system is one of the best in class.
A frequent statement says: "If it compiles, it runs". 

My current development stack uses the CLI [ghci][ghci] and vim as an editor.
I use [Hoogle][hoogle] to search for types or type definitions, and use [Hackage][hackage] to dive into the libraries frequently used.

<!--more-->

# Start

The Haskell "Hello World" looks like

```
main :: IO ()
main = putStr "Hello World"
```

This file compiles with `ghc Main.hs` and will output `Hello World`.
The `Main.hs` file contains a definition of a method `main` which returns an `IO ()`.
The function definition header is not mandatory, if Haskell can infer the signature during compile time. 
It is best practice to add a function signature, even it is not necessary.

```
ghci>:t putStr
putStr :: String -> IO ()
```

Looking at the `putStr` method, you can see, that it takes a `String` as a first parameter and returns an `IO ()`.

When you work with Haskell, your coding workflow will be slightly different.
Instead of debugging, you will often use the [ghci][ghci] to test your functions.

# Real world projects

There are several Haskell projects, which are really remarkable. 
Without any order these projects are worth looking into it.

## hakyll

This static site generator is used to generate this blog. 
Outgoing from its application, [hakyll][hakyll] is very io centric.
It is a good example to show, when and where to make pure or impure functions.
On hakylls homepage, you can find [several examples][hakyllExamples] how to build a static website.
It take a lot advantage from the next project.

## pandoc

This swiss-army knife let you convert files between different markup formats.
There are several readers and writers available.
[Pandoc][pandoc] uses an internal AST which can be manipulated by [filters][pandocFilters].

[ghci]: https://wiki.haskell.org/GHC/GHCi
[hoogle]: https://www.haskell.org/hoogle/
[hackage]: http://hackage.haskell.org/
[hakyll]: https://jaspervdj.be/hakyll/
[hakyllExamples]: https://jaspervdj.be/hakyll/examples.html
[pandoc]: http://pandoc.org/
[pandocFilters]: http://pandoc.org/scripting.html
