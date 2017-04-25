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
ghci> :t putStr
putStr :: String -> IO ()
```

Looking at the `putStr` method, you can see, that it takes a `String` as a first parameter and returns an `IO ()`.

When you work with Haskell, your coding workflow will be slightly different.
Instead of debugging, you will often use the [ghci][ghci] to test your functions.

# Types

With the `data` keyword you define new types in Haskell.
In [prelude][preludeBool] the `Bool` definition looks like.

```
data Bool = True | False
```

`True` and `False` are *value constructors.* 
Value constructors can have zero or more parameters.

```
data Person = SimplePerson String String
```

In this example a `Person` can be created using the value constructor `SimplePerson`.
The first parameter will be the first name, the second the last name.

```
ghci> :t SimplePerson 
SimplePerson :: String -> String -> Person
```

Creating a new `Person` in ghci will cause an error, if you don't bind it to a variable.

```
ghci> data Person = SimplePerson String String
ghci> SimplePerson "Jan" "Hake"

<interactive>:6:1:
    No instance for (Show Person) arising from a use of ‘print’
    In a stmt of an interactive GHCi command: print it
```

If you want to see, how a `Person` looks like, it must be able to show it self. 
Therefore a first [type class][typeClass] has to be introduced to the new `Person`.

```
ghci> data Person = SimplePerson String String deriving Show
ghci> SimplePerson "Jan" "Hake"
SimplePerson "Jan" "Hake"
```

In Haskell, every function is a [curried][currying] function. 
This means, every function in Haskell has exactly one parameter.
This is called [partial application][partialApplication] and looks like

```
ghci> let add a b =  a + b
ghci> :t add
add :: Num a => a -> a -> a
ghci> let addOne = add 1
ghci> :t addOne
addOne :: Num a => a -> a
ghci> addOne 10
11
```

Some new stuff here. 

The function signature `add :: Num a => a -> a -> a` says, that the `add` function uses a *type variable* `a`, which has to be an instance of the `Num` *type class*.
The `->` operator is right associative, so the signature can be written as `add :: Num a => a -> (a -> a)`.

You can build a list of `Person`s with the following expression.

```
ghci> map ($ "Hake") [SimplePerson "Jan", SimplePerson "Brother dear"]
[SimplePerson "Jan" "Hake",SimplePerson "Brother dear" "Hake"]

```

When you consider the `map` function

```
ghci> :t map
map :: (a -> b) -> [a] -> [b]
```

from the right site, you can see that `[b]` is the return type of the `map` function.
In this case it is a list of `Person`, so `b` is a `Person`.
the `[a]` is a list of `SimplePerson`is which takes a last name and returns a Person.
The signature is

```
ghci> :t SimplePerson "Jan"
SimplePerson "Jan" :: String -> Person
```

The `map` function applies the method 

```
ghci> :t ($ "Hake")
($ "Hake") :: ([Char] -> b) -> b
```

to the list of partial `SimplePerson`s and returns a list of `Person`.
<!--
# Type classes

Type classes are a kind of interface, a behaviour a *type* can have.

Looking at the definition of the *+* function

```
ghci> :t (+)
(+) :: Num a => a -> a -> a
```
-->

# Real world projects

There are several Haskell projects, which are really remarkable. 
Without any order these projects are worth looking into it.

## hakyll

This static site generator is used to generate this blog. 
Outgoing from its application, [hakyll][hakyll] is very io centric.
It is a good example to show, when and where to make pure or impure functions.
On hakylls homepage, you can find [several examples][hakyllExamples] how to build a static website.
It takes a lot advantage from the next project.

## pandoc

This swiss-army knife let you convert files between different markup formats.
There are several readers and writers available.
[Pandoc][pandoc] uses an internal AST which can be manipulated by [filters][pandocFilters].

```
Prelude> :m Text.Pandoc
Prelude Text.Pandoc> readMarkdown def "# Test"
Right (Pandoc (Meta {unMeta = fromList []}) [Header 1 ("test",[],[]) [Str "Test"]])
```

This litte example reads markdown with [default reader options][pandocDef]

[ghci]: https://wiki.haskell.org/GHC/GHCi
[hoogle]: https://www.haskell.org/hoogle/
[hackage]: http://hackage.haskell.org/
[hakyll]: https://jaspervdj.be/hakyll/
[hakyllExamples]: https://jaspervdj.be/hakyll/examples.html
[pandoc]: http://pandoc.org/
[pandocFilters]: http://pandoc.org/scripting.html
[preludeBool]: https://hackage.haskell.org/package/base-4.9.1.0/docs/Prelude.html#t:Bool
[typeClass]: http://learnyouahaskell.com/types-and-typeclasses
[currying]: https://en.wikipedia.org/wiki/Currying
[partialApplication]: https://wiki.haskell.org/Partial_application
[pandocDef]: http://hackage.haskell.org/package/pandoc-1.19.2.1/docs/Text-Pandoc-Options.html#v:def
