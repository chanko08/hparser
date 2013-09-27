hparser
=======

A Parsec inspired simple parser combinator library written in Haskell

This repository is the code that is related to the following blog posts on [alexbechanko.com](alexbechanko.com):


* [Create a Monadic Parser](alexbechanko.com/create-monadic-parser.html)
* [Create an Applicative Parser](alexbechanko.com/create-applicative-parser.html)
* [Using Alternative in the Parser](alexbechanko.com/create-alternative-parser.html)

The types and typeclasses that get used by all three posts are defined in the `Parser.hs` file. The code relevant to the monadic parser post is in `Mparser.hs`. For the applicative parser code is in `AParser.hs`. Finally, the alternative typeclass is used in `AltParser.hs`.


## To Build
Hopefully you are somewhat familiar with Haskell, as this repository uses `cabal` as the build system. Assuming you have `cabal` installed, run the following within the repository:

```
cabal configure
cabal build
```

This will create a `dist` directory that holds the compiled versions of all three parsers.