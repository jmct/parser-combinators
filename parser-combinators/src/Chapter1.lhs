We are going to build a parser combinator library from scratch. This is meant
to solidify your understanding of functions as first-class values and to
illustrate the common funcitonal programming pattern of a DSL-as-a-library.

> module Chapter1 where
> import Control.Applicative
> import Data.List (isPrefixOf)
> import Data.Char (isSpace, isDigit, isAlpha, toLower
>                  ,isLower, isAlphaNum, isPunctuation
>                  ,isUpper, isLetter)

You'll notice that we don't import a lot, and nothing we import is outside of
`base`. Our goal is the simplest exposition, not the most efficient or
configurable library. So we limit ourselves to the Haskell Prelude and
a few functions from Data.Char and Data.List.

Because this is written for intermediate Haskellers, we're going to motivate
many of the choices we make with examples. In particular, we take a different
approach than many other parser combinator tutorial, instead of starting off
with the type definition for parsers and immediately defining all sorts of
type-class instances, we motivate each type-class instance with an example that
is difficult to accomplish without the instance declaration.  This may be
boring for those of you that are very comfortable with many of the type-classes
we'll use, in which case you may wish to skim this chapter quickly.

\subsection{Some definitons and concepts}

Our first step is to decide what a parser _is_. This turns out to be quite
concise in a language with first-class functions: A parser for something (which
we'll call `a`) is a function that takes an input `String` and returns all the
possible `a`s that could result, as well as any remaining input that was not
consumed. In Haskell types, this results in the following:

< type Parser a = String -> [(a,String)]

Because we will want to define various type-class instances for `Parser`s,
we will wrap the definition in a `newtype`:


> newtype Parser a = P { parse :: String -> [(a,String)] }

Now let's write three of the most trivial parser: one that always fails,
one that always succeeds with a default value, and one that always consumes
one character of the input stream.

Since our notion of success is a list of parses, it's very easy to define the
parser that represents failure in all cases, regardless of the input stream:

> failure :: Parser a
> failure = P $ \s -> []

Almost as simple as the definition of `failure`, is the notion of trivial
success. Unlike `failure`, `succeed` takes an argument because it's impossible
to come up with something of _any_ type (the `a` in `Parser a`), so `success`
takes the value that you'd like to have represent a success.

> succeed :: a -> Parser a
> succeed x = P $ \s -> [(x, s)]

The final of the three trivial parsers is the one that always consumes one
character of its input:

> item :: Parser Char
> item = P $ \s -> case s of
>                    []   -> []
>                    c:cs -> [(c,cs)]

Let's use each of these primitive parsers to see if it matches your intuition:

```
ghci> parse failure "The Beatles"
[]
ghci> parse (succeed 3.14) "The Beatles"
[(3.14,"The Beatles")]
ghci> parse (succeed "Billy Preston") "The Beatles"
[("Billy Preston","The Beatles")]
ghci> parse item "The Beatles"
[('T',"he Beatles")]
```

One step up from these primitive parsers is a parser that matches some
desired input:

> sat :: (Char -> Bool) -> Parser Char
> sat p = P $ \s -> [ (c, cs) | (c,cs) <- parse item s, p c ]

Using `sat` we can write a parser that only accepts a specific character from
the input stream:

> char :: Char -> Parser Char
> char c = sat (== c)

Another useful parser is one that accepts decimal digits:

> digit :: Parser Char
> digit = sat (`elem` "0123456789")

We return the literal character and not the numeric value because it is up to
the consumer of this function to decide how to use the fact that the character
is a digit.

Now we're getting somewhere! We can actually use these parsers to accept or
reject inputs:

```
ghci> parse (char 'A') "ABBA"
[('A',"BBA")]
ghci> parse digit "1975"
[('1',"975")]
ghci> parse (char 'A') "Queen"
[]
```

Now that we have some primitive parsers, let's consider how we might want
to combine parsers. Let's imagine part of a grammar for a subset arithmetic
expressions (precedence is not handled by the grammar):

```
expression :  number | add | sub | '(' expression ')'

add : expression '+' expression
sub : expression '-' expression
```

The above grammar demonstrates two of the fundamental building blocks for
parsers: choice (an expression can take the form of a `sub` _or_ an `add`) and
sequence.

A good type-class to be familiar with is the `Alternative` type-class, that is
used to abstract over types that can present a choice between two alternatives.
It has the following minimal definition:

```{.haskell}
class Applicative f => Alternative (f :: * -> *) where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some  :: f a -> f [a]
  many  :: f a -> f [a]
  {-# MINIMAL empty, (<|>) #-}
```

The `MINIMAL` pragma tells us that we only need to define `empty` and `<|>` in
order to be a valid instance. While there is no way to express it in the
Haskell definition, `(<|>)` must be an associative binary operator and `empty`
must act as the identity such that the following holds:

```
empty <|> alt   === alt
alt   <|> empty === alt
```

As we will see shortly, the instances required in order to define an
`Alternative` instance for `Parser` (`Applicative`, and therefore also
`Functor`) will also provide us with a way to sequence `Parser`s.

Let us start with the first class in the chain, `Functor`:

> instance Functor Parser where
>   fmap f (P p) = P $ \s -> [ (f x, cs) | (x, cs) <- p s ]

The `Functor` instance let's us `map` over a parser, this is useful even
without the other instance definitions:

```
ghci> parse (fmap toLower item) "The Beatles"
[('t',"he Beatles")]
```

Now for `Applicative`. Remember that the type of `(<*>)`, when we specialize to
`Parser`, is `Parser (a -> b) -> Parser a -> Parser b`.

> instance Applicative Parser where
>   pure = succeed
>   p1 <*> p2 = P $ \s -> [ (f x', cs') | (f , cs) <- parse p1 s
>                                       , (x', cs') <- parse p2 cs ]

You may wonder how a parser can return function, and while there are
a few ways, the most common case is that you partially apply a function
using `fmap`. You can try the following at the repl:

```
ghci> let pLess = fmap (<) item
ghci> :t pLess
pLess :: Parser (Char -> Bool)
```

With the `Applicative` instance in hand, we are now ready to discuss the
sequencing of parsers.

\subsection{Sequence}

The sequencing of parsers is achieved via our `Applicative` instance, recall
our definition of `<*>`:

```{.haskell}
  p1 <*> p2 = P $ \s -> [ (f x', cs') | (f , cs) <- parse p1 s
                                      , (x', cs') <- parse p2 cs ]
```

The second generator in our list comprehension (`(x', cs') <- parse x cs`)
depends explicitly on the results of our first generator, `cs`. This dependency
enforces a sequencing of the parsers, `p1` must decide how much of the input
stream it will consume before we can provide the rest of the input stream to
`p2`.

This means that `<*>` can be used as a sequencing operator, but with the caveat
that its first argument must be a `Parser` that results in a function. As we
saw earlier, this can be accomplished by using `fmap` to partially apply a
function to a parser's result. Say we wanted to combine two parsers and have a
parser that returns a pair:
 
> pair :: Parser a -> Parser b -> Parser (a,b)
> pair p q = (,) <$> p <*> q

The infix `fmap` (`<$>`) has higher precedence than `<*>`, so the definition
could be written as `((,) <$> p) <*> q` without changing its meaning.

Using this pattern we can now define a parser for literal strings, which takes
the desired string and succeed only when the beginning of the input stream
matches that string exactly. This is exactly the same as a sequence of `char`
parsers, one for each character in the desired string literal:

> string :: String -> Parser String
> string ""     = succeed ""
> string (c:cs) = (:) <$> char c <*> string cs

```
ghci> parse (string "The Beatles") "The Beatles"
[("The Beatles","")]
ghci> parse (string "The Beatles") "The Kinks"
[]
ghci> parse (string "The Beatles") "The Beatles and The Kinks"
[("The Beatles"," and The Kinks")]
```

There are also the functions `*>` and `<*`, which sequence the parsers but
ignore the results of the first (`*>`) or the second (`<*`) parser,
respectively.

```
ghci> parse (string "The " *> string "Beatles") "The Beatles"
[("Beatles","")]
ghci> parse (string "Ringo " <* string "Starr") "Ringo Starr"
[("Ringo ","")]
```

\subsection{Choice}

With out `Applicative` instance in place we can now properly define our
`Alternative` instance in order to get choice:

> instance Alternative Parser where
>   empty = failure
>   (P p1) <|> (P p2) = P $ \s -> p1 s ++ p2 s

There are a few valid definitions of `<|>`, the one we have chosen above is the
'traditional' definition, and most general (it allows more grammars). However,
these options can have profound consequences on performance and error messages,
so the definition of `<|>` is something that we will return to a few times
throughout this document.

With our `Alternative` instance in hand, we can now make parsers that induce a
choice. Remembering our `digit` parser from before:

> hexDigit :: Parser Char
> hexDigit = digit <|> sat (`elem` "abcdefABCDEF")

\subsection{Repeated Patterns}

With sequence and choice in hand, we can now define a lot of useful
combinators. The `Alternative` type-class provides the following functions
that aid us in defining more sophisticated parsers:

```{.haskell}
  some  :: f a -> f [a]
  many  :: f a -> f [a]
```

The function `some` applies the computation 1 or more times, failing completely
if the computation does not apply at least once. Similarly, `many` applies the
computation 0 or more times. The difference being that the overall computation
is still considered a success if the computation does not apply once.

It is enlightening to try using `many` and `some` on a few simple examples. Say
you wanted to parse variables in a grammar that only required that variable names
be composed of letters:

```
ghci> let var = many (sat isLetter)
ghci> :t var
var :: Parser [Char]
ghci> parse var "index"
[("index",""),("inde","x"),("ind","ex"),("in","dex"),("i","ndex"),("","index")]
```

The reasoning here is that our definition of `<|>` is inclusive. So at every
point where the parser must make a choice between two parsers that would both
succeed, we get both results! This allows our parser to be very flexible, but
also has significant downsides for performance, notice that one of the results
did not consumer any input, so the parser is holding on the he entire input
stream!

In all of our examples so far, we have had to be careful about where
we put whitespace in our `string` parsers and input streams. We can
define combinators that allow us to not worry about such things:

> token :: Parser a -> Parser a
> token p = p <* space
>
> space :: Parser String
> space = many (sat isSpace)
>
> lit :: String -> Parser String
> lit = token . string

We can re-do some of our earlier examples using `lit` in order to avoid
worrying about whitespace:

```
ghci> parse (lit "Ringo" <* lit "Starr") "Ringo Starr"
[("Ringo","")]
ghci> parse (lit "The" *> lit "Beatles") "The Beatles"
[("Beatles","")]
```

\subsection{A Simple Example}

Earlier, we used an example of a simple grammar to motivate choice and sequence:

```
expression :  number | add | sub | '(' expression ')'

add : expression '+' expression
sub : expression '-' expression
```

We can now try to write a parser that accepts this grammar. First, let us
define the structure that will represent a parsed expression:

> data Expr = Expr :+: Expr
>           | Expr :-: Expr
>           | Number Int
>   deriving Show

We do not need an explicit constructor for the bracketed expression, because
the bracketing is implicit in the tree structure of the data-type. That being
said it could be useful for error messages if it was desired.

Now on to the parser:

> expression :: Parser Expr
> expression =  number
>           <|> addition
>           <|> subtraction
>           <|> lit "(" *> expression <* lit ")"
>
> addition :: Parser Expr
> addition = (:+:) <$> expression <*> (lit "+" *> expression)
>
> subtraction :: Parser Expr
> subtraction = (:-:) <$> expression <*> (lit "-" *> expression)
>
> number :: Parser Expr
> number = Number . read <$> token (some (sat isDigit))


```
ghci> parse expression "4"
[(Number 4,"")*** Exception: stack overflow
ghci> parse addition  "4 + 5"
[(Number 4 :+: Number 5,"")*** Exception: stack overflow
ghci> parse expression "4 + 4"
[(Number 4,"+ 4"),(Number 4," + 4"),(Number 4 :+: Number 4,"")*** Exception: stack overflow
ghci> takeLongest p i = head [ res | (res, "") <- parse p i]
ghci> takeLongest expression "4 + 4"
Number 4 :+: Number 4
ghci> 
```
