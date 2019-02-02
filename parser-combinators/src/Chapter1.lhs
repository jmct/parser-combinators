We are going to build a parser combinator library from scratch. This is meant
to solidify your understanding of functions as first-class values and to
illustrate the common funcitonal programming pattern of a DSL-as-a-library.

> module Chapter1 where
> import Control.Applicative
> import Data.List (isPrefixOf)
> import Data.Char (isSpace, isDigit, isAlpha, toLower
>                  ,isLower, isAlphaNum, isPunctuation)

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
to combine parsers. Let's imagine part of a grammar for arithmetic
expressions:

```
expression : mult | add

mult : expression '*' expression
add  : expression '+' expression
```

The above grammar demonstrates two of the fundamental building blocks for
parsers: choice (an expression can take the form of a `mult` _or_ an `add`) and
sequence. The next two sections will deal with each of these in turn. One
interesting fact is that both are enabled by defining an `Applicative` instance
for `Parser`.

\subsection{Choice}

Choice can be represented by the `Alternative` type-class, which has the following
definition

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

The final thing that we must satisfy in order to define an `Alternative`
instance for `Parser` is that `Parser` must also have an `Applicative`
instance, and `Applicative` requires `Functor`, so let's start there:

> instance Functor Parser where
>   fmap f (P p) = P $ \s -> [ (f x, cs) | (x, cs) <- p s ]

The `Functor` instance let's us `map` over a parser, this is useful even without
the other instance definitions:

```
ghci> parse (fmap toLower item) "The Beatles"
[('t',"he Beatles")]
```

Now for `Applicative`. Remember that the type of `(<*>)`, when we specialize to `Parser`, is
`Parser (a -> b) -> Parser a -> Parser b`.

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

This will be very useful when we deal with sequence in the next section.

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

Next, we address the sequencing of `Parser`s. The good news is that we already
have all the machinery in place!

\subsection{Sequence}

The sequencing of parsers also depends on our `Applicative` instance,
recall our definition of `<*>`:

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
the desired string and succeed only when the begining of the input stream
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

In addition to these three primitive parsers, we'll definitely want to be
be able to to `map` over parsers. This calls for a `Functor` instance:

Let's write a parser that accepts boolean values. The parser will return `Bool`
values if the input stream has literally "True" or literally "False", and
return an empty list for any other value.

< bool1 :: Parser Bool
< bool1 s
<   | "True"  `isPrefixOf` s = [(True, drop 4 s)]
<   | "False" `isPrefixOf` s = [(False, drop 5 s)]
<   | otherwise              = []

We can test this on a few inputs in `ghci`. First we'll try on inputs that the
parser should accept:

< *Parser> bool1 "True lolwut"
< [(True," lolwut")]
<
< *Parser> bool1 "False yee"
< [(False," yee")]

Okay, that makes sense, the result is the appropriate `Bool` value that
matches the input. What about an input stream that shouldn't match?

< *Parser> bool1 "Falsify yee"
< []

So far, so good. "Falsify" is not a value that our parser should accept so we
get an empty set of resulting parses. We can think of the empty list as the
signal that our parse failed. There is a common refrain, due to Philip Wadler:
"Turning a failure into a list of successes", since the parser returns a list
of possible parses, the empty list is what represents a failed parse.

Let's try one last thing, a valid `Bool` but at the beginning we have some
whitespace:

< *Parser> bool1 " True yee"
< []

This makes sense with how we've defined the `bool1` parser, it doesn't try to
strip any whitespace, so this isn't one of the valid parses. Addtionally,
you'll notice that in the tests of `bool1` that did accept their input, the
remaining input stream in the result had whitespace at the beginning. Often we
do lexical analysis before parsing, so that this isn't a concern for the
parser. However, it's not hard to write some helper functions in order to
facilitate dealing with extraneous whitespace.

> dropConsumed :: String -> String -> String
> dropConsumed s = dropWhile isSpace . drop (length s)

The function `dropConsumed` takes a `String`, removes the number of characters
specified by the length of the string from the beginning of the input stream
and then drops any additional whitespace from the beginning of the remaining
stream. This way our parsers will never have whitespace at the beginning of
their input stream.

< bool2 :: Parser Bool
< bool2 s
<   | "True"  `isPrefixOf` s = [(True, dropConsumed 4 s)]
<   | "False" `isPrefixOf` s = [(False, dropConsumed 5 s)]
<   | otherwise              = []

It will also be useful for us to define a function that determines whether
a character is 'separator' or not. For this tutorial we define a separator
as being either a literal separator (like `,`, or `;`) or white space.

> isSep :: Char -> Bool
> isSep c = isSpace c || isPunctuation c

Now, as long as we ensure that the initial input stream did not start with
whitespace, we are able to make sure that intial whitespace is not an issue for
our parsers.


\subsection{Parsing Literals}

Let's try writing a parser that matches a given string exactly. This will be
a new function that takes the literal string we want to match, and returns
a parser for `String`.

< lit :: String -> Parser String

The `Parser` type synonym helps us being encapsulating the notion of a parser
into a single name, `Parser`, but we can expand it to see what's going on
underneath:

< --     |----------------------------------- The literal we want to match
< --     v                       v----------- The resulting value of the parse
< lit :: String -> String -> [(String, String)]
< --                ^                   ^---- The remaining input stream
< --                |------------------------ The initial input stream

That's a bunch of `String`s! Let's try writing a function with this type:

< lit :: String -> Parser String
< lit l s
<   | l == pref = [(l, dropConsumed l s)]
<   | otherwise = []
<  where
<    pref = take len s

We don't always want literal parsing to return the raw `String` though, sometimes
we want to match a literal `String` but have it represent some other type. This is
easy to accomplish with some addtions to `lit`.

< litWith :: String -> (String -> a) -> Parser a
< litWith l f s
<   | l == pref = [(f l, dropConsumed l s)]
<   | otherwise = []
<  where
<    pref = take (length l) s

Above, `litWith` allows the user to provide a way to convert a `String` into
the type they desire as the result of the parser. This is strictly more
general than `lit` as we can define `lit` as

< lit :: String -> Parser String
< lit l s = litWith l id s


If parsing a known literal always results in the same value, we can just pass
a constant function to `litWith`. Giving us another useful function:

< litConst :: String -> a -> Parser a
< litConst l c = litWith l (const c)


For example, we can define a parser for "True" as follows

< true = litConst "True" True

\subsection{Parsing Variations of Strings}

Often you will need to parser different types of strings for your grammar, all
lowercase strings, or strings that cannot begin with a digit, for example.

A simple illustration of this is a parser for strings that only include
alphabetical characters.

< alpha :: Parser String
< alpha s
<   | null word        = [] -- This is very important!
<   | all isAlpha word = [(word, dropConsumed word s)]
<   | otherwise        = []
<  where
<     word = takeWhile (not . isSep) s

Many languages have rules about the capitalization of variables. Haskell, for
example, requires that variables begin in lowercase letters. A simpler version
of this is defined below:

< variable :: Parser String
< variable s
<   | isCamelCase word = [(word, dropConsumed word s)]
<   | otherwise        = []
<  where
<     word = takeWhile (not . isSep) s
<     isCamelCase (c:cs) = isLower c && all isAlphaNum cs
<     isCamelCase _      = False


\subsection{Combining Parsers}

\subsubsection{Choice}

`true` has the type `Parser Bool`, which is what we would expect. However,
we're pretty limited here as we can only parse known literal strings, but
without accepting any alternatives. If we want to write a `bool` parser in
terms of `litConst`, we need to be able to handle the possibility that the
input stream should parse if it starts with "True" _or_ "False". This is
sometimes called the 'choice' operator.

< choice :: Parser a -> Parser a -> Parser a
< choice p1 p2 = \s -> case p1 s of
<                        [] -> p2 s
<                        s' -> s'

`choice` attempts to parse the input stream with `p1` and if that fails (via
returning an empty list), it returns the result of passing the input stream to
`p2`. This is fine, in that it does a sensible thing, but it's not exactly what
we want.

Remember that our notion of parsing allows for _multiple resulting parses_,
hence returning a list of results. Looking back at `choice`, we can see that
if `p1` were to succeed, we would never see the possible parses from `p2`,
which would violate this notion of all possible parses. The solution is
to be more generous with what we return:

< (<|>) :: Parser a -> Parser a -> Parser a
< p1 <|> p2 = \s -> p1 s ++ p2 s

The definition of `<|>` is more of what we'd expect from a parser combinator
library. In fact, most parser combinator libraries provide a function called
`<|>` in their API, though the definitions might differ due to implementation
concerns, their meanings tend to align.

We can naturally extend this idea to taking a list of parsers and performing
a choice over all of them, and we can represent that as a fold:

< oneOf :: [Parser a] -> Parser a
< oneOf ps = foldr (<|>) failure ps

\subsubsection{Sequence}

Now that we can parse alternatives, let's figure out the dual notion of a parser
that has to accept two sub-parses. You can think of this as 'sequence' or 'and',
we're going to call it `andThen`, you'll see why in a moment.

< andThen :: Parser a -> Parser b -> Parser (a,b)
< andThen p1 p2 = \s -> [((x,y), s'') | (x, s')  <- p1 s
<                                     , (y, s'') <- p2 s']

For those that may not be familiar with list comprehensions, let's break this
down, part by part. Once again, it is important to note that parsers result in
a _lists_ of parses. Therefore `p1 s` returns a list of pairs, the result of
the parse and the remaining input stream. With that in mind, `(x, s')  <- p1 s`
can be read as 'for each `(x, s')` that is generated by `p1 s`. Here, `x` is
the result of the parser `p1` and `s'` is the remaining input stream. This
part of the list comprehension, something of the form `r <- g`, is known
as a _generator_.

The definition of `andThen` uses a second generator, `(y, s'') <- p2 s'`. This
second generator _uses part of the result of the first generator_. By relying
on the results of the first parse, this generator can only happen 'after' the
first. That solves one part of problem: sequencing. However, we also want to
ensure that the overall parse is only valid if _both_ parsers are successful.

List comprehensions help us here as well: If either generator results in an
empty list, the entire list comprehension results in an empty list. This
perfectly aligns with the behaviour we expect form our `andThen` parser. If
either parser fails (return the empty list) the entire parser fails.

Lastly, we have the expression to the left of the `|`, `((x,y), s'')`. This
states that the final result of the list comprehension is a nested pair where
the first element is the pair of `x` and `y` from our generators, and the
second element is `s''` which is the remaining input stream returned from our
_second_ generator. This enforces that the overall parser starts from where the
`p2` left off.

The user of `andThen` often knows how they want to combine the resulting pair.
For this reason, it's useful to have a function similar to `andThen` but with
an additional parameter that specifies the desired combination:

< andThenWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
< andThenWith f p1 p2 = \s -> [(f x y, s'') | (x, s')  <- p1 s
<                                             , (y, s'') <- p2 s']


One simple example of `andThenWith` would be parsing something like
a configuration file. You may have to parser fields and theirs
values, such as "city: York". This could be accomplished
with

< cityField :: Parser String
< cityField = andThenWith (\x y -> y) (lit "city:") alpha

Testing this out on a few examples shows that it matches our intuitions:

< *Chapter1> let cityField = andThenWith (flip const) (lit "city:") alpha
< *Chapter1> cityField "city: York"
< [("York","")]
< *Chapter1> cityField "city: York\nname: Jose"
< [("York","name: Jose")]
< *Chapter1> cityField "cities: York"
< []


\subsubsection{Repeated parsers and optional parsers}

Often grammars allow for repeated sequences of the same pattern (take Our
`cityField` parser above requires that the `city` field is only one word, but
we know that many cities have multiple words in their name. This necessitates
the ability to parse a pattern multiple times, we'll call this `oneOrMany`:

< oneOrMany :: Parser a -> Parser [a]
< oneOrMany p = andThenWith (:) p (zeroOrMore p)

As you can see, in order to parser `oneOrMany` of some parser, `p`, you require
a successful parse of `p` followed by `zeroOrMore` parses of the same `p`. Now
we can define `zerOrMore`:

< zeroOrMore :: Parser a -> Parser [a]
< zeroOrMore p = oneOrMany p <|> succeed []

`zeroOrMore` is composed of two choices. If there is at least one pattern to
parse then `oneOrMany` would succeed!  Of course, `zeroOrMore` should be
successful if there are zero parses, which is why the definition includes the
choice of `succeed []`.

A degenerate case of `zeroOrMore` would be `zeroOrOne`, or alternatively `option`:

< parseWith :: (a -> b) -> Parser a -> Parser b
< parseWith f p = \i -> [(f x, s) | (x, s) <- p i]

< option :: Parser a -> Parser (Maybe a)
< option p = parseWith Just p <|> succeed Nothing

\subsubsection{Generalized Sequence}

The `andThen` function defined above is very useful, but it has one major
downside: it always returns a pair for it's result, requiring the user of the
function to deconstruct or manipulate the pair when they actually want some
other, non tuple, combination of the two parsers.

There are several ways of solving this problem, the one we are going to take
first is to keep `andThen` as it is, but provide a nice way of handling its
return value that makes it easy for users of the function to change the result
of a parser.  Take for example a grammar that specifies that each `if` is
followed by an expression (note, we haven't defined the parser `expr` yet):

< lit "if" `andThen` expr :: Parser (String, Expr)

It is unlikely that the person parsing such a thing will actually need the
string `if`, so we want a function that looks something like the following

< f1 :: Parser (a,b) -> Parser b
< f1 p = \s -> case p s of
<                rs -> map sndOfFst rs
<   where
<     -- For each of the possible parses,
<     -- take only the second part of the result pair
<     sndOfFst (x, s') = (snd x, s')

This solves the specific problem of ignoring the first part (by returning only
the `snd` of the resulting tuple of the parse), but it's clearly a pattern
we'll want to reuse. The code for ignoring the second element in a tuple will
look almost identical, replacing the `snd` with a `fst`, for instance. So let's
generalize the function:

< f2 :: ((a,b) -> c) -> Parser (a,b) -> Parser c
< f2 f p = \s -> case p s of
<                  rs -> map fOfFst rs
<   where
<     -- For each of the possible parses,
<     -- apply the provided function to each result
<     fOfFst (x, s') = (f x, s')

Alright, this is nice. Provided with a function that can turn a pair into some
desired type, we can pass that function and a `Parser` that returns a pair and
get back a `Parser` that returns our desired type. However, in the process of
generalizing from `f1` to `f2` we don't actually use the fact that the parser
passed to `f2` returns a pair, it's only the type that requires this, nothing
about the structure of the code itself. That's another hint to generalize in
that direction:


< f3 :: (a -> b) -> Parser a -> Parser b
< f3 f p = \s -> case p s of
<                  rs -> map fOfFst rs
<   where
<     -- For each of the possible parses,
<     -- apply the provided function to each result
<     fOfFst (x, s') = (f x, s')

Notice that the code didn't change at all for `f3`, so if we hadn't provided a
type signature to `f2` the compiler would have inferred the type we gave `f3`!
If you squint a bit this looks a lot like the `map` function, but instead of
`[a]` and `[b]` we have `Parser a` and `Parser b`. This pattern is exactly that
which the `Functor` type-class is made to address, which means that `Parser`
is a `Functor`:

< instance Functor Parser where
<   fmap f p = \s -> case p s of
<                      rs -> map fOfFst rs
<     where
<       fOfFst (x, s') = (f x, s')

