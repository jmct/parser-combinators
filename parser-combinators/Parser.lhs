We are going to build a parser combinator library from scratch. This is meant
to solidify your understanding of functions as first-class values and to
illustrate the common funcitonal programming pattern of a DSL-as-a-library.

> module Parser where
> import Data.List (isPrefixOf)
> import Data.Char (isSpace)

You'll notice that we don't import a lot, and nothing we import is outside of
`base`. Our goal is the simplest exposition, not the most efficient or
configurable library. So we limit ourselves to the Haskell Prelude and
a few functions from Data.Char and Data.List.

\section{Some definitons and concepts}

Our first step is to decide what a parser _is_. This turns out to be quite
concise in a language with first-class functions: A parser for something (which
we'll call `a`) is a function that takes an input `String` and returns all the
possible `a`s that could result, as well as any remaining input that was not
consumed. In Haskell types, this results in the following:

> type Parser a = String -> [(a,String)]

Let's write a parser that accepts boolen values. The parser will return `Bool`
values if the input stream has literally "True" or literally "False", and
return an empty list for any other value.

> bool1 :: Parser Bool
> bool1 s
>   | "True"  `isPrefixOf` s = [(True, drop 4 s)]
>   | "False" `isPrefixOf` s = [(False, drop 5 s)]
>   | otherwise              = []

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

> dropConsumed :: Int -> String -> String
> dropConsumed n = dropWhile isSpace . drop n

The function `dropConsumed` takes a an integer, removes the numer of characters
specified by the integer from the beginning of the `String` and then drops any
additional whitespace from the beginning of the remaining stream. This way our
parsers will never have whitespace at the beginning of their input stream.

> bool2 :: Parser Bool
> bool2 s
>   | "True"  `isPrefixOf` s = [(True, dropConsumed 4 s)]
>   | "False" `isPrefixOf` s = [(False, dropConsumed 5 s)]
>   | otherwise              = []

Now, as long as we ensure that the initial input stream did not start with
whitespace, we are able to make sure that intial whitespace is not an issue for
our parsers.

\section{Parsing Literals}

Let's try writing a parser that matches a given string exactly. This will be
a new function that takes the literal string we want to match, and returns
a parser for `String`.

< lit :: String -> Parser String

The `Parser` type synonym helps us being encapsulating the notion of a parser
into a single name, `Parser`, but we can expand it to see what's going on
underneath:

<         |----------------------------------- The literal we want to match
<         v                       v----------- The resulting value of the parse
< lit1 :: String -> String -> [(String, String)]
<                    ^                   ^---- The remaining input stream
<	             |------------------------ The initial input stream

That's a bunch of `String`s! Let's try writing a function with this type:

> lit1 :: String -> Parser String
> lit1 l s
>   | l == pref = [(l, dropConsumed len s)]
>   | otherwise = []
>  where
>    len = length l
>    pref = take len s

We don't always want literal parsing to return the raw `String` though, sometimes
we want to match a literal `String` but have it represent some other type. This is
easy to accomplish with some addtions to `lit1`.

> litWith :: String -> (String -> a) -> Parser a
> litWith l f s
>   | l == pref = [(f l, dropConsumed len s)]
>   | otherwise = []
>  where
>    len = length l
>    pref = take len s

Above, `litWith` allows the user to provide a way to convert a `String` into
the type they desire as the result of the parser. This is strictly more
general than `lit1` as we can define `lit1` as

< lit1 l s = litWith l id s

If parsing a known literal always results in the same value, we can just pass
a constant function to `litWith`. Giving us another useful function:

> litConst :: String -> a -> Parser a
> litConst l c = litWith l (const c)


For example, we can define a parser for "True" as follows

< true = litConst "True" True

`true` has the type `Parser Bool`, which is what we would expect. However,
we're pretty limited here as we can only parse known literal strings, but
without accepting any alternatives. If we want to write a `bool` parser in
terms of `litConst`, we need to be able to handle the possibility that the
input stream contains _either_ "True" or "False".

> (<|>) :: Parser a -> Parser a -> Parser a
> p1 <|> p2 = \s -> case p1 s of
>                     [] -> p2 s
>                     s  -> s
