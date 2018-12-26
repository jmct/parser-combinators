% Parser Combinators
% JMCT
% December 2018

\section{Introduction}

Parser combinator libraries are a great example of the power of functional
programming. They demonstrate the ability of taking a few simple functions and
developing an entire API based on those simple functions and functions that
combine those functions in various ways.

As pointed out by Burge, parser combinators have the nice property that the
structure of the function for parsing to a structure closely resembles the
structure itself (though he didn't call them parser combinators). Take
for example a parser that accepts either "True" of "False".

< bool :: Parser String
< bool =  lit "False"
<     <|> lit "True"

which doesn't look too different from the definition of `Bool` itself:

< data Bool = False
<           | True

Because of the above, parser combinator libraries are often used as a
pedagogical tool for teaching intermediate functional programming. While the
resulting libraries are are often examplars of the clarity of functional
programming, they often contain space leaks and have terrible error messages.

In this tutorial series we start off by developing a parser combinator library
from scratch, assuming minimal Haskell exposure: the reader should be
comfortable with higher-order functions, and the simpler type-classes from the
`base` libraries, `Functor`, `Applicative`, `Monad` to start (while you're
expected to be familiar with these type-classes, we do not necessaritly expect
that you can immediately spot _when_ something is a `Functor`, we'll point it
out). Once we establish the shared vocabularyof the parser combinator library
we will iterate on the library, discussing the changes required to improve
performance and provide better error messages.

\section{Mark I}

\input{src/Chapter1}
