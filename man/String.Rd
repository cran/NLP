\name{String}
\alias{String}
\alias{as.String}
\alias{is.String}
\title{String objects}
\description{
  Creation and manipulation of string objects.
}
\usage{
String(x)
as.String(x)
is.String(x)
}
\arguments{
  \item{x}{a character vector with the appropriate encoding information
    for \code{String}; an arbitrary \R object otherwise.
  }
}
\details{
  String objects provide character strings encoded in UTF-8 with class
  \code{"String"}, which currently has a useful \code{[} subscript
  method: with indices \code{i} and \code{j} of length one, this gives a
  String object with the substring starting at the position given by
  \code{i} and ending at the position given by \code{j}; subscripting
  with a single index giving an object inheriting from class
  \code{"\link{Span}"} or a list of such objects returns a character
  vector of substrings with the respective spans, or a list thereof.

  Further methods may be added in the future.

  \code{String} creates a string object from a given character vector,
  taking the first element of the vector and converting it to UTF-8
  encoding.

  \code{as.String} coerces to a string object by calling \code{String}
  on the result of the \code{\link{toString}} generic.

  \code{is.String} tests whether an object inherits from class
  \code{"String"}.
}
\value{
  For \code{String} and \code{as.String}, a string object (of class
  \code{"String"}).

  For \code{is.String}, a logical.
}
\examples{
## A simple text.
s <- String("  First sentence.  Second sentence.  ")
##           ****5****0****5****0****5****0****5**

## Basic sentence and word token annotation for the text.
a <- c(Annotation(1 : 2,
                  rep.int("sentence", 2L),
                  c( 3L, 20L),
                  c(17L, 35L)),
       Annotation(3 : 6,
                  rep.int("word", 4L),
                  c( 3L,  9L, 20L, 27L),
                  c( 7L, 16L, 25L, 34L)))

## All word tokens (by subscripting with an annotation object):
s[a[a$type == "word"]]
## Word tokens according to sentence (by subscripting with a list of
## annotation objects):
s[annotations_in_spans(a[a$type == "word"], a[a$type == "sentence"])]
}
