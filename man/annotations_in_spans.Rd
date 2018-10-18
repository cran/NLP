\name{annotations_in_spans}
\alias{annotations_in_spans}
\title{Annotations contained in character spans}
\description{
  Extract annotations contained in character spans.
}
\usage{
annotations_in_spans(x, y)
}
\arguments{
  \item{x}{an \code{\link{Annotation}} object.}
  \item{y}{a \code{\link{Span}} object, or something coercible to this
    (such as an \code{\link{Annotation}} object).}
}
\value{
  A list with elements the annotations in \code{x} with character spans
  contained in the respective elements of \code{y}.
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

## Annotation for word tokens according to sentence:
annotations_in_spans(a[a$type == "word"], a[a$type == "sentence"])
}
