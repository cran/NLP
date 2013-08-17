\name{Span}
\alias{Span}
\alias{as.Span}
\alias{is.Span}
\alias{[.Span}
%% \alias{[<-.Span}
\alias{[[.Span}
%% \alias{[[<-.Span}
\alias{$<-.Span}
\alias{Ops.Span}
\alias{as.data.frame.Span}
\alias{as.list.Span}
\alias{c.Span}
\alias{duplicated.Span}
\alias{format.Span}
\alias{length.Span}
\alias{names.Span}
\alias{print.Span}
\alias{unique.Span}
\title{Span objects}
\description{
  Creation and manipulation of span objects.
}
\usage{
Span(start, end)
as.Span(x)
is.Span(x)
}
\arguments{
  \item{start, end}{integer vectors giving the start and end positions
    of the spans.}
  \item{x}{an \R object.}
}
\details{
  A single span is a pair with \dQuote{slots} \sQuote{start} and
  \sQuote{end}, giving the start and end positions of the span.

  Span objects provide sequences (allowing positional access) of single
  spans.  They have class \code{"Span"}.  Span objects can be coerced to
  annotation objects via \code{\link{as.Annotation}} (which of course is
  only appropriate provided that the spans are character spans of the
  natural language text being annotated), and annotation objects can be
  coerced to span objects via \code{as.Span} (giving the character spans
  of the annotations).

  Subscripting span objects via \code{[} extracts subsets of spans;
  subscripting via \code{$} extracts integer vectors with the sequence
  of values of the named slot.

  There are several additional methods for class \code{"Span"}:
  \code{print} and \code{format}; \code{c} combines spans, and
  \code{as.list} and \code{as.data.frame} coerce to lists (of single
  span objects) and data frames (with spans and slots corresponding to
  rows and columns).  Finally, one can add a scalar and a span object
  (resulting in shifting the start and end positions by the scalar).

  \code{Span()} creates span objects from the given sequence of start
  and end positions, which must have the same length.

  \code{as.Span()} coerces to span objects, with a method for annotation
  objects.

  \code{is.Span()} tests whether an object inherits from class
  \code{"Span"} (and hence returns \code{TRUE} for both span and
  annotation objects).
}
\value{
  For \code{Span()} and \code{as.Span()}, a span object (of class
  \code{"Span"}).

  For \code{is.Span()}, a logical.
}
