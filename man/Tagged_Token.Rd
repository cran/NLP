\name{Tagged_Token}
\alias{Tagged_Token}
\alias{as.Tagged_Token}
\alias{is.Tagged_Token}
\alias{[.Tagged_Token}
%% \alias{[<-.Tagged_Token}
\alias{[[.Tagged_Token}
%% \alias{[[<-.Tagged_Token}
\alias{$<-.Tagged_Token}
\alias{as.data.frame.Tagged_Token}
\alias{as.list.Tagged_Token}
\alias{c.Tagged_Token}
\alias{duplicated.Tagged_Token}
\alias{format.Tagged_Token}
\alias{length.Tagged_Token}
\alias{names.Tagged_Token}
\alias{print.Tagged_Token}
\alias{unique.Tagged_Token}
\title{Tagged_Token objects}
\description{
  Creation and manipulation of tagged token objects.
}
\usage{
Tagged_Token(token, tag)
as.Tagged_Token(x)
is.Tagged_Token(x)
}
\arguments{
  \item{token, tag}{character vectors giving tokens and the
    corresponding tags.}
  \item{x}{an \R object.}
}
\details{
  A tagged token is a pair with \dQuote{slots} \sQuote{token} and
  \sQuote{tag}, giving the token and the corresponding tag.

  Tagged token objects provide sequences (allowing positional access) of
  single tagged tokens.  They have class \code{"Tagged_Token"}.

  Subscripting tagged token objects via \code{[} extracts subsets of
  tagged tokens; subscripting via \code{$} extracts character vectors
  with the sequence of values of the named slot.

  There are several additional methods for class \code{"Tagged_Token"}:
  \code{print()} and \code{format()} (which concatenate tokens and tags
  separated by \samp{/});
  \code{c()} combines tagged token objects (or objects coercible to
  these using \code{as.Tagged_Token()}), and
  \code{as.list()} and \code{as.data.frame()} coerce, respectively, to
  lists (of single tagged token objects) and data frames (with tagged
  tokens and slots corresponding to rows and columns).

  \code{Tagged_Token()} creates tagged token objects from the given
  sequences of tokens and tags, which must have the same length.

  \code{as.Tagged_Token()} coerces to tagged token objects, with a
  method for \code{\link{TextDocument}} objects using
  \code{\link{tagged_words}()}.

  \code{is.Tagged_Token()} tests whether an object inherits from class
  \code{"Tagged_Token"}.
}
\value{
  For \code{Tagged_Token()} and \code{as.Tagged_Token()}, a tagged token
  object (of class \code{"Tagged_Token"}).

  For \code{is.Tagged_Token()}, a logical.
}
