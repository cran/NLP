\name{generics}
\alias{content}
\alias{content<-}
\alias{meta}
\alias{meta<-}
\title{Access or Modify Content or Metadata}
\description{
  Access or modify the content or metadata of \R objects.
}
\usage{
content(x)
content(x) <- value
meta(x, tag = NULL, ...)
meta(x, tag = NULL, ...) <- value
}
\arguments{
  \item{x}{an \R object.}
  \item{value}{a suitable \R object.}
  \item{tag}{a character string or \code{NULL} (default), indicating to
    return the single metadata value for the given tag, or all metadata
    tag/value pairs.}
  \item{...}{arguments to be passed to or from methods.}
}
\details{
  These are generic functions, with no default methods.

  Often, classed \R objects (e.g., those representing text documents in
  packages \pkg{NLP} and \pkg{tm}) contain information that can be
  grouped into \dQuote{content}, metadata and other components, where
  content can be arbitrary, and metadata are collections of tag/value
  pairs represented as named or empty lists.  The \code{content()} and
  \code{meta()} getters and setters aim at providing a consistent
  high-level interface to the respective information (abstracting from
  how classes internally represent the information).
}
\value{
  Methods for \code{meta()} should return a named or empty list of
  tag/value pairs if no tag is given (default), or the value for the
  given tag.
}
\seealso{
  \code{\link{TextDocument}} for basic information on the text document
  infrastructure employed by package \pkg{NLP}.
}
