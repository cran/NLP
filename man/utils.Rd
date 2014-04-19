\name{utils}
\alias{next_id}
\alias{single_feature}
\title{Annotation Utilities}
\description{Utilities for creating annotation objects.}
\usage{
next_id(id)
single_feature(value, tag)
}
\arguments{
  \item{id}{an integer vector of annotation ids.}
  \item{value}{an \R object.}
  \item{tag}{a character string.}
}
\details{
  \code{next_id()} obtains the next \dQuote{available} id based on the
  given annotation ids (one more than the maximal non-missing id).

  \code{single_feature()} creates a single feature from the given value
  and tag (i.e., a named list with the value named by the tag).
}
