\name{features}
\alias{features}
\title{Extract Annotation Features}
\description{
  Conveniently extract features from annotations and annotated plain
  text documents.
}
\usage{
features(x, type = NULL, simplify = TRUE)
}
\arguments{
  \item{x}{an object inheriting from class \code{"Annotation"} or
    \code{"AnnotatedPlainTextDocument"}.}
  \item{type}{a character vector of annotation types to be used for
    selecting annotations, or \code{NULL} (default) to use all
    annotations.  When selecting, the elements of \code{type} will
    partially be matched against the annotation types.}
  \item{simplify}{a logical indicating whether to simplify feature
    values to a vector.}
}
\details{
  \code{features()} conveniently gathers all feature tag-value pairs in
  the selected annotations into a data frame with variables the values
  for all tags found (using a \code{NULL} value for tags without a
  value).  In general, variables will be \emph{lists} of extracted
  values.  By default, variables where all elements are length one
  atomic vectors are simplified into an atomic vector of values.  The
  values for specific tags can be extracted by suitably subscripting the
  obtained data frame. 
}
\examples{
## Use a pre-built annotated plain text document,
## see ? AnnotatedPlainTextDocument.
d <- readRDS(system.file("texts", "stanford.rds", package = "NLP"))
## Extract features of all *word* annotations in doc:
x <- features(d, "word")
## Could also have abbreviated "word" to "w".
x
## Only lemmas:
x$lemma
## Words together with lemmas:
paste(words(d), x$lemma, sep = "/")
}
