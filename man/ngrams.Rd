\name{ngrams}
\alias{ngrams}
\title{Compute N-Grams}
\description{
  Compute the \eqn{n}-grams (contiguous sub-sequences of length \eqn{n})
  of a given sequence.
}
\arguments{
  \item{x}{a sequence (vector).}
  \item{n}{a positive integer giving the length of contiguous
    sub-sequences to be computed}
}
\value{
  a list with the computed sub-sequences.
}
\examples{
s <- "The quick brown fox jumps over the lazy dog"
## Split into words:
w <- strsplit(s, " ", fixed = TRUE)[[1L]]
## Word tri-grams:
ngrams(w, 3L)
## Word tri-grams pasted together:
vapply(ngrams(w, 3L), paste, "", collapse = " ")
}
