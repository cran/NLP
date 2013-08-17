\name{Annotator}
\alias{Annotator}
\title{Annotator objects}
\description{
  Create annotator objects.
}
\usage{
Annotator(f, description = NULL, classes = NULL)
}
\arguments{
  \item{f}{an annotator function: these must have formals \code{s} and
    \code{a} giving, respectively, the string with the natural language
    text to annotate and an annotation object to start from, and return
    an annotation object with the computed annotations.}
  \item{description}{a character string describing the annotator, or
    \code{NULL} (default).}
  \item{classes}{a character vector or \code{NULL} (default) giving
    additional classes to be used for the created annotator object in
    addition to \code{"Annotator"}.}
}
\details{
  \code{Annotator()} checks that the given annotator function has the
  appropriate formals, and returns an annotator object which inherits
  from the given classes and \code{"Annotator"}, and contains the given
  description (currently, as an attribute) to be used in the
  \code{print} method for such objects.
}
\value{
  An annotator object inheriting from the given classes and class
  \code{"Annotator"}.
}
\seealso{
  \code{\link{Simple annotator generators}} for creating \dQuote{simple}
  annotator objects based on function performing simple basic NLP tasks.

  Package \pkg{StanfordCoreNLP} available from the repository at
  \url{http://datacube.wu.ac.at} which provides annotator generators for
  annotator pipelines based on the Stanford CoreNLP tools.
}
\examples{
## Use blankline_tokenizer() for a simple paragraph token annotator:
para_token_annotator <-
Annotator(function(s, a = Annotation()) {
              spans <- blankline_tokenizer(s)
              n <- length(spans)
              ## Need n consecutive ids, starting with the next "free"
              ## one:
              from <- if(!length(id <- a$id)) 1L else max(id) + 1L
              Annotation(seq(from = from, length.out = n),
                         rep.int("paragraph", n),
                         spans$start,
                         spans$end)
          },
          "A paragraph token annotator based on blankline_tokenizer().")
para_token_annotator

## A simple text with two paragraphs:
s <- String(paste("  First sentence.  Second sentence.  ",
                  "  Second paragraph.  ",
                  sep = "\n\n"))
a <- annotate(s, para_token_annotator)
## Annotations for paragraph tokens.
a
## Extract paragraph tokens.
s[a]
}
