\name{Annotator}
\alias{Annotator}
\alias{Annotator_Pipeline}
\alias{as.Annotator_Pipeline}
\title{Annotator (pipeline) objects}
\description{
  Create annotator (pipeline) objects.
}
\usage{
Annotator(f, meta = list(), classes = NULL)
Annotator_Pipeline(..., meta = list())
as.Annotator_Pipeline(x)
}
\arguments{
  \item{f}{an annotator function, which must have formals \code{s} and
    \code{a} giving, respectively, the string with the natural language
    text to annotate and an annotation object to start from, and return
    an annotation object with the computed annotations.}
  \item{meta}{an empty or named list of annotator (pipeline) metadata
    tag-value pairs.}
  \item{classes}{a character vector or \code{NULL} (default) giving
    classes to be used for the created annotator object in addition to
    \code{"Annotator"}.}
  \item{...}{annotator objects.}
  \item{x}{an \R object.}
}
\details{
  \code{Annotator()} checks that the given annotator function has the
  appropriate formals, and returns an annotator object which inherits
  from the given classes and \code{"Annotator"}.  There are
  \code{print()} and \code{format()} methods for such objects, which use
  the \code{description} element of the metadata if available.

  \code{Annotator_Pipeline()} creates an annotator pipeline object from
  the given annotator objects.  Such pipeline objects can be used by
  \code{\link{annotate}()} for successively computing and merging
  annotations, and can also be obtained by coercion with
  \code{as.Annotator_Pipeline()}, which currently handles annotator
  objects and lists of such (and of course, annotator pipeline
  objects).
}
\value{
  For \code{Annotator()}, an annotator object inheriting from the given
  classes and class \code{"Annotator"}.

  For \code{Annotator_Pipeline()} and \code{as.Annotator_Pipeline()}, an
  annotator pipeline object inheriting from class
  \code{"Annotator_Pipeline"}.
}
\seealso{
  \link{Simple annotator generators} for creating \dQuote{simple}
  annotator objects based on function performing simple basic NLP tasks.

  Package \pkg{StanfordCoreNLP} available from the repository at
  \url{https://datacube.wu.ac.at} which provides generators for annotator
  pipelines based on the Stanford CoreNLP tools.
}
\examples{
## Use blankline_tokenizer() for a simple paragraph token annotator:
para_token_annotator <-
Annotator(function(s, a = Annotation()) {
              spans <- blankline_tokenizer(s)
              n <- length(spans)
              ## Need n consecutive ids, starting with the next "free"
              ## one:
              from <- next_id(a$id)
              Annotation(seq(from = from, length.out = n),
                         rep.int("paragraph", n),
                         spans$start,
                         spans$end)
          },
          list(description = 
              "A paragraph token annotator based on blankline_tokenizer()."))
para_token_annotator
## Alternatively, use Simple_Para_Token_Annotator().

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
