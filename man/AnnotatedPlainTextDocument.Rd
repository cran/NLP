\name{AnnotatedPlainTextDocument}
\alias{AnnotatedPlainTextDocument}
\alias{annotation}
\title{Annotated Plain Text Documents}
\description{
  Create annotated plain text documents from plain text and collections
  of annotations for this text.
}
\usage{
AnnotatedPlainTextDocument(s, a, meta = list())
annotation(x)
}
\arguments{
  \item{s}{a \code{\link{String}} object, or something coercible to this
    using \code{\link{as.String}()} (e.g., a character string with
    appropriate encoding information).}
  \item{a}{an \code{\link{Annotation}} object with annotations for
    \code{s}.}
  \item{meta}{a named or empty list of document metadata tag-value
    pairs.}
  \item{x}{an object inheriting from class
    \code{"AnnotatedPlainTextDocument"}.}
}
\details{
  Annotated plain text documents combine plain text with annotations for
  the text.

  A typical workflow is to use \code{\link{annotate}()} with suitable
  annotator pipelines to obtain the annotations, and then use
  \code{AnnotatedPlainTextDocument()} to combine these with the text
  being annotated.  This yields an object inheriting from
  \code{"AnnotatedPlainTextDocument"} and \code{"\link{TextDocument}"},
  from which the text and annotations can be obtained using,
  respectively, \code{\link{as.character}()} and \code{annotation()}.

  There are methods for class \code{"AnnotatedPlainTextDocument"} and
  generics
  \code{\link{words}()},
  \code{\link{sents}()},
  \code{\link{paras}()},
  \code{\link{tagged_words}()},
  \code{\link{tagged_sents}()},
  \code{\link{tagged_paras}()},
  \code{\link{chunked_sents}()},
  \code{\link{parsed_sents}()} and
  \code{\link{parsed_paras}()}
  providing structured views of the text in such documents.  These all
  require the necessary annotations to be available in the annotation
  object used.

  The methods for generics
  \code{\link{tagged_words}()},
  \code{\link{tagged_sents}()} and
  \code{\link{tagged_paras}()}
  provide a mechanism for mapping POS tags via the \code{map} argument,
  see section \bold{Details} in the help page for
  \code{\link{tagged_words}()} for more information.
  The POS tagset used will be inferred from the \code{POS_tagset}
  metadata element of the annotation object used.
}
\value{
  For \code{AnnotatedPlainTextDocument()}, an annotated plain text
  document object inheriting from
  \code{"AnnotatedPlainTextTextDocument"} and
  \code{"\link{TextDocument}"}.

  For \code{annotation()}, an \code{\link{Annotation}} object.
}
\seealso{
  \code{\link{TextDocument}} for basic information on the text document
  infrastructure employed by package \pkg{NLP}.
}
\examples{
## Use a pre-built annotated plain text document obtained by employing an
## annotator pipeline from package 'StanfordCoreNLP', available from the
## repository at <https://datacube.wu.ac.at>, using the following code:
##   require("StanfordCoreNLP")
##   s <- paste("Stanford University is located in California.",
##              "It is a great university.")
##   p <- StanfordCoreNLP_Pipeline(c("pos", "lemma", "parse"))
##   d <- AnnotatedPlainTextDocument(s, p(s))

d <- readRDS(system.file("texts", "stanford.rds", package = "NLP"))

d

## Extract available annotation:
a <- annotation(d)
a

## Structured views:
sents(d)
tagged_sents(d)
tagged_sents(d, map = Universal_POS_tags_map)
parsed_sents(d)

## Add (trivial) paragraph annotation:
s <- as.character(d)
a <- annotate(s, Simple_Para_Token_Annotator(blankline_tokenizer), a)
d <- AnnotatedPlainTextDocument(s, a)
## Structured view:
paras(d)
}
