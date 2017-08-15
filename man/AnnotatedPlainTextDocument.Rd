\name{AnnotatedPlainTextDocument}
\alias{AnnotatedPlainTextDocument}
\alias{annotations}
\title{Annotated Plain Text Documents}
\description{
  Create annotated plain text documents from plain text and collections
  of annotations for this text.
}
\usage{
AnnotatedPlainTextDocument(s, annotations, meta = list())
annotations(x)
}
\arguments{
  \item{s}{a \code{\link{String}} object, or something coercible to this
    using \code{\link{as.String}()} (e.g., a character string with
    appropriate encoding information).}
  \item{annotations}{an \code{\link{Annotation}} object with annotations
    for \code{x}, or a list of such objects.}
  \item{meta}{a named or empty list of document metadata tag-value
    pairs.}
  \item{x}{an object inheriting from class
    \code{"AnnotatedPlainTextDocument"}.}
}
\details{
  Annotated plain text documents combine plain text with collections
  (\dQuote{sets}, implemented as lists) of objects with annotations for
  the text.

  A typical workflow is to use \code{\link{annotate}()} with suitable
  annotator pipelines to obtain the annotations, and then use
  \code{AnnotatedPlainTextDocument()} to combine these with the text
  being annotated.  This yields an object inheriting from
  \code{"AnnotatedPlainTextDocument"} and \code{"\link{TextDocument}"},
  from which the text and collection of annotations can be obtained
  using, respectively, \code{\link{as.character}()} and
  \code{annotations()}.

  There are methods for generics
  \code{\link{words}()},
  \code{\link{sents}()},
  \code{\link{paras}()},
  \code{\link{tagged_words}()},
  \code{\link{tagged_sents}()},
  \code{\link{tagged_paras}()},
  \code{\link{chunked_sents}()},
  \code{\link{parsed_sents}()} and
  \code{\link{parsed_paras}()}
  and class \code{"AnnotatedPlainTextDocument"} providing structured
  views of the text in such documents.  These all have an additional
  argument \code{which} for specifying the annotation object to use (by
  default, the first one is taken), and of course require the necessary
  annotations to be available in the annotation object used.

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
  For \code{AnnotatedPlainTextDocument()}, an object inheriting from
  \code{"AnnotatedPlainTextTextDocument"} and \code{"\link{TextDocument}"}.

  For \code{annotations()}, a list of \code{\link{Annotation}} objects.
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
##   doc <- AnnotatedPlainTextDocument(s, p(s))

doc <- readRDS(system.file("texts", "stanford.rds", package = "NLP"))

doc

## Extract available annotation:
a <- annotations(doc)[[1L]]
a

## Structured views:
sents(doc)
tagged_sents(doc)
tagged_sents(doc, map = Universal_POS_tags_map)
parsed_sents(doc)

## Add (trivial) paragraph annotation:
s <- as.character(doc)
a <- annotate(s, Simple_Para_Token_Annotator(blankline_tokenizer), a)
doc <- AnnotatedPlainTextDocument(s, a)
## Structured view:
paras(doc)
}
