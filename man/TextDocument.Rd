\name{TextDocument}
\alias{TextDocument}
\title{Text Documents}
\description{
  Representing and computing on text documents.
}
\details{
  \emph{Text documents} are documents containing (natural language)
  text.  In packages which employ the infrastructure provided by package
  \pkg{NLP}, such documents are represented via the virtual S3 class
  \code{"TextDocument"}: such packages then provide S3 text document
  classes extending the virtual base class (such as the
  \code{\link{AnnotatedPlainTextDocument}} objects provided by package
  \pkg{NLP} itself).

  All extension classes must provide an \code{\link{as.character}()}
  method which extracts the natural language text in documents of the
  respective classes in a \dQuote{suitable} (not necessarily structured)
  form, as well as \code{\link{content}()} and \code{\link{meta}()}
  methods for accessing the (possibly raw) document content and metadata.

  In addition, the infrastructure features the generic functions
  \code{\link{words}()}, \code{\link{sents}()}, etc., for which
  extension classes can provide methods giving a structured view of the
  text contained in documents of these classes (returning, e.g., a
  character vector with the word tokens in these documents, and a list
  of such character vectors).
}
\seealso{
  \code{\link{AnnotatedPlainTextDocument}},
  \code{\link{CoNLLTextDocument}},
  \code{\link{TaggedTextDocument}}, and
  \code{\link{WordListDocument}}
  for the text document classes provided by package \pkg{NLP}.
}
