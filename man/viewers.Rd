\name{viewers}
\alias{sents}
\alias{words}
\alias{paras}
\alias{tagged_sents}
\alias{tagged_paras}
\alias{tagged_words}
\alias{chunked_sents}
\alias{parsed_sents}
\alias{parsed_paras}
\title{Text Document Viewers}
\description{
  Provide suitable \dQuote{views} of the text contained in text
  documents.
}
\usage{
words(x, ...)
sents(x, ...)
paras(x, ...)
tagged_words(x, ...)
tagged_sents(x, ...)
tagged_paras(x, ...)
chunked_sents(x, ...)
parsed_sents(x, ...)
parsed_paras(x, ...)
}
\arguments{
  \item{x}{a text document object.}
  \item{...}{further arguments to be passed to or from methods.}
}
\details{
  \emph{Text documents} are documents containing (natural language)
  text.  In packages which employ the infrastructure provided by package
  \pkg{NLP}, such documents are represented via the virtual S3 class
  \code{"TextDocument"}: such packages then provide S3 text document
  classes extending the virtual base class (such as the
  \code{\link{AnnotatedPlainTextDocument}} objects provided by package
  \pkg{NLP} itself).

  All extension classes must provide an \code{\link{as.character}}
  method which extracts the natural language text in documents of the
  respective classes in a \dQuote{suitable} (not necessarily structured)
  form.

  In addition, the infrastructure features the above generic functions
  \code{words()}, \code{sents()}, etc., for which extension classes can
  provide methods giving a structured view of the text contained in
  documents of these classes (returning, e.g., a character vector with
  the word tokens in these documents, and a list of such character
  vectors).
}
\value{
  For \code{words()}, a character vector with the word tokens in the
  document.

  For \code{sents()}, a list of character vectors with the word tokens
  in each sentence.

  For \code{paras()}, a list of lists of character vectors with the word
  tokens in each sentence, grouped according to the paragraphs.

  For \code{tagged_words()}, a character vector with the POS tagged word
  tokens in the document (i.e., the word tokens and their POS tags,
  separated by \code{/}).

  For \code{tagged_sents()}, a list of character vectors with the POS
  tagged word tokens in each sentence.

  For \code{tagged_paras()}, a list of lists of character vectors with
  the POS tagged word tokens in each sentence, grouped according to the
  paragraphs.
  
  For \code{chunked_sents()}, a list of (flat) \code{\link{Tree}}
  objects giving the chunk trees for each sentence in the document.

  For \code{parsed_sents()}, a list of \code{\link{Tree}}
  objects giving the parse trees for each sentence in the document.

  For \code{parsed_paras()}, a list of lists of \code{\link{Tree}}
  objects giving the parse trees for each sentence in the document,
  grouped according to the paragraphs in the document.
}
