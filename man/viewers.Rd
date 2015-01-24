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
\value{
  For \code{words()}, a character vector with the word tokens in the
  document.

  For \code{sents()}, a list of character vectors with the word tokens
  in each sentence.

  For \code{paras()}, a list of lists of character vectors with the word
  tokens in each sentence, grouped according to the paragraphs.

  For \code{tagged_words()}, a character vector with the POS tagged word
  tokens in the document (i.e., the word tokens and their POS tags,
  separated by \samp{/}).

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
\seealso{
  \code{\link{TextDocument}} for basic information on the text document
  infrastructure employed by package \pkg{NLP}.
}
