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
\alias{otoks}
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
  Methods for extracting POS tagged word tokens (i.e., for generics
  \code{tagged_words()}, \code{tagged_sents()} and
  \code{tagged_paras()}) can optionally provide a mechanism for mapping
  the POS tags via a \code{map} argument.  This can give a function, a
  named character vector (with names and elements the tags to map from
  and to, respectively), or a named list of such named character
  vectors, with names corresponding to POS tagsets (see
  \code{\link{Universal_POS_tags_map}} for an example).  If a list, the
  map used will be the element with name matching the POS tagset used
  (this information is typically determined from the text document
  metadata; see the the help pages for text document extension classes
  implementing this mechanism for details).

  Text document classes may provide support for representing both
  (syntactic) words (for which annotations can be provided) and
  orthographic (word) tokens, e.g., in Spanish \emph{dámelo = da me lo}.
  For these, \code{words()} gives the syntactic word tokens, and
  \code{otoks()} the orthographic word tokens.  This is currently
  supported for \link[=CoNLLUTextDocument]{CoNNL-U text documents} (see
  \url{https://universaldependencies.org/format.html} for more
  information) and \link[=AnnotatedPlainTextDocument]{annotated plain
  text documents} (via \code{word} features as used for example for some
  Stanford CoreNLP annotator pipelines provided by package
  \pkg{StanfordCoreNLP} available from the repository at
  \url{https://datacube.wu.ac.at}).

  In addition to methods for the text document classes provided by
  package \pkg{NLP} itself, (see \link{TextDocument}), package \pkg{NLP}
  also provides word tokens and POS tagged word tokens for the results
  of
  \code{\link[udpipe:udpipe_annotate]{udpipe_annotate}()}
  from package \CRANpkg{udpipe},
  \code{\link[spacyr:spacy_parse]{spacy_parse}()}
  from package \CRANpkg{spacyr},
  and
  \code{\link[cleanNLP:cnlp_annotate]{cnlp_annotate}()}
  from package \CRANpkg{cleanNLP}.
}
\value{
  For \code{words()}, a character vector with the word tokens in the
  document.

  For \code{sents()}, a list of character vectors with the word tokens
  in the sentences.

  For \code{paras()}, a list of lists of character vectors with the word
  tokens in the sentences, grouped according to the paragraphs.

  For \code{tagged_words()}, a character vector with the POS tagged word
  tokens in the document (i.e., the word tokens and their POS tags,
  separated by \samp{/}).

  For \code{tagged_sents()}, a list of character vectors with the POS
  tagged word tokens in the sentences.

  For \code{tagged_paras()}, a list of lists of character vectors with
  the POS tagged word tokens in the sentences, grouped according to the
  paragraphs.
  
  For \code{chunked_sents()}, a list of (flat) \code{\link{Tree}}
  objects giving the chunk trees for the sentences in the document.

  For \code{parsed_sents()}, a list of \code{\link{Tree}}
  objects giving the parse trees for the sentences in the document.

  For \code{parsed_paras()}, a list of lists of \code{\link{Tree}}
  objects giving the parse trees for the sentences in the document,
  grouped according to the paragraphs in the document.

  For \code{otoks()}, a character vector with the orthographic word
  tokens in the document.
}
\seealso{
  \code{\link{TextDocument}} for basic information on the text document
  infrastructure employed by package \pkg{NLP}.
}
\examples{
## Example from <https://universaldependencies.org/format.html>:
d <- CoNLLUTextDocument(system.file("texts", "spanish.conllu",
                                    package = "NLP"))
content(d)
## To extract the syntactic words:
words(d)
## To extract the orthographic word tokens:
otoks(d)
}
