\name{CoNLLTextDocument}
\alias{CoNLLTextDocument}
\title{CoNLL-Style Text Documents}
\description{
  Create text documents from CoNLL-style files.
}
\usage{
CoNLLTextDocument(con, encoding = "unknown", format = "conll00",
                  meta = list())
}
\arguments{
  \item{con}{a connection object or a character string.
    See \code{\link{scan}()} for details.
  }
  \item{encoding}{encoding to be assumed for input strings.
    See \code{\link{scan}()} for details.
  }
  \item{format}{a character vector specifying the format.
    See \bold{Details}.
  }
  \item{meta}{a named or empty list of document metadata tag-value
    pairs.}
}
\details{
  CoNLL-style files use an extended tabular format where empty lines
  separate sentences, and non-empty lines consist of whitespace
  separated columns giving the word tokens and annotations for these.
  Such formats were popularized through their use for the shared tasks
  of CoNLL (Conference on Natural Language Learning), the yearly meeting
  of the Special Interest Group on Natural Language Learning of the
  Association for Computational Linguistics (see
  \url{http://www.signll.org/conll/} for more information about CoNLL).
  
  The precise format can vary according to corpus, and must be specified
  via argument \code{format}, as either a character string giving a
  pre-defined format, or otherwise a character vector with elements
  giving the names of the \sQuote{fields} (columns), and names used to
  give the field \sQuote{types}, with \sQuote{WORD}, \sQuote{POS} and
  \sQuote{CHUNK} to be used for, respectively, word tokens, POS tags, and
  chunk tags.  For example, \preformatted{  c(WORD = "WORD", POS = "POS", CHUNK = "CHUNK")}
  would be a format specification appropriate for the CoNLL-2000
  chunking task, as also available as the pre-defined \code{"conll00"},
  which serves as default format for reasons of back-compatibility.
  Other pre-defined formats are \code{"conll01"} (for the CoNLL-2001
  clause identification task), \code{"conll02"} (for the CoNLL-2002
  language-independent named entity recognition task), \code{"conllx"}
  (for the CoNLL-X format used in at least the CoNLL-2006 and CoNLL-2007
  multilingual dependency parsing tasks), and \code{"conll09"} (for the
  CoNLL-2009 shared task on syntactic and semantic dependencies in
  multiple languages).

  The lines are read from the given connection and split into fields
  using \code{\link{scan}()}.  From this, a suitable representation of
  the provided information is obtained, and returned as a CoNLL text
  document object inheriting from classes \code{"CoNLLTextDocument"} and
  \code{"\link{TextDocument}"}.

  There are methods for generics
  \code{\link{words}()},
  \code{\link{sents}()},
  \code{\link{tagged_words}()},
  \code{\link{tagged_sents}()}, and
  \code{\link{chunked_sents}()}
  (as well as \code{\link{as.character}()})
  and class \code{"CoNLLTextDocument"},
  which should be used to access the text in such text document
  objects.

  The methods for generics
  \code{\link{tagged_words}()} and
  \code{\link{tagged_sents}()}
  provide a mechanism for mapping POS tags via the \code{map} argument,
  see section \bold{Details} in the help page for
  \code{\link{tagged_words}()} for more information.
  The POS tagset used will be inferred from the \code{POS_tagset}
  metadata element of the CoNLL-style text document.
}
\value{
  An object inheriting from \code{"CoNLLTextDocument"} and
  \code{"\link{TextDocument}"}.
}
\seealso{
  \code{\link{TextDocument}} for basic information on the text document
  infrastructure employed by package \pkg{NLP}.

  \url{http://www.cnts.ua.ac.be/conll2000/chunking/} for the CoNLL-2000
  chunking task, and training and test data sets which can be read in
  using \code{CoNLLTextDocument()}.
}
