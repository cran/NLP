\name{CoNLLTextDocument}
\alias{CoNLLTextDocument}
\title{CoNLL-Style Text Documents}
\description{
  Create text documents from CoNLL-style files.
}
\usage{
CoNLLTextDocument(con, encoding = "unknown", meta = list())
}
\arguments{
  \item{con}{a connection object or a character string.
    See \code{\link{scan}()} for details.
  }
  \item{encoding}{encoding to be assumed for input strings.
    See \code{\link{scan}()} for details.
  }
  \item{meta}{a named or empty list of document metadata tag-value
    pairs.}
}
\details{
  CoNLL-style files use an extended tabular format where empty lines
  separate sentences, and non-empty lines consist of whitespace
  separated columns giving the word tokens and annotations for these.
  In principle, these annotations can vary from corpus to corpus: the
  current version of \code{CoNLLTextDocument()} assumes a fixed set of 3
  columns giving, respectively, the word token and its POS and chunk
  tags.

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

  \url{http://ifarm.nl/signll/conll/} for general information about
  CoNLL (Conference on Natural Language Learning), the yearly meeting of
  the Special Interest Group on Natural Language Learning of the
  Association for Computational Linguistics. 
  
  \url{http://www.cnts.ua.ac.be/conll2000/chunking/} for the CoNLL 2000
  chunking task, and training and test data sets which can be read in
  using \code{CoNLLTextDocument()}.
}
