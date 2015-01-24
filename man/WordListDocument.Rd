\name{WordListDocument}
\alias{WordListDocument}
\title{Word List Text Documents}
\description{
  Create text documents from word lists.
}
\usage{
WordListDocument(con, encoding = "unknown", meta = list())
}
\arguments{
  \item{con}{a connection object or a character string.
    See \code{\link{readLines}()} for details.
  }
  \item{encoding}{encoding to be assumed for input strings.
    See \code{\link{readLines}()} for details.
  }
  \item{meta}{a named or empty list of document metadata tag-value
    pairs.}
}
\details{
  \code{WordListDocument()} uses \code{\link{readLines}()} to read
  collections of words from connections for which each line provides one
  word, with blank lines ignored, and returns a word list document
  object which inherits from classes \code{"WordListDocument"} and
  \code{"\link{TextDocument}"}.

  The words can be extracted using the \code{\link{words}()} and
  \code{\link{as.character}()} methods for class
  \code{"WordListDocument"}.
}
\value{
  An object inheriting from \code{"WordListDocument"} and
  \code{"\link{TextDocument}"}.
}  
\seealso{
  \code{\link{TextDocument}} for basic information on the text document
  infrastructure employed by package \pkg{NLP}.
}
