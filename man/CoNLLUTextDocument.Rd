\name{CoNLLUTextDocument}
\alias{CoNLLUTextDocument}
\alias{read_CoNNLU}
\title{
  CoNNL-U Text Documents
}
\description{
  Create text documents from CoNNL-U format files.
}
\usage{
CoNLLUTextDocument(con, meta = list(), text = NULL)
read_CoNNLU(con)
}
\arguments{
  \item{con}{a connection object or a character string.
    See \code{\link{scan}()} for details.
  }
  \item{meta}{a named or empty list of document metadata tag-value
    pairs.}
  \item{text}{a character vector giving the text of the CoNNL-U
    annotation.  If \code{NULL}, the \code{text} comments of the
    annotation are used.}
}
\details{
  The CoNLL-U format (see
  \url{https://universaldependencies.org/format.html})
  is a CoNLL-style format for annotated texts popularized and employed
  by the Universal Dependencies project
  (see \url{https://universaldependencies.org/}).
  For each \dQuote{word} in the text, this provides exactly the 10
  fields
  \code{ID},
  \code{FORM} (word form or punctuation symbol),
  \code{LEMMA} (lemma or stem of word form),
  \code{UPOSTAG} (universal part-of-speech tag, see
  \url{https://universaldependencies.org/u/pos/index.html}),
  \code{XPOSTAG} (language-specific part-of-speech tag, may be
  unavailable),
  \code{FEATS} (list of morphological features),
  \code{HEAD},
  \code{DEPREL},
  \code{DEPS}, and
  \code{MISC}.

  \code{read_CoNNLU()} reads the lines with these fields and optional
  comments from the given connection and splits into fields using
  \code{\link{scan}()}.  This is combined with consecutive sentence ids
  into a data frame inheriting from class \code{"CoNNLU_Annotation"}
  used for representing the annotation information,

  \code{CoNLLUTextDocument()} combines this annotation information with
  the given metadata (and optionally the original pre-tokenized text)
  into a CoNLL-U text document inheriting from classes
  \code{"CoNLLUTextDocument"} and \code{"\link{TextDocument}"}.

  The complete annotation information data frame can be extracted via
  \code{content()}.  CoNLL-U v2 requires providing the complete texts of
  each sentence (or a reconstruction thereof) in \samp{# text =} comment
  lines.  Where consistently provided, these are made available in the
  \code{text} attribute of the content data frame.

  In addition, there are methods for generics
  \code{\link{as.character}()},
  \code{\link{words}()},
  \code{\link{sents}()},
  \code{\link{tagged_words}()}, and
  \code{\link{tagged_sents}()}
  and class \code{"CoNLLUTextDocument"},
  which should be used to access the text in such text document
  objects.
  
  The CoNLL-U format allows to represent both words and (multiword)
  tokens (see section \sQuote{Words, Tokens and Empty Nodes} in the
  format documentation), as distinguished by ids being integers or
  integer ranges, with the words being annotated further.  One can
  use \code{as.character()} to extract the \emph{tokens}; all other
  viewers listed above use the \emph{words}.  Finally, the viewers
  incorporating POS tags take a \code{which} argument to specify using
  the universal or language-specific tags, by giving a substring of
  \code{"UPOSTAG"} (default) or \code{"XPOSTAG"}.
}
\value{
  For \code{CoNLLUTextDocument()}, an object inheriting from
  \code{"CoNLLUTextDocument"} and \code{"\link{TextDocument}"}.

  For \code{read_CoNNLU()}, an object inherting from
  \code{"CoNNLU_Annotation"} and \code{"\link{data.frame}"}
}
\seealso{
  \code{\link{TextDocument}} for basic information on the text document
  infrastructure employed by package \pkg{NLP}.

  \url{https://universaldependencies.org/} for access to the Universal
  Dependencies treebanks, which provide annotated texts in \emph{many}
  different languages using CoNLL-U format.
}
