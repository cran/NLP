\name{TaggedTextDocument}
\alias{TaggedTextDocument}
\title{POS-Tagged Word Text Documents}
\description{
  Create text documents from files containing POS-tagged words.
}
\usage{
TaggedTextDocument(con, encoding = "unknown",
                   word_tokenizer = whitespace_tokenizer,
                   sent_tokenizer = Regexp_Tokenizer("\n", invert = TRUE),
                   para_tokenizer = blankline_tokenizer,
                   sep = "/",
                   meta = list())
}
\arguments{
  \item{con}{a connection object or a character string.
    See \code{\link{readLines}()} for details.
  }
  \item{encoding}{encoding to be assumed for input strings.
    See \code{\link{readLines}()} for details.
  }
  \item{word_tokenizer}{a function for obtaining the word token spans.}
  \item{sent_tokenizer}{a function for obtaining the sentence token
    spans.}
  \item{para_tokenizer}{a function for obtaining the paragraph token
    spans, or \code{NULL} in which case no paragraph tokenization is
    performed.}
  \item{sep}{the character string separating the word tokens and their
    POS tags.}
  \item{meta}{a named or empty list of document metadata tag-value
    pairs.}
}
\details{
  \code{TaggedTextDocument()} creates documents representing natural
  language text as suitable collections of POS-tagged words, based on
  using \code{\link{readLines}()} to read text lines from connections
  providing such collections.

  The text read is split into paragraph, sentence and tagged word tokens
  using the span tokenizers specified by arguments
  \code{para_tokenizer}, \code{sent_tokenizer} and
  \code{word_tokenizer}.  By default, paragraphs are assumed to be
  separated by blank lines, sentences by newlines and tagged word tokens
  by whitespace.  Finally, word tokens and their POS tags are obtained
  by splitting the tagged word tokens according to \code{sep}.  From
  this, a suitable representation of the provided collection of
  POS-tagged words is obtained, and returned as a tagged text document
  object inheriting from classes \code{"TaggedTextDocument"} and
  \code{"\link{TextDocument}"}.

  There are methods for generics
  \code{\link{words}()},
  \code{\link{sents}()},
  \code{\link{paras}()},
  \code{\link{tagged_words}()},
  \code{\link{tagged_sents}()}, and
  \code{\link{tagged_paras}()}
  (as well as \code{\link{as.character}()})
  and class \code{"TaggedTextDocument"},
  which should be used to access the text in such text document
  objects.

  The methods for generics
  \code{\link{tagged_words}()}, 
  \code{\link{tagged_sents}()} and
  \code{\link{tagged_paras}()}
  provide a mechanism for mapping POS tags via the \code{map} argument,
  see section \bold{Details} in the help page for
  \code{\link{tagged_words}()} for more information.
  The POS tagset used will be inferred from the \code{POS_tagset}
  metadata element of the CoNLL-style text document.
}
\value{
  A tagged text document object inheriting from
  \code{"TaggedTextDocument"} and \code{"\link{TextDocument}"}.
}
\seealso{
  \url{https://www.nltk.org/nltk_data/packages/corpora/brown.zip}
  which provides the W. N. Francis and H. Kucera Brown tagged word
  corpus as an archive of files which can be read in using
  \code{TaggedTextDocument()}.

  Package \pkg{tm.corpus.Brown} available from the repository at
  \url{https://datacube.wu.ac.at} conveniently provides this corpus
  as a \pkg{tm} \link[tm]{VCorpus} of tagged text documents.
}
