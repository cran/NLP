\name{Tokenizer}
\alias{Span_Tokenizer}
\alias{as.Span_Tokenizer}
\alias{is.Span_Tokenizer}
\alias{Token_Tokenizer}
\alias{as.Token_Tokenizer}
\alias{is.Token_Tokenizer}
\title{Tokenizer objects}
\description{
  Create tokenizer objects.
}
\usage{
Span_Tokenizer(f, meta = list())
as.Span_Tokenizer(x, ...)

Token_Tokenizer(f, meta = list())
as.Token_Tokenizer(x, ...)
}
\arguments{
  \item{f}{a tokenizer function taking the string to tokenize as
    argument, and returning either the tokens (for
    \code{Token_Tokenizer}) or the corresponding spans (for
    \code{Span_Tokenizer}).}
  \item{meta}{a named or empty list of tokenizer metadata tag-value
    pairs.}
  \item{x}{an \R object.}
  \item{...}{further arguments passed to or from other methods.}
}
\details{
  Tokenization is the process of breaking a text string up into words,
  phrases, symbols, or other meaningful elements called tokens.  This
  can be accomplished by returning the sequence of tokens, or the
  corresponding spans (character start and end positions).
  We refer to tokenization resources of the respective kinds as
  \dQuote{token tokenizers} and \dQuote{span tokenizers}.

  \code{Span_Tokenizer()} and \code{Token_Tokenizer()} return tokenizer
  objects which are functions with metadata and suitable class
  information, which in turn can be used for converting between the two
  kinds using \code{as.Span_Tokenizer()} or \code{as.Token_Tokenizer()}.
  It is also possible to coerce annotator (pipeline) objects to
  tokenizer objects, provided that the annotations provide suitable
  token annotations.  By default, word tokens are used; this can be
  controlled via the \code{type} argument of the coercion methods (e.g.,
  use \code{type = "sentence"} to extract sentence tokens).

  There are also \code{print()} and \code{format()} methods for
  tokenizer objects, which use the \code{description} element of the
  metadata if available.
}
\seealso{
  \code{\link{Regexp_Tokenizer}()} for creating regexp span tokenizers.
}
\examples{
## A simple text.
s <- String("  First sentence.  Second sentence.  ")
##           ****5****0****5****0****5****0****5**

## Use a pre-built regexp (span) tokenizer:
wordpunct_tokenizer
wordpunct_tokenizer(s)
## Turn into a token tokenizer:
tt <- as.Token_Tokenizer(wordpunct_tokenizer)
tt
tt(s)
## Of course, in this case we could simply have done
s[wordpunct_tokenizer(s)]
## to obtain the tokens from the spans.
## Conversion also works the other way round: package 'tm' provides
## the following token tokenizer function:
scan_tokenizer <- function(x)
    scan(text = as.character(x), what = "character", quote = "", 
         quiet = TRUE)
## Create a token tokenizer from this:
tt <- Token_Tokenizer(scan_tokenizer)
tt(s)
## Turn into a span tokenizer:
st <- as.Span_Tokenizer(tt)
st(s)
## Checking tokens from spans:
s[st(s)]
}
