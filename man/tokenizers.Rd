\name{tokenizers}
\alias{Regexp_Tokenizer}
\alias{blankline_tokenizer}
\alias{whitespace_tokenizer}
\alias{wordpunct_tokenizer}
\title{Regexp tokenizers}
\description{
  Tokenizers using regular expressions to match either tokens or
  separators between tokens.
}
\usage{
Regexp_Tokenizer(pattern, invert = FALSE, ..., meta = list())
blankline_tokenizer(s)
whitespace_tokenizer(s)
wordpunct_tokenizer(s)
}
\arguments{
  \item{pattern}{a character string giving the regular expression to use
    for matching.}
  \item{invert}{a logical indicating whether to match separators between
    tokens.}
  \item{...}{further arguments to be passed to \code{\link{gregexpr}()}.}
  \item{meta}{a named or empty list of tokenizer metadata tag-value
    pairs.}
  \item{s}{a \code{\link{String}} object, or something coercible to this
    using \code{\link{as.String}()} (e.g., a character string with
    appropriate encoding information)}
}
\details{
  \code{Regexp_Tokenizer()} creates regexp span tokenizers which use the
  given \code{pattern} and \code{...} arguments to match tokens or
  separators between tokens via \code{\link{gregexpr}()}, and then
  transform the results of this into character spans of the tokens
  found.

  \code{whitespace_tokenizer()} tokenizes by treating any sequence of
  whitespace characters as a separator.

  \code{blankline_tokenizer()} tokenizes by treating any sequence of
  blank lines as a separator.

  \code{wordpunct_tokenizer()} tokenizes by matching sequences of
  alphabetic characters and sequences of (non-whitespace) non-alphabetic
  characters.
}
\value{
  \code{Regexp_Tokenizer()} returns the created regexp span tokenizer.

  \code{blankline_tokenizer()}, \code{whitespace_tokenizer()} and
  \code{wordpunct_tokenizer()} return the spans of the tokens found in
  \code{s}.
}
\seealso{
  \code{\link{Span_Tokenizer}()} for general information on span
  tokenizer objects.
}
\examples{
## A simple text.
s <- String("  First sentence.  Second sentence.  ")
##           ****5****0****5****0****5****0****5**

spans <- whitespace_tokenizer(s)
spans
s[spans]

spans <- wordpunct_tokenizer(s)
spans
s[spans]
}
