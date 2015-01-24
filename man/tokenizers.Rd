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
Regexp_Tokenizer(pattern, description = NULL, invert = FALSE, ...)
blankline_tokenizer(s)
whitespace_tokenizer(s)
wordpunct_tokenizer(s)
}
\arguments{
  \item{pattern}{a character string giving the regular expression to use
    for matching.}
  \item{description}{a character string describing the tokenizer, or
    \code{NULL} (default).}
  \item{invert}{a logical indicating whether to match separators between
    tokens.}
  \item{...}{further arguments to be passed to \code{\link{gregexpr}()}.}
  \item{s}{a \code{\link{String}} object, or something coercible to this
    using \code{\link{as.String}()} (e.g., a character string with
    appropriate encoding information)}
}
\details{
  \code{Regexp_Tokenizer()} creates regexp tokenizers which use the
  given \code{pattern} and \code{...} arguments to match tokens or
  separators between tokens via \code{\link{gregexpr}()}, and then
  transform the results of this into character spans of the tokens
  found.  The given \code{description} is currently kept as an
  attribute.

  \code{whitespace_tokenizer()} tokenizes by treating any sequence of
  whitespace characters as a separator.

  \code{blankline_tokenizer()} tokenizes by treating any sequence of
  blank lines as a separator.

  \code{wordpunct_tokenizer()} tokenizes by matching sequences of
  alphabetic characters and sequences of (non-whitespace) non-alphabetic
  characters.
}
\value{
  \code{Regexp_Tokenizer()} returns the created regexp tokenizer.

  \code{blankline_tokenizer()}, \code{whitespace_tokenizer()} and
  \code{wordpunct_tokenizer()} return the spans of the tokens found in
  \code{s}.
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
