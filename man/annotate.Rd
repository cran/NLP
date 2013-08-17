\name{annotate}
\alias{annotate}
\title{Annotate text strings}
\description{
  Compute annotations by iteratively calling the given annotators with
  the given text and current annotations, and merging the newly computed
  annotations with the already available ones.
}
\usage{
annotate(s, f, a = Annotation())
}
\arguments{
  \item{s}{a \code{\link{String}} object, or something coercible to this
    using \code{\link{as.String}} (e.g., a character string with
    appropriate encoding information)}
  \item{f}{an \code{\link{Annotator}} object, or a list of such objects
    representing an annotator pipeline.}
  \item{a}{an \code{\link{Annotation}} object giving the annotations to
    start with.}
}
\value{
  An annotation object containing the iteratively computed and merged
  annotations.
}
\examples{
## A simple text.
s <- String("  First sentence.  Second sentence.  ")
##           ****5****0****5****0****5****0****5**

## A very trivial sentence tokenizer.
sent_tokenizer <-
function(s) {
    s <- as.String(s)
    m <- gregexpr("[^[:space:]][^.]*\\\\.", s)[[1L]]
    Span(m, m + attr(m, "match.length") - 1L)
}
## (Could also use Regexp_Tokenizer() with the above regexp pattern.)
## A simple sentence token annotator based on the sentence tokenizer.
sent_token_annotator <- Simple_Sent_Token_Annotator(sent_tokenizer)

## Annotate sentence tokens.
a1 <- annotate(s, sent_token_annotator)
a1

## A very trivial word tokenizer.
word_tokenizer <-
function(s) {
    s <- as.String(s)
    ## Remove the last character (should be a period when using
    ## sentences determined with the trivial sentence tokenizer).
    s <- substring(s, 1L, nchar(s) - 1L)
    ## Split on whitespace separators.
    m <- gregexpr("[^[:space:]]+", s)[[1L]]
    Span(m, m + attr(m, "match.length") - 1L)
}
## A simple word token annotator based on the word tokenizer.
word_token_annotator <- Simple_Word_Token_Annotator(word_tokenizer)

## Annotate word tokens using the already available sentence token
## annotations.
a2 <- annotate(s, word_token_annotator, a1)
a2

## Can also perform sentence and word token annotations in a pipeline:
annotate(s, list(sent_token_annotator, word_token_annotator))
}
