\name{annotators}
\alias{Simple_Para_Token_Annotator}
\alias{Simple_Sent_Token_Annotator}
\alias{Simple_Word_Token_Annotator}
\alias{Simple_POS_Tag_Annotator}
\alias{Simple_Entity_Annotator}
\alias{Simple_Chunk_Annotator}
\alias{Simple_Stem_Annotator}
\alias{Simple annotator generators}
\title{Simple annotator generators}
\description{
  Create annotator objects for composite basic NLP tasks based on
  functions performing simple basic tasks.
}
\usage{
Simple_Para_Token_Annotator(f, description = NULL, classes = NULL)
Simple_Sent_Token_Annotator(f, description = NULL, classes = NULL)
Simple_Word_Token_Annotator(f, description = NULL, classes = NULL)
Simple_POS_Tag_Annotator(f, description = NULL, classes = NULL)
Simple_Entity_Annotator(f, description = NULL, classes = NULL)
Simple_Chunk_Annotator(f, description = NULL, classes = NULL)
Simple_Stem_Annotator(f, description = NULL, classes = NULL)
}
\arguments{
  \item{f}{a function performing a \dQuote{simple} basic NLP task (see
    \bold{Details}).}
  \item{description}{a character string describing the annotator, or
    \code{NULL} (default).}
  \item{classes}{a character vector or \code{NULL} (default) giving
    classes to be used for the created annotator object in addition to
    the default ones (see \bold{Details}).}
}
\details{
  The purpose of these functions is to facilitate the creation of
  annotators for basic NLP tasks as described below.

  \code{Simple_Para_Token_Annotator()} creates \dQuote{simple} paragraph
  token annotators.  Argument \code{f} should be a paragraph tokenizer,
  which takes a string \code{s} with the whole text to be processed, and
  returns the spans of the paragraphs in \code{s}, or an annotation
  object with these spans and (possibly) additional features.  The
  generated annotator inherits from the default classes
  \code{"Simple_Para_Token_Annotator"} and \code{"Annotator"}.  It uses
  the results of the simple paragraph tokenizer to create and return
  annotations with unique ids and type \sQuote{paragraph}.
  
  \code{Simple_Sent_Token_Annotator()} creates \dQuote{simple} sentence
  token annotators.  Argument \code{f} should be a sentence tokenizer,
  which takes a string \code{s} with the whole text to be processed, and
  returns the spans of the sentences in \code{s}, or an annotation
  object with these spans and (possibly) additional features.  The
  generated annotator inherits from the default classes
  \code{"Simple_Sent_Token_Annotator"} and \code{"Annotator"}.  It uses
  the results of the simple sentence tokenizer to create and return
  annotations with unique ids and type \sQuote{sentence}, possibly
  combined with sentence constituent features for already available
  paragraph annotations.

  \code{Simple_Word_Token_Annotator()} creates \dQuote{simple} word
  token annotators.  Argument \code{f} should be a simple word
  tokenizer, which takes a string \code{s} giving a sentence to be
  processed, and returns the spans of the word tokens in \code{s}, or an 
  annotation object with these spans and (possibly) additional features.
  The generated annotator inherits from the default classes
  \code{"Simple_Word_Token_Annotator"} and \code{"Annotator"}.
  It uses already available sentence token annotations to extract the
  sentences and obtains the results of the word tokenizer for these.  It
  then adds the sentence character offsets and unique word token ids,
  and word token constituents features for the sentences, and returns
  the word token annotations combined with the augmented sentence token
  annotations.

  \code{Simple_POS_Tag_Annotator()} creates \dQuote{simple} POS tag
  annotators.  Argument \code{f} should be a simple POS tagger, which
  takes a character vector giving the word tokens in a sentence, and
  returns either a character vector with the tags, or a list of feature
  maps with the tags as \sQuote{POS} feature and possibly other
  features.  The generated annotator inherits from the default classes
  \code{"Simple_POS_Tag_Annotator"} and \code{"Annotator"}.  It uses
  already available sentence and word token annotations to extract the
  word tokens for each sentence and obtains the results of the simple
  POS tagger for these, and returns annotations for the word tokens with
  the features obtained from the POS tagger.

  \code{Simple_Entity_Annotator()} creates \dQuote{simple} entity
  annotators.  Argument \code{f} should be a simple entity detector
  (\dQuote{named entity recognizer}) which takes a character vector
  giving the word tokens in a sentence, and return an annotation object
  with the \emph{word} token spans, a \sQuote{kind} feature giving the
  kind of the entity detected, and possibly other features.  The
  generated annotator inherits from the default classes
  \code{"Simple_Entity_Annotator"} and \code{"Annotator"}.  It uses
  already available sentence and word token annotations to extract the
  word tokens for each sentence and obtains the results of the simple
  entity detector for these, transforms word token spans to character
  spans and adds unique ids, and returns the combined entity
  annotations.
  
  \code{Simple_Chunk_Annotator()} creates \dQuote{simple} chunk
  annotators.  Argument \code{f} should be a simple chunker, which takes
  as arguments character vectors giving the word tokens and the
  corresponding POS tags, and returns either a character vector with the
  chunk tags, or a list of feature lists with the tags as
  \sQuote{chunk_tag} feature and possibly other features.  The generated
  annotator inherits from the default classes
  \code{"Simple_Chunk_Annotator"} and \code{"Annotator"}.  It uses
  already available annotations to extract the word tokens and POS tags
  for each sentence and obtains the results of the simple chunker for
  these, and returns word token annotations with the chunk features
  (only).

  \code{Simple_Stem_Annotator()} creates \dQuote{simple} stem
  annotators.  Argument \code{f} should be a simple stemmer, which takes
  as arguments a character vector giving the word tokens, and returns a
  character vector with the corresponding word stems.  The generated
  annotator inherits from the default classes
  \code{"Simple_Stem_Annotator"} and \code{"Annotator"}.  It uses
  already available annotations to extract the word tokens, and returns
  word token annotations with the corresponding stem features (only).

  In all cases, if the underlying simple processing function returns
  annotation objects these should not provide their own ids (or use such
  in the features), as the generated annotators will necessarily provide
  these (the already available annotations are only available at the
  annotator level, but not at the simple processing level).
}
\value{
  An annotator object inheriting from the given classes and the default
  ones.
}
\seealso{
  Package \pkg{openNLP} which provides annotator generators for sentence
  and word tokens, POS tags, entities and chunks, using processing
  functions based on the respective Apache OpenNLP MaxEnt processing
  resources.
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
sent_tokenizer(s)
## A simple sentence token annotator based on the sentence tokenizer.
sent_token_annotator <- Simple_Sent_Token_Annotator(sent_tokenizer)
sent_token_annotator
a1 <- annotate(s, sent_token_annotator)
a1
## Extract the sentence tokens.
s[a1]

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
lapply(s[a1], word_tokenizer)
## A simple word token annotator based on the word tokenizer.
word_token_annotator <- Simple_Word_Token_Annotator(word_tokenizer)
word_token_annotator
a2 <- annotate(s, word_token_annotator, a1)
a2
## Extract the word tokens.
s[subset(a2, type == "word")]

## A simple word token annotator based on wordpunct_tokenizer():
word_token_annotator <-
    Simple_Word_Token_Annotator(wordpunct_tokenizer,
                                "Based on wordpunct_tokenizer().")
word_token_annotator
a2 <- annotate(s, word_token_annotator, a1)
a2
## Extract the word tokens.
s[subset(a2, type == "word")]
}
