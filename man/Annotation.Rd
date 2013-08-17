\name{Annotation}
\alias{Annotation}
\alias{as.Annotation}
\alias{as.Annotation.Span}
\alias{is.Annotation}
\alias{[.Annotation}
%% \alias{[<-.Annotation}
\alias{[[.Annotation}
%% \alias{[[<-.Annotation}
\alias{$<-.Annotation}
\alias{as.data.frame.Annotation}
\alias{as.list.Annotation}
\alias{c.Annotation}
\alias{duplicated.Annotation}
\alias{format.Annotation}
\alias{length.Annotation}
\alias{merge.Annotation}
\alias{names.Annotation}
\alias{print.Annotation}
\alias{subset.Annotation}
\alias{unique.Annotation}
\title{Annotation objects}
\description{
  Creation and manipulation of annotation objects.
}
\usage{
Annotation(id = NULL, type = NULL, start, end, features = NULL)
as.Annotation(x, ...)
\method{as.Annotation}{Span}(x, id = NULL, type = NULL, ...)
is.Annotation(x)
}
\arguments{
  \item{id}{an integer vector giving the annotation ids, or \code{NULL}
    (default) resulting in missing ids.}
  \item{type}{a character vector giving the annotation types, or
    \code{NULL} (default) resulting in missing types.}
  \item{start, end}{integer vectors giving the start and end positions
    of the character spans the annotations refer to.}
  \item{features}{a list of (named or empty) features lists, or
    \code{NULL} (default), resulting in empty feature lists.}
  \item{x}{an \R object (an object of class \code{"\link{Span}"} for the
    coercion methods for such objects).}
  \item{...}{further arguments passed to or from other methods}
}
\details{
  A single annotation (of natural language text) is a quintuple with
  \dQuote{slots} \sQuote{id}, \sQuote{type}, \sQuote{start},
  \sQuote{end}, and \sQuote{features}.  These give, respectively, id and
  type, the character span the annotation refers to, and a collection of
  features (tag/value pairs).

  Annotation objects provide sequences (allowing positional access) of
  single annotations.  They have class \code{"Annotation"} and, as they
  contain character spans, also inherit from class \code{"\link{Span}"}.
  Span objects can be coerced to annotation objects via
  \code{as.Annotation} which allows to specify ids and types (using the
  default values sets these to missing), and annotation objects can be
  coerced to span objects using \code{\link{as.Span}}.

  The features of a single annotation are represented as named or empty
  lists.

  Subscripting annotation objects via \code{[} extracts subsets of
  annotations; subscripting via \code{$} extracts the sequence of values
  of the named slot, i.e., an integer vector for \sQuote{id},
  \sQuote{start}, and \sQuote{end}, a character vector for
  \sQuote{type}, and a list of named or empty lists for
  \sQuote{feature}.

  There are several additional methods for class \code{"Annotation"}:
  \code{print} and \code{format}; \code{c} combines annotations;
  \code{merge} merges annotations by combining the feature lists of
  annotations with otherwise identical slots; \code{subset} allows
  subsetting by expressions involving the slot names; and \code{as.list}
  and \code{as.data.frame} coerce to lists (of single annotation
  objects) and data frames (with annotations and slots corresponding to
  rows and columns).

  \code{Annotation()} creates annotation objects from the given sequences
  of slot values: those not \code{NULL} must all have the same length
  (the number of annotations in the object).

  \code{as.Annotation()} coerces to annotation objects, with a method
  for span objects.

  \code{is.Annotation()} tests whether an object inherits from class
  \code{"Annotation"}.
}
\value{
  For \code{Annotation()} and \code{as.Annotation()}, an annotation
  object (of class \code{"Annotation"} also inheriting from class
  \code{"Span"}).

  For \code{is.Annotation}, a logical.
}
\examples{
## A simple text.
s <- String("  First sentence.  Second sentence.  ")
##           ****5****0****5****0****5****0****5**

## Basic sentence and word token annotations for the text.
a1s <- Annotation(1 : 2,
                  rep.int("sentence", 2L),
                  c( 3L, 20L),
                  c(17L, 35L))
a1w <- Annotation(3 : 6,
                  rep.int("word", 4L),
                  c( 3L,  9L, 20L, 27L),
                  c( 7L, 16L, 25L, 34L))

## Use c() to combine these annotations:
a1 <- c(a1s, a1w)
a1
## Subscripting via '[':
a1[3 : 4]
## Subscripting via '$':
a1$type
## Subsetting according to slot values, directly:
a1[a1$type == "word"]
## or using subset():
subset(a1, type == "word")

## We can subscript string objects by annotation objects to extract the
## annotated substrings:
s[subset(a1, type == "word")]
## We can also subscript by lists of annotation objects:
s[annotations_in_spans(subset(a1, type == "word"),
                       subset(a1, type == "sentence"))]

## Suppose we want to add the sentence constituents (the ids of the
## words in the respective sentences) to the features of the sentence
## annotations.  The basic computation is
lapply(annotations_in_spans(a1[a1$type == "word"],
                            a1[a1$type == "sentence"]),
       function(a) a$id)
## For annotations, we need lists of feature lists:
features <-
    lapply(annotations_in_spans(a1[a1$type == "word"],
                                a1[a1$type == "sentence"]),
           function(e) list(constituents = e$id))
## Could add these directly:
a2 <- a1
a2$features[a2$type == "sentence"] <- features
a2
## Note how the print() method summarizes the features.
## We could also write a sentence constituent annotator
## (note that annotators should always have formals 's' and 'a', even
## though for computing the sentence constituents s is not needed):
sent_constituent_annotator <-
Annotator(function(s, a) {
              i <- which(a$type == "sentence")
              features <-
                  lapply(annotations_in_spans(a[a$type == "word"],
                                              a[i]),
                        function(e) list(constituents = e$id))
              Annotation(a$id[i], a$type[i], a$start[i], a$end[i],
                         features)
         })
sent_constituent_annotator(s, a1)
## Can use merge() to merge the annotations:
a2 <- merge(a1, sent_constituent_annotator(s, a1))
a2
## Equivalently, could have used
a2 <- annotate(s, sent_constituent_annotator, a1)
a2
## which merges automatically.
}
