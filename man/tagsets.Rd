\name{tagsets}
\alias{Penn_Treebank_POS_tags}
\alias{Brown_POS_tags}
\alias{Universal_POS_tags}
\alias{Universal_POS_tags_map}
\title{NLP Tag Sets}
\description{
  Tag sets frequently used in Natural Language Processing.
}
\usage{
Penn_Treebank_POS_tags
Brown_POS_tags
Universal_POS_tags
Universal_POS_tags_map
}
\details{
  \code{Penn_Treebank_POS_tags} and \code{Brown_POS_tags} provide,
  respectively, the Penn Treebank POS tags
  (\url{https://catalog.ldc.upenn.edu/docs/LDC95T7/cl93.html}, Table 2)
  and the POS tags used for the Brown corpus
  (\url{https://en.wikipedia.org/wiki/Brown_Corpus}),
  both as data frames with the following variables:
  \describe{
    \item{entry}{a character vector with the POS tags}
    \item{description}{a character vector with short descriptions of the
      tags}
    \item{examples}{a character vector with examples for the tags}
  }

  \code{Universal_POS_tags} provides the universal POS tagset introduced
  by Slav Petrov, Dipanjan Das, and Ryan McDonald
  (\doi{10.48550/arXiv.1104.2086}), as a data frame with character
  variables \code{entry} and \code{description}.

  \code{Universal_POS_tags_map} is a named list of mappings from
  language and treebank specific POS tagsets to the universal POS tags,
  with elements named \samp{en-ptb} and \samp{en-brown} giving the
  mappings, respectively, for the Penn Treebank and Brown POS tags.
}
\source{
  \url{https://catalog.ldc.upenn.edu/docs/LDC95T7/cl93.html},
  % As of 2024-07, \url{http://www.hit.uib.no/icame/brown/bcm.html} no
  % longer works, so use the download info from Wikipedia.
  \url{http://www.nltk.org/nltk_data/},
  \url{https://github.com/slavpetrov/universal-pos-tags}.
}
\examples{
## Penn Treebank POS tags
dim(Penn_Treebank_POS_tags)
## Inspect first 20 entries:
write.dcf(head(Penn_Treebank_POS_tags, 20L))

## Brown POS tags
dim(Brown_POS_tags)
## Inspect first 20 entries:
write.dcf(head(Brown_POS_tags, 20L))

## Universal POS tags
Universal_POS_tags

## Available mappings to universal POS tags
names(Universal_POS_tags_map)
}
