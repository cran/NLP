\name{language}
\alias{parse_IETF_language_tag}
\title{Parse IETF Language Tag}
\description{
  Extract language, script, region and variant subtags from IETF
  language tags.
}
\usage{parse_IETF_language_tag(x, expand = FALSE)}
\arguments{
  \item{x}{a character vector with IETF language tags.}
  \item{expand}{a logical indicating whether to expand subtags into
    their description(s).}
}
\details{
  Internet Engineering Task Force (IETF) language tags are defined by
  \href{https://www.rfc-editor.org/info/bcp47}{IETF BCP 47},
  which is currently composed by the normative
  \href{https://datatracker.ietf.org/doc/html/rfc5646}{RFC 5646} and
  \href{https://datatracker.ietf.org/doc/html/rfc4647}{RFC 4647},
  along with the normative content of the
  \href{https://www.iana.org/assignments/language-subtag-registry/}{IANA Language Subtag Registry}
  regulated by these RFCs.
  These tags are used in a number of modern computing standards.

  Each language tag is composed of one or more \dQuote{subtags}
  separated by hyphens.  Normal language tags have the following
  subtags:
  \itemize{
    \item a language subtag (optionally, with language extension
    subtags),
    \item an optional script subtag,
    \item an optional region subtag,
    \item optional variant subtags,
    \item optional extension subtags,
    \item an optional private use subtag.
  }
  Language subtags are mainly derived from ISO 639-1 and ISO 639-2,
  script subtags from ISO 15924, and region subtags from ISO 3166-1
  alpha-2 and UN M.49, see package \pkg{ISOcodes} for more information
  about these standards.  Variant subtags are not derived from any
  standard.  The Language Subtag Registry
  (\url{https://www.iana.org/assignments/language-subtag-registry}),
  maintained by the Internet Assigned Numbers Authority (IANA), lists
  the current valid public subtags, as well as the so-called
  \dQuote{grandfathered} language tags.

  See \url{https://en.wikipedia.org/wiki/IETF_language_tag} for more
  information.
}
\value{
  If \code{expand} is false, a list of character vectors of the form
  \code{"\var{type}=\var{subtag}"}, where \var{type} gives the type of
  the corresponding subtag (one of \sQuote{Language}, \sQuote{Extlang},
  \sQuote{Script}, \sQuote{Region}, \sQuote{Variant}, or
  \sQuote{Extension}), or \code{"\var{type}=\var{tag}"} with \var{type}
  either \sQuote{Privateuse} or \sQuote{Grandfathered}.

  Otherwise, a list of lists of character vectors obtained by replacing
  the subtags by their corresponding descriptions (which may be
  multiple) from the IANA registry.  Note that no such descriptions for
  Extension and Privateuse subtags are available in the registry; on the
  other hand, empty expansions of the other subtags indicate malformed
  tags (as these subtags must be available in the registry).
}
\examples{
## German as used in Switzerland:
parse_IETF_language_tag("de-CH")
## Serbian written using Latin script as used in Serbia and Montenegro:
parse_IETF_language_tag("sr-Latn-CS")
## Spanish appropriate to the UN Latin American and Caribbean region:
parse_IETF_language_tag("es-419")
## All in one:
parse_IETF_language_tag(c("de-CH", "sr-Latn-CS", "es-419"))
parse_IETF_language_tag(c("de-CH", "sr-Latn-CS", "es-419"),
                        expand = TRUE)
## Two grandfathered tags:
parse_IETF_language_tag(c("i-klingon", "zh-min-nan"),
                        expand = TRUE)
}
\keyword{utilities}
