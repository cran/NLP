\name{datetime}
\alias{parse_ISO_8601_datetime}
\title{Parse ISO 8601 Date/Time Strings}
\description{
  Extract date/time components from strings following one of the six
  formats specified in the NOTE-datetime ISO 8601 profile
  (\url{http://www.w3.org/TR/NOTE-datetime}).
}
\arguments{
  \item{x}{a character vector.}
}
\details{
  For character strings in one of the formats in the profile, the
  corresponding date/time components are extracted, with seconds and
  decimal fractions of seconds combined.  Other (malformed) strings are
  warned about.

  The extracted components for each string are gathered into a named
  list with elements of the appropriate type (integer for year to min;
  double for sec; character for the time zone designator).  The object
  returned is a (suitably classed) list of such named lists.  This
  internal representation may change in future versions.

  One can subscript such ISO 8601 date/time objects using \code{[} and
  extract components using \code{$} (where missing components will
  result in \code{NA}s), and convert them to the standard R date/time
  classes using \code{\link{as.Date}}, \code{\link{as.POSIXct}} and
  \code{\link{as.POSIXlt}} (incomplete elements will convert to suitably
  missing elements).  In addition, there are \code{print} and
  \code{as.data.frame} methods for such objects.
}
\value{
  An object inheriting from class \code{"ISO_8601_datetime"} with the
  extracted date/time components.
}
\examples{
## Use the examples from <http://www.w3.org/TR/NOTE-datetime>, plus one
## in UTC.
x <- c("1997",
       "1997-07",
       "1997-07-16",
       "1997-07-16T19:20+01:00",
       "1997-07-16T19:20:30+01:00",
       "1997-07-16T19:20:30.45+01:00",
       "1997-07-16T19:20:30.45Z")
y <- parse_ISO_8601_datetime(x)
y
## Conversions: note that "incomplete" elements are converted to
## "missing".
as.Date(y)
as.POSIXlt(y)
## Subscripting and extracting components:
head(y, 3)
y$mon
}
