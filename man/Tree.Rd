\name{Tree}
\alias{Tree}
\alias{format.Tree}
\alias{print.Tree}
\alias{Tree_parse}
\alias{Tree_apply}
\title{Tree objects}
\description{Creation and manipulation of tree objects.}
\usage{
Tree(value, children = list())
\method{format}{Tree}(x, width = 0.9 * getOption("width"), indent = 0,
       brackets = c("(", ")"), ...)
Tree_parse(x, brackets = c("(", ")"))
Tree_apply(x, f, recursive = FALSE)
}
\arguments{
  \item{value}{a (non-tree) node value of the tree.}
  \item{children}{a list giving the children of the tree.}
  \item{x}{a tree object for the \code{format()} method and
    \code{Tree_apply()}; a character string for \code{Tree_parse()}.}
  \item{width}{a positive integer giving the target column for a
    single-line nested bracketting.}
  \item{indent}{a non-negative integer giving the indentation used for
    formatting.}
  \item{brackets}{a character vector of length two giving the pair of
    opening and closing brackets to be employed for formatting or
    parsing.}
  \item{...}{further arguments passed to or from other methods.}
  \item{f}{a function to be applied to the children nodes.}
  \item{recursive}{a logical indicating whether to apply \code{f}
    recursively to the children of the children and so forth.}
}
\details{
  Trees give hierarchical groupings of leaves and subtrees, starting
  from the root node of the tree.  In natural language processing, the
  syntactic structure of sentences is typically represented by parse
  trees (e.g., \url{https://en.wikipedia.org/wiki/Concrete_syntax_tree})
  and displayed using nested brackettings.

  The tree objects in package \pkg{NLP} are patterned after the ones in
  NLTK (\url{https://www.nltk.org}), and primarily designed for representing
  parse trees.  A tree object consists of the value of the root node and
  its children as a list of leaves and subtrees, where the leaves are
  elements with arbitrary non-tree values (and not subtrees with no
  children).  The value and children can be extracted via \code{$}
  subscripting using names \code{value} and \code{children},
  respectively.

  There is a \code{format()} method for tree objects: this first tries a
  nested bracketting in a single line of the given width, and if this is
  not possible, produces a nested indented bracketting.  The
  \code{print()} method uses the \code{format()} method, and hence its
  arguments to control the formatting.

  \code{Tree_parse()} reads nested brackettings into a tree object.
}
\examples{
x <- Tree(1, list(2, Tree(3, list(4)), 5))
format(x)
x$value
x$children

p <- Tree("VP",
          list(Tree("V",
                    list("saw")),
               Tree("NP",
                    list("him"))))
p <- Tree("S",
          list(Tree("NP",
                    list("I")),
               p))
p
## Force nested indented bracketting:
print(p, width = 10)

s <- "(S (NP I) (VP (V saw) (NP him)))"
p <- Tree_parse(s)
p

## Extract the leaves by recursively traversing the children and
## recording the non-tree ones:
Tree_leaf_gatherer <-
function()
{
    v <- list()
    list(update =
         function(e) if(!inherits(e, "Tree")) v <<- c(v, list(e)),
         value = function() v,
         reset = function() { v <<- list() })
}
g <- Tree_leaf_gatherer()
y <- Tree_apply(p, g$update, recursive = TRUE)
g$value()
}
