## Regexp tokenizers a la NLTK.

Regexp_Tokenizer <-
function(pattern, description = NULL, invert = FALSE, ...)
{
    args <- list(...)
    f <- if(invert) {
        ## Pattern gives the separators.
        function(s) {
            s <- as.String(s)
            if(is.na(s) || !nchar(s))
                stop("Need a non-empty string.")
            m <- do.call(gregexpr,
                         c(list(pattern = pattern, text = s), args))[[1L]]
            if((length(m) == 1L) && (m == -1L))
                return(Span(1L, nchar(s)))
            start <- c(1L, m + attr(m, "match.length"))
            end <- c(m - 1L, nchar(s))
            ind <- start <= end
            Span(start[ind], end[ind])
        }
    } else {
        ## Pattern gives the tokens.
        function(s) {
            s <- as.String(s)
            if(is.na(s) || !nchar(s))
                stop("Need a non-empty string.")
            m <- do.call(gregexpr,
                         c(list(pattern = pattern, text = s), args))[[1L]]
            Span(m, m + attr(m, "match.length") - 1L)
        }
    }
    ## <NOTE>
    ## We currently store the description as an attribute: could also
    ## store it in the environment of the generated tokenizer (where it
    ## is less visible, though).
    ## Currently, no class and hence no print method nicely showing the
    ## description ...
    ## </NOTE>
    attr(f, "description") <- description
    f
}

whitespace_tokenizer <-
    Regexp_Tokenizer("\\s+",
                     paste("A tokenizer that divides strings into substrings by treating",
                           "any sequence of whitespace characters as a separator.",
                           sep = "\n"),
                     invert = TRUE)

blankline_tokenizer <-
    Regexp_Tokenizer("\\s*\n\\s*\\n\\s*",
                     paste("A tokenizer that divides strings into substrings by treating",
                           "any sequence of blank lines as a separator.",
                           sep = "\n"),
                     invert = TRUE)

wordpunct_tokenizer <-
    Regexp_Tokenizer("\\w+|[^\\w\\s]+",
                     paste("A tokenizer that divides strings into substrings of",
                           "alphabetic and (non-whitespace) non-alphabetic characters.",
                           sep = "\n"),
                     perl = TRUE)
