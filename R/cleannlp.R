## Viewer methods for objects of class "cnlp_annotation" as obtained by
## cleanNLP::cnlp_annotate().

words.cnlp_annotation <-
function(x, ...)
{
    x$token$token
}

sents.cnlp_annotation <-
function(x, ...)
{
    x <- x$token
    split(x$token, x$sid)
}

## paras.cnlp_annotation <-
## function(x, ...)
## {
##     x <- x$token
##     if(is.na(match("pid", names(x))))
##         stop("unavailable paragraph ids")
##     lapply(split(x, x$pid),
##            function(e) split(e$token, e$sid))
## }

tagged_words.cnlp_annotation <-
function(x, which = c("upos", "pos"), ...)
{
    x <- x$token
    which <- match.arg(which)
    Tagged_Token(x$token, x[[which]])
}

tagged_sents.cnlp_annotation <-
function(x, which = c("upos", "pos"), ...)
{
    x <- x$token
    which <- match.arg(which)
    .tagged_sents_from_cnlp_token_frame(x, which)
}

.tagged_sents_from_cnlp_token_frame <-
function(x, which)
{
    lapply(split(x, x$sid),
           function(e) Tagged_Token(e$token, e[[which]]))
}

## tagged_paras.cnlp_annotation <-
## function(x, which = c("upos", "pos"), ...)
## {
##     x <- x$token
##     if(is.na(match("pid", names(x))))
##         stop("unavailable paragraph ids")
##     which <- match.arg(which)
##     lapply(split(x, x$pid),
##            .tagged_sents_from_cnlp_token_frame,
##            which)
## }

