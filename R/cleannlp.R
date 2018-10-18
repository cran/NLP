## Viewer methods for objects of class "annotation" as obtained by
## cleanNLP::cnlp_annotate().

words.annotation <-
function(x, ...)
{
    x <- cleanNLP::cnlp_get_token(x)
    x$word
}

sents.annotation <-
function(x, ...)
{
    x <- cleanNLP::cnlp_get_token(x)
    split(x$word, x$sid)
}

paras.annotation <-
function(x, ...)
{
    x <- cleanNLP::cnlp_get_token(x)
    if(is.na(match("pid", names(x))))
        stop("unavailable paragraph ids")
    lapply(split(x, x$pid),
           function(e) split(e$word, e$sid))
}

tagged_words.annotation <-
function(x, which = c("upos", "pos"), ...)
{
    x <- cleanNLP::cnlp_get_token(x)
    which <- match.arg(which)
    Tagged_Token(x$word, x[[which]])
}

tagged_sents.annotation <-
function(x, which = c("upos", "pos"), ...)
{
    x <- cleanNLP::cnlp_get_token(x)
    which <- match.arg(which)
    .tagged_sents_from_cnlp_token_frame(x, which)
}

.tagged_sents_from_cnlp_token_frame <-
function(x, which)
{
    lapply(split(x, x$sid),
           function(e) Tagged_Token(e$word, e[[which]]))
}

tagged_paras.annotation <-
function(x, which = c("upos", "pos"), ...)
{
    x <- cleanNLP::cnlp_get_token(x)
    if(is.na(match("pid", names(x))))
        stop("unavailable paragraph ids")
    which <- match.arg(which)
    lapply(split(x, x$pid),
           .tagged_sents_from_cnlp_token_frame,
           which)
}

