## Viewer methods for objects of class "spacyr_parsed" as obtained by
## spacyr::spacy_parse().

words.spacyr_parsed <-
function(x, ...)
{
    x$token
}

sents.spacyr_parsed <-
function(x, ...)
{
    split(x$token, x$sentence_id)
}

tagged_words.spacyr_parsed <-
function(x, which = c("pos", "tag"), ...)
{
    which <- match.arg(which)
    Tagged_Token(x$token, x[[which]])
}

tagged_sents.spacyr_parsed <-
function(x, which = c("pos", "tag"), ...)
{
    which <- match.arg(which)
    lapply(split(x, x$sentence_id),
           function(e) Tagged_Token(e$token, e[[which]]))
}    

