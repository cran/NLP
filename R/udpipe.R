## Viewer methods for objects of class "udpipe_connlu" as obtained by
## udpipe::udpipe_annotate().

## <FIXME>
## All methods will need the udpipe namespace loaded to use the
## as.data.frame() method for class "udpipe_connlu".
## Should we check for this?
## Perhaps simply call loadNamespace("udpipe") in the methods?
## </FIXME>

words.udpipe_connlu <-
function(x, ...)
{
    x <- as.data.frame(x)
    x$token
}

sents.udpipe_connlu <-
function(x, ...)
{
    x <- as.data.frame(x)
    split(x$token, x$sentence_id)
}

paras.udpipe_connlu <-
function(x, ...)
{
    x <- as.data.frame(x)
    lapply(split(x, x$paragraph_id),
           function(e) split(e$token, e$sentence_id))
}

tagged_words.udpipe_connlu <-
function(x, which = c("upos", "xpos"), ...)
{
    x <- as.data.frame(x)
    which <- match.arg(which)
    Tagged_Token(x$token, x[[which]])
}

tagged_sents.udpipe_connlu <-
function(x, which = c("upos", "xpos"), ...)
{
    x <- as.data.frame(x)
    which <- match.arg(which)
    .tagged_sents_from_conllu_frame(x, which)
}

.tagged_sents_from_conllu_frame <-
function(x, which)
{
    lapply(split(x, x$sentence_id),
           function(e) Tagged_Token(e$token, e[[which]]))
}
    
tagged_paras.udpipe_connlu <-
function(x, which = c("upos", "xpos"), ...)
{
    x <- as.data.frame(x)
    which <- match.arg(which)
    lapply(split(x, x$paragraph_id),
           .tagged_sents_from_conllu_frame,
           which)
}
