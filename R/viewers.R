words <-
function(x, ...)
    UseMethod("words")

sents <-
function(x, ...)
    UseMethod("sents")

paras <-
function(x, ...)
    UseMethod("paras")

tagged_words <-
function(x, ...)
    UseMethod("tagged_words")

tagged_sents <-
function(x, ...)
    UseMethod("tagged_sents")

tagged_paras <-
function(x, ...)
    UseMethod("tagged_paras")

chunked_sents <-
function(x, ...)
    UseMethod("chunked_sents")

parsed_sents <-
function(x, ...)
    UseMethod("parsed_sents")

parsed_paras <-
function(x, ...)
    UseMethod("parsed_paras")


chunk_tree_from_chunk_info <-
function(words, ptags, ctags)
{
    ind <- grepl("^[BO]", ctags)
    chunks <- split(sprintf("%s/%s", words, ptags), cumsum(ind))
    nms <- sub(".*-", "", ctags[ind])
    ind <- nms != "O"
    chunks[ind] <- Map(Tree, nms[ind], chunks[ind])
    Tree("S", chunks)
}
