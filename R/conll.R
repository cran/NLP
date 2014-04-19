CoNLLTextDocument <-
function(con, encoding = "unknown", meta = list())
{
    ## <NOTE>
    ## Could make the fields controllable, e.g.
    ##   CoNLLDocument(con, fields = c("word", "POS", "chunk_tag"))
    ## and use "" for something to be skipped.
    ## </NOTE>

    records <- scan(con, what = list("", "", ""), encoding = encoding,
                    quote = NULL, quiet = TRUE, fill = TRUE,
                    blank.lines.skip = FALSE)
    words <- records[[1L]]
    ind <- words == ""

    doc <- list(content =
                data.frame(sent = cumsum(ind) + 1L,
                           word = words,
                           POS = records[[2L]],
                           chunk_tag = records[[3L]],
                           stringsAsFactors = FALSE)[!ind, ],
                meta = meta)
    class(doc) <- c("CoNLLTextDocument", "TextDocument")
    doc
}

print.CoNLLTextDocument <-
function(x, ...)
{
    content <- x$content
    nr <- NROW(content)
    writeLines(sprintf("<<CoNLLTextDocument (words: %d, sents: %d)>>",
                       nr, content[[nr, "sent"]]))
    invisible(x)
}

content.CoNLLTextDocument <-
function(x)
    x$content

meta.CoNLLTextDocument <-
function(x, tag = NULL, ...)
    if(is.null(tag)) x$meta else x$meta[[tag]]

as.character.CoNLLTextDocument <-
words.CoNLLTextDocument <-
function(x, ...)
{
    x$content$word
}

sents.CoNLLTextDocument <-
function(x, ...)
{
    split(x$content$word,
          x$content$sent)
}

tagged_words.CoNLLTextDocument <-
function(x, ...)
{
    sprintf("%s/%s", x$content$word, x$content$POS)
}

tagged_sents.CoNLLTextDocument <-
function(x, ...)
{
    split(sprintf("%s/%s", x$content$word, x$content$POS),
          x$content$sent)
}

chunked_sents.CoNLLTextDocument <-
function(x, ...)
{
    Map(chunk_tree_from_chunk_info,
        split(x$content$word, x$content$sent),
        split(x$content$POS, x$content$sent),
        split(x$content$chunk_tag, x$content$sent))
}
