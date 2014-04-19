WordListDocument <-
function(con, encoding = "unknown", meta = list())
{
    words <- readLines(con, encoding = encoding, warn = FALSE)
    doc <- list(content = words, meta = meta)
    class(doc) <- c("WordListDocument", "TextDocument")
    doc
}

print.WordListDocument <-
function(x, ...)
{
    writeLines(sprintf("<<WordListDocument (words: %d)>>",
                       length(x$content)))
    invisible(x)
}

content.WordListDocument <-
function(x)
    x$content

meta.WordListDocument <-
function(x, tag = NULL, ...)
    if(is.null(tag)) x$meta else x$meta[[tag]]

as.character.WordListDocument <-
words.WordListDocument <-
function(x, ...)
    x$content
