WordListDocument <-
function(con, encoding = "unknown", meta = list())
{
    words <- readLines(con, encoding = encoding, warn = FALSE)
    doc <- list(content = words, meta = meta)
    class(doc) <- c("WordListDocument", "TextDocument")
    doc
}

as.character.WordListDocument <-
words.WordListDocument <-
function(x, ...)
    x$content

print.WordListDocument <-
function(x, ...)
{
    writeLines(sprintf("<<WordListDocument (words: %d)>>",
                       length(x$content)))
    invisible(x)
}
               
