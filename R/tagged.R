## <TODO>
## Add argument 'map'.
## </TODO>

TaggedTextDocument <-
function(con, encoding = "unknown",
         word_tokenizer = whitespace_tokenizer,
         sent_tokenizer = Regexp_Tokenizer("\n", invert = TRUE),
         para_tokenizer = blankline_tokenizer,
         sep = "/",
         meta = list())
{
    s <- String(paste(readLines(con, encoding = encoding, warn = FALSE),
                      collapse = "\n"))
    paras <- if(!is.null(para_tokenizer))
        s[para_tokenizer(s)]
    else
        as.character(s)

    x <- lapply(paras,
                function(para) {
                    ## Avoid as.String() coercion.
                    spans <- sent_tokenizer(para)
                    sents <- substring(para, spans$start, spans$end)
                    lapply(sents,
                           function(sent) {
                               spans <- word_tokenizer(sent)
                               words <-
                                   substring(sent, spans$start, spans$end)
                               toks <- strsplit(words, sep, fixed = TRUE)
                               data.frame(word =
                                          sapply(toks, `[[`, 1L),
                                          POS =
                                          toupper(sapply(toks, `[[`, 2L)),
                                          stringsAsFactors = FALSE)
                           })
                })

    ## Use sentence ids which are unique across paras.

    lens <- lapply(x, length)
    ids <- Map(function(f, l)
               as.integer(seq(from = f, length.out = l)),
               c(0L, head(cumsum(lens), -1L)) + 1L,
               lens)

    x <- Map(function(u, v) {
        cbind(data.frame(sent = rep.int(u, sapply(v, nrow))),
              do.call(rbind, v))
    },
        ids, x)

    doc <- list(content = x, meta = meta)
    class(doc) <- c("TaggedTextDocument", "TextDocument")

    doc
}

print.TaggedTextDocument <-
function(x, ...)
{
    content <- x$content
    len <- length(content)
    writeLines(sprintf("<<TaggedTextDocument (words: %d, sents: %d, paras: %d)>>",
                       sum(sapply(content, NROW)),
                       tail(content[[len]]$sent, 1L),
                       len))
    invisible(x)
}

## <NOTE>
## It would be nice if the as.character() method could "suitably"
## detokenize the word tokens into sentences.  But this requires
## (a) knowing at least the language of the text
## (b) having code to detokenize when knowing the language ...
## </NOTE>

as.character.TaggedTextDocument <-
words.TaggedTextDocument <-
function(x, ...)
{
    unlist(lapply(x$content, `[[`, "word"))
}

## <NOTE>
## Could more simply do
##   sents.TaggedTextDocument <- function(x, ...)
##       unlist(paras(x), recursive = FALSE)
## </NOTE>

sents.TaggedTextDocument <-
function(x, ...)
{
    unlist(lapply(x$content,
                  function(e) split(e$word, e$sent)),
           recursive = FALSE)
}

paras.TaggedTextDocument <-
function(x, ...)
{
    lapply(x$content,
           function(e) split(e$word, e$sent))
}

tagged_words.TaggedTextDocument <-
function(x, ...)
{
    unlist(lapply(x$content,
                  function(e) sprintf("%s/%s", e$word, e$POS)))
}

## <NOTE>
## Could more simply do
##   tagged_sents.TaggedTextDocument <- function(x, ...)
##       unlist(tagged_paras(x), recursive = FALSE)
## </NOTE>

tagged_sents.TaggedTextDocument <-
function(x, ...)
{
    unlist(lapply(x$content,
                  function(e)
                  split(sprintf("%s/%s", e$word, e$POS), e$sent)),
           recursive = FALSE)
}
    
tagged_paras.TaggedTextDocument <-
function(x, ...)
{
    lapply(x$content,
           function(e)
           split(sprintf("%s/%s", e$word, e$POS), e$sent))
}
