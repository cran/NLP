CoNLLTextDocument <-
function(con, encoding = "unknown", format = "conll00", meta = list())
{
    if(length(format) == 1L) {
        format <-
            switch(format,
                   conll00 = c(WORD = "WORD",
                               POS = "POS",
                               CHUNK = "CHUNK"),
                   conll01 = c(WORD = "WORD",
                               POS = "POS",
                               CHUNK = "CHUNK",
                               "CLAUSE"),
                   conll02 = c(WORD = "WORD",
                               NE = "NE"),
                   ## conll03 would have different fields for the German
                   ## variant
                   conllx  = c("ID",
                               WORD = "FORM",
                               "LEMMA",
                               POS = "CPOSTAG",
                               "POSTAG", "FEATS", "HEAD", 
                               "DEPREL", "PHEAD", "PDEPREL"),
                   ## Corresponding to CoNLL X (10) from 2006, also used
                   ## for conll07
                   conll09 = c("ID",
                               WORD = "FORM",
                               "LEMMA", "PLEMMA",
                               POS = "POS",
                               "PPOS", "FEAT", "PFEAT", "HEAD", "PHEAD",
                               "DEPREL", "PDEPREL", "FILLPRED", "PRED",
                               "APREDs"))
    }

    records <- scan(con, what = rep.int(list(""), length(format)),
                    encoding = encoding, quote = NULL, quiet = TRUE,
                    fill = TRUE, blank.lines.skip = FALSE)
    names(records) <- format
    ind <- (records[[1L]] == "")
    tab <- cbind(data.frame(sent = cumsum(ind) + 1L),
                 as.data.frame(do.call(cbind, records),
                               stringsAsFactors = FALSE))[!ind, ]
    attr(tab, "format") <- c("sent", format)

    doc <- list(content = tab,
                meta = meta)
    class(doc) <- c("CoNLLTextDocument", "TextDocument")
    doc
}

format.CoNLLTextDocument <-
function(x, ...)
{
    content <- x$content
    nr <- NROW(content)
    c(.format_TextDocument(x),
      sprintf("Content:  words: %d, sents: %d",
              nr,
              content[[nr, "sent"]]))
}

## print.CoNLLTextDocument <-
## function(x, ...)
## {
##     content <- x$content
##     nr <- NROW(content)
##     writeLines(sprintf("<<CoNLLTextDocument (words: %d, sents: %d)>>",
##                        nr, content[[nr, "sent"]]))
##     invisible(x)
## }

content.CoNLLTextDocument <-
function(x)
    x$content

## meta.CoNLLTextDocument <-
## function(x, tag = NULL, ...)
##     if(is.null(tag)) x$meta else x$meta[[tag]]

## `meta<-.CoNLLTextDocument` <-
## function(x, tag = NULL, ..., value)
## {
##     if(is.null(tag))
##         x$meta <- value
##     else
##         x$meta[[tag]] <- value
##     x
## }

as.character.CoNLLTextDocument <-
words.CoNLLTextDocument <-
function(x, ...)
{
    fmt <- attr(x$content, "format")
    pos <- .position_of_field(fmt, "WORD")
    x$content[[pos]]
}

sents.CoNLLTextDocument <-
function(x, ...)
{
    fmt <- attr(x$content, "format")
    pos <- .position_of_field(fmt, "WORD")
    split(x$content[[pos]],
          x$content$sent)
}

tagged_words.CoNLLTextDocument <-
function(x, map = NULL, ...)
{
    if(!is.null(map))
        x <- .map_POS_tags_CoNLLTextDocument(x, map)
    fmt <- attr(x$content, "format")
    pos_W <- .position_of_field(fmt, "WORD")
    pos_P <- .position_of_field(fmt, "POS")
    Tagged_Token(x$content[[pos_W]], x$content[[pos_P]])
}

tagged_sents.CoNLLTextDocument <-
function(x, map = NULL, ...)
{
    if(!is.null(map))
        x <- .map_POS_tags_CoNLLTextDocument(x, map)
    fmt <- attr(x$content, "format")
    pos_W <- .position_of_field(fmt, "WORD")
    pos_P <- .position_of_field(fmt, "POS")
    split(Tagged_Token(x$content[[pos_W]], x$content[[pos_P]]),
          x$content$sent)
}

chunked_sents.CoNLLTextDocument <-
function(x, ...)
{
    fmt <- attr(x$content, "format")
    pos_W <- .position_of_field(fmt, "WORD")
    pos_P <- .position_of_field(fmt, "POS")
    pos_C <- .position_of_field(fmt, "CHUNK")
    Map(chunk_tree_from_chunk_info,
        split(x$content[[pos_W]], x$content$sent),
        split(x$content[[pos_P]], x$content$sent),
        split(x$content[[pos_C]], x$content$sent))
}

.map_POS_tags_CoNLLTextDocument <-
function(x, map)
{
    map <- POS_tag_mapper(map, meta(x, "POS_tagset"))
    fmt <- attr(x$content, "format")
    pos <- .position_of_field(fmt, "POS")    
    x$content[[pos]] <- map(x$content[[pos]])
    x
}

.position_of_field <-
function(fmt, kind)
{
    pos <- which(names(fmt) == kind)
    if(length(pos) != 1L)
        stop(gettextf("Cannot determine position of '%s'", kind),
             call. = FALSE, domain = NA)
    pos
}
