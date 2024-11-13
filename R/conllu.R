## See <http://universaldependencies.org/format.html>.

read_CoNNLU <-
function(con)
{
    lines <- readLines(con, encoding = "UTF-8")
    ind_b <- lines == ""
    ind_c <- startsWith(lines, "#")
    ind <- !ind_b & !ind_c

    ## Now using scan(text = lines[ind]) to read in the records is
    ## possible but unbearably slow for large documents: instead, try to
    ## proceed "directly".

    ## records <- strsplit(lines[ind], "\t", fixed = TRUE)
    ## records <- as.data.frame(do.call(rbind, records),
    ##                          stringsAsFactors = FALSE)

    ## names(records) <-
    ##     c("ID", "FORM", "LEMMA", "UPOSTAG", "XPOSTAG", "FEATS", "HEAD", 
    ##       "DEPREL", "DEPS", "MISC")
    ## sent <- cumsum(ind_b) + 1L
    ## tab <- cbind(data.frame(sent = sent),
    ##              as.data.frame(do.call(cbind, records),
    ##                            stringsAsFactors = FALSE))[ind , ]

    pos <- which(ind_b)
    sent <- rep.int(seq_along(pos), diff(c(0L, pos)))
    tab <- cbind(data.frame(sent[ind]),
                 as.data.frame(do.call(rbind,
                                       strsplit(lines[ind], "\t",
                                                fixed = TRUE)),
                               stringsAsFactors = FALSE))
    names(tab) <-
        c("sent",
          "ID", "FORM", "LEMMA", "UPOSTAG", "XPOSTAG", "FEATS", "HEAD", 
          "DEPREL", "DEPS", "MISC")
    
    comments <- split(lines[ind_c], sent[ind_c])
    attr(tab, "comments") <- comments
    ## From CoNLL-U v2 on 'sent_id' and 'text' comments are compulsory
    ## for every sentence.  Be defensive and add these as attributes
    ## only if always available.
    ind <- startsWith(lines, "# sent_id =")
    if(all(diff(sent[ind]) == 1))
        attr(tab, "sent_id") <-
            sub("^# sent_id = *", "", lines[ind])
    ind <- startsWith(lines, "# text =")
    if(all(diff(sent[ind]) == 1))
        attr(tab, "text") <-
            sub("^# text = *", "", lines[ind])

    class(tab) <- c("CoNNLU_Annotation", "data.frame")
    tab
}

CoNLLUTextDocument <-
function(con, meta = list(), text = NULL)    
{
    tab <- read_CoNNLU(con)
    doc <- list(content = tab,
                meta = meta,
                text = if(is.null(text))
                           attr(tab, "text")
                       else text)
    class(doc) <- c("CoNLLUTextDocument", "TextDocument")
    doc
}

## CoNLL-U allows to represent both words and (multiword) tokens, which
## both have FORM entries, with ID single integers for words and integer
## ranges for the tokens.  We provide the tokens with as.character() and
## the words with the other "viewers", in particular, words().

format.CoNLLUTextDocument <-
function(x, ...)    
{
    y <- x$content
    ind <- !grepl("[.-]", y$ID)
    c(.format_TextDocument(x),
      sprintf("Content:  words: %d, sents: %d",
              sum(ind),
              y[NROW(y), "sent"]))
}
    
content.CoNLLUTextDocument <-
function(x)
    x$content

as.character.CoNLLUTextDocument <-
function(x, ...)
{
    if(!is.null(y <- x$text))
        y
    else
        otoks(x)
}

## <NOTE>
## All methods below could also be provided for CoNNLU_Annotation
## objects. 
## </NOTE>
    
otoks.CoNLLUTextDocument <-
function(x, ...)    
{
    y <- x$content
    ## Drop empty nodes.
    y <- y[!grepl(".", y$ID, fixed = TRUE), ]
    ## Expand ranges to determine forms to be skipped for tokens.
    ind <- grepl("-", y$ID, fixed = TRUE)
    ids <- y$ID[ind]
    skip <- Map(seq, sub("-.*", "", ids), sub(".*-", "", ids))
    skip <- paste(rep.int(y$sent[ind], lengths(skip)),
                  unlist(skip),
                  sep = ".")
    y$FORM[is.na(match(paste(y$sent, y$ID, sep = "."), skip))]
}

words.CoNLLUTextDocument <-
function(x, ...)    
{
    ind <- !grepl("[.-]", x$content$ID)
    x$content$FORM[ind]
}

sents.CoNLLUTextDocument <-
function(x, ...)    
{
    ind <- !grepl("[.-]", x$content$ID)
    split(x$content$FORM[ind],
          x$content$sent[ind])
}

tagged_words.CoNLLUTextDocument <-
function(x, which = c("UPOSTAG", "XPOSTAG"), ...)
{
    which <- match.arg(which)
    ind <- !grepl("[.-]", x$content$ID)
    Tagged_Token(x$content$FORM[ind],
                 x$content[[which]][ind])
}

tagged_sents.CoNLLUTextDocument <-
function(x, which = c("UPOSTAG", "XPOSTAG"), ...)
{
    which <- match.arg(which)
    ind <- !grepl("[.-]", x$content$ID)
    split(Tagged_Token(x$content$FORM[ind],
                       x$content[[which]][ind]),
          x$content$sent[ind])
}
