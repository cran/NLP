AnnotatedPlainTextDocument <-
function(s, a, meta = list())
{
    s <- as.String(s)

    ## Be nice.
    a <- as.Annotation(a)

    doc <- list(content = s, annotation = a, meta = meta)
    class(doc) <- c("AnnotatedPlainTextDocument",
                    "PlainTextDocument",
                    "TextDocument")

    doc
}

format.AnnotatedPlainTextDocument <-
function(x, ...)
{        
    c(.format_TextDocument(x),
      sprintf("Annotations:  length: %s",
              length(x$annotation)),
      sprintf("Content:  chars: %d",
              nchar(x$content)))
}
    
content.AnnotatedPlainTextDocument <-
function(x)
    x$content

`content<-.AnnotatedPlainTextDocument` <-
function(x, value)
    stop("content modification is not possible for AnnotatedPlainTextDocument objects")

## meta.AnnotatedPlainTextDocument <-
## function(x, tag = NULL, ...)
##     if(is.null(tag)) x$meta else x$meta[[tag]]

## `meta<-.AnnotatedPlainTextDocument` <-
## function(x, tag = NULL, ..., value)
## {
##     if(is.null(tag))
##         x$meta <- value
##     else
##         x$meta[[tag]] <- value
##     x
## }

as.character.AnnotatedPlainTextDocument <-
function(x, ...)
    x$content

annotation <-
function(x)
{
    if(!inherits(x, "AnnotatedPlainTextDocument"))
        stop("argument 'x' must be an AnnotatedPlainTextDocument object")
    x$annotation
}

## NLTK style functions for high level access 

words.AnnotatedPlainTextDocument <-
function(x, ...)
{
    if(!inherits(x, "AnnotatedPlainTextDocument"))
        stop("argument 'x' must be an AnnotatedPlainTextDocument object")
    s <- x$content
    a <- x$annotation
    a <- a[a$type == "word"]
    .words_from_annotation_and_text(a, s)
}

.words_from_annotation_and_text <-
function(a, s)
{
    a <- a[a$type == "word"]
    w <- s[a]
    ## Use a word feature where available.
    f <- lapply(a$features, `[[`, "word")
    i <- (lengths(f) > 0L)
    w[i] <- unlist(f[i])
    w
}

sents.AnnotatedPlainTextDocument <-
function(x, ...)
{
    if(!inherits(x, "AnnotatedPlainTextDocument"))
        stop("argument 'x' must be an AnnotatedPlainTextDocument object")
    s <- x$content
    a <- x$annotation
    .sents_from_annotation_and_text(a, s)
}

.sents_from_annotation_and_text <-
function(a, s)
{
    a <- annotations_in_spans(a[a$type == "word"],
                              a[a$type == "sentence"])
    lapply(a, .words_from_annotation_and_text, s)
}

paras.AnnotatedPlainTextDocument <-
function(x, ...)
{
    if(!inherits(x, "AnnotatedPlainTextDocument"))
        stop("argument 'x' must be an AnnotatedPlainTextDocument object")
    s <- x$content
    a <- x$annotation
    lapply(annotations_in_spans(a, a[a$type == "paragraph"]),
           .sents_from_annotation_and_text, s)
}

tagged_words.AnnotatedPlainTextDocument <-
function(x, map = NULL, ...)
{
    if(!inherits(x, "AnnotatedPlainTextDocument"))
        stop("argument 'x' must be an AnnotatedPlainTextDocument object")
    s <- x$content
    a <- x$annotation
    a <- a[a$type == "word"]
    if(!is.null(map))
        a <- .map_POS_tags_Annotation(a, map)
    .tagged_words_from_annotation_and_text(a, s)
}

.tagged_words_from_annotation_and_text <-
function(a, s)
{
    pos <- .annotation_features_with_template(a, "POS")
    Tagged_Token(.words_from_annotation_and_text(a, s), pos)
}

tagged_sents.AnnotatedPlainTextDocument <-
function(x, map = NULL, ...)
{
    if(!inherits(x, "AnnotatedPlainTextDocument"))
        stop("argument 'x' must be an AnnotatedPlainTextDocument object")
    s <- x$content
    a <- x$annotation
    if(!is.null(map))
        a <- .map_POS_tags_Annotation(a, map)
    .tagged_sents_from_annotation_and_text(a, s)
}

.tagged_sents_from_annotation_and_text <-
function(a, s)
{
    lapply(annotations_in_spans(a[a$type == "word"],
                                a[a$type == "sentence"]),
           .tagged_words_from_annotation_and_text, s)
}

tagged_paras.AnnotatedPlainTextDocument <-
function(x, map = NULL, ...)
{
    if(!inherits(x, "AnnotatedPlainTextDocument"))
        stop("argument 'x' must be an AnnotatedPlainTextDocument object")
    s <- x$content
    a <- x$annotation
    if(!is.null(map))
        a <- .map_POS_tags_Annotation(a, map)
    lapply(annotations_in_spans(a, a[a$type == "paragraph"]),
           .tagged_sents_from_annotation_and_text, s)
}
           
parsed_sents.AnnotatedPlainTextDocument <-
function(x, ...)
{
    if(!inherits(x, "AnnotatedPlainTextDocument"))
        stop("argument 'x' must be an AnnotatedPlainTextDocument object")
    a <- x$annotation
    .parsed_sents_from_annotation(a)
}

.parsed_sents_from_annotation <-
function(a)
{
    a <- a[a$type == "sentence"]
    ptexts <- .annotation_features_with_template(a, "parse")
    lapply(ptexts, Tree_parse)
}

parsed_paras.AnnotatedPlainTextDocument <-
function(x, ...)
{
    if(!inherits(x, "AnnotatedPlainTextDocument"))
        stop("argument 'x' must be an AnnotatedPlainTextDocument object")
    a <- x$annotation
    lapply(annotations_in_spans(a, a[a$type == "paragraph"]),
           .parsed_sents_from_annotation)
}

chunked_sents.AnnotatedPlainTextDocument <-
function(x, ...)
{
    if(!inherits(x, "AnnotatedPlainTextDocument"))
        stop("argument 'x' must be an AnnotatedPlainTextDocument object")
    s <- x$content
    a <- x$annotation

    ## Require annotations with POS and chunk_tag features, as obtained
    ## e.g. with the Apache OpenNLP POS tag and chunk annotators.  We
    ## could alternatively use annotations with parse features and
    ## flatten the parse trees.

    lapply(annotations_in_spans(a[a$type == "word"],
                                a[a$type == "sentence"]),
           function(a) {
               ptags <- .annotation_features_with_template(a, "POS")
               ctags <- .annotation_features_with_template(a, "chunk_tag")
               words <- .words_from_annotation_and_text(a, s)
               chunk_tree_from_chunk_info(words, ptags, ctags)
           })
}

otoks.AnnotatedPlainTextDocument <-
function(x, ...)
{
    if (!inherits(x, "AnnotatedPlainTextDocument")) 
        stop("argument 'x' must be an AnnotatedPlainTextDocument object")
    s <- x$content
    a <- x$annotation
    a <- a[a$type == "word"]
    a <- a[!duplicated(sprintf("%d-%d", a$start, a$end))]
    s[a]
}

.map_POS_tags_Annotation <-
function(x, map)
{
    map <- POS_tag_mapper(map, meta(x, "POS_tagset"))
    x$features <-
        lapply(x$features,
               function(e) {
                   if(!is.null(pos <- e$POS))
                       e$POS <- map(pos)
                   e
               })
    x
}

.annotation_features_with_template <-
function(x, tag, FUN.VALUE = "")
{
    tryCatch(vapply(x$features, function(e) e[[tag]], FUN.VALUE),
             error = function(e) {
                 stop(sprintf("incomplete or invalid '%s' features",
                              tag),
                      call. = FALSE)
             })
}
