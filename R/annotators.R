## All annotators should have formals s and a, giving the string to
## annotate and an annotation to start from, and return "their own"
## annotation. 

Annotator <-
function(f, description = NULL, classes = NULL)
{
    if(!identical(names(formals(function(s, a) NULL)), c("s", "a")))
        stop("Annotators must have formals 's' and 'a'.")
    class(f) <- .classes_with_default(classes, "Annotator")
    attr(f, "description") <- description
    f
}

print.Annotator <-
function(x, ...)
{
    d <- attr(x, "description")
    s <- c(sprintf("An annotator inheriting from classes\n  %s",
                   paste(class(x), collapse = " ")),
           if(is.null(d)) {
               "with no additional description."
           } else {
               c("with description",
                 strwrap(d, indent = 2L, exdent = 2L))
           })
    writeLines(s)
    invisible(x)
}

## Annotator generators.

## Provide annotator generators for composite basic NLP tasks (e.g.,
## obtaining POS tags for the tokens in all sentences) based on
## functions which perform simple tasks (e.g., obtaining POS tags for
## the token in a single sentence) and return spans/features or simple
## annotations (but do not provide ids themselves).

Simple_Sent_Token_Annotator <-
function(f, description = NULL, classes = NULL)
{
    ## f should be a simple sentence tokenizer, which takes a string s
    ## representing the whole text, and returns the spans of the
    ## sentences in s, or a simple annotation with these spans and
    ## (possibly) additional features.

    default <- "Simple_Sent_Token_Annotator"
    classes <- .classes_with_default(classes, default)

    g <- function(s, a = Annotation()) {
        s <- as.String(s)
        y <- f(s)
        n <- length(y)
        id <- .seq_id(.next_id(a$id), n)
        type <- rep.int("sentence", n)
        if(is.Annotation(y)) {
            ## Could check whether ids are really missing.
            y$id <- id
            y$type <- type              # Just making sure ...
        } else if(is.Span(y)) {
            y <- as.Annotation(y, id = id, type = type)
        } else
            stop("Invalid result from underlying sentence tokenizer.")
        y
    }

    Annotator(g, description, classes)
}

Simple_Word_Token_Annotator <-
function(f, description = NULL, classes = NULL)
{
    ## f should be a simple "word" tokenizer, which takes a string s
    ## representing a single sentence, and returns the spans of the word
    ## tokens in s, or a simple annotation with these spans and
    ## (possibly) additional features.
    
    ## The generated annotator adds the sentence offsets and unique
    ## word token ids, and constituents features for the sentences.

    default <- "Simple_Word_Token_Annotator"
    classes <- .classes_with_default(classes, default)
    
    g <- function(s, a) {
        s <- as.String(s)

        ## Use the given annotation to extract the sentences.
        i <- which(a$type == "sentence")
        if(!length(i))
            stop("no sentence token annotations found")            

        ## Obtain the results of the word tokenizer for these sentences.
        y <- lapply(substring(s, a$start[i], a$end[i]), f)
        ## Compute ids for the word tokens, and turn results into
        ## annotations.
        ## If m is the maximal id used in a and sentence i has n_i
        ## tokens, then the ids for these start from
        ##   m + 1 + sum(n_j: j < i)
        ## and have length n_i, of course.
        if(all(sapply(y, is.Annotation))) {
            y <- Map(function(u, v) {
                         u$start <- u$start + v
                         u$end <- u$end + v
                         u
                     },
                     y,
                     a$start[i] - 1L)
            n <- sapply(y, length)
            id <- Map(.seq_id,
                      .next_id(a$id) + c(0L, cumsum(head(n, -1L))),
                      n)
            type <- Map(rep.int, "word", n)
            y <- Map(function(u, id, type) {
                         u$id <- id
                         u$type <- type # Just making sure ...
                         u
                     },
                     y, id, type)
        } else if(all(sapply(y, is.Span))) {
            y <- Map(`+`, y, a$start[i] - 1L) # Add sentence offsets.
            n <- sapply(y, length)
            id <- Map(.seq_id,
                      .next_id(a$id) + c(0L, cumsum(head(n, -1L))),
                      n)
            type <- Map(rep.int, "word", n)
            y <- Map(function(u, id, type)
                     as.Annotation(u, id = id, type = type),
                     y, id, type)
        } else
            stop("Invalid result from underlying word tokenizer.")

        ## Constituent features for the sentences.
        a <- a[i]
        a$features <- .simple_feature_map(id, "constituents")

        ## Combine sentence annotation with constituent features and the
        ## word token annotations.
        c(a, do.call(c, y))
    }

    Annotator(g, description, classes)
}

Simple_POS_Tag_Annotator <-
function(f, description = NULL, classes = NULL)
{
    ## f should be a simple POS tagger, which takes a character vector
    ## giving the word tokens in a sentence, and returns either a
    ## character vector with the tags, or a list of feature maps with
    ## the tags as 'POS' feature and possibly other features.

    ## The generated annotator simply computes an annotation for the
    ## word tokens with the features obtained from the POS tagger.

    default <- "Simple_POS_Tag_Annotator"
    classes <- .classes_with_default(classes, default)

    g <- function(s, a) {
        s <- as.String(s)

        a <- annotations_in_spans(a[a$type == "word"],
                                  a[a$type == "sentence"])
        if(!length(a))
            stop("no sentence token annotations found")
        if(!any(sapply(a, length) > 0L))
            stop("no word token annotations found")

        y <- lapply(s[a], f)
        if(all(sapply(y, is.character)))
            features <- .simple_feature_map(unlist(y), "POS")
        else if(all(sapply(y, is.list)))
            features <- unlist(y, recursive = FALSE)
        else 
            stop("Invalid result from underlying POS tagger.")

        a <- do.call(c, a)
        a$features <- features
        a
    }
    
    Annotator(g, description, classes)
}

Simple_Entity_Annotator <-
function(f, description = NULL, classes = NULL)
{
    ## f should be a simple entity detector ("named entity recognizer") 
    ## which takes a character vector giving the word tokens in a
    ## sentence, and return a simple annotation containing the word
    ## token spans and types of the entities found.

    ## The generated annotator adds ids and transforms word token spans
    ## to character spans.

    default <- "Simple_Entity_Annotator"
    classes <- .classes_with_default(classes, default)

    g <- function(s, a) {
        s <- as.String(s)

        a <- annotations_in_spans(a[a$type == "word"],
                                  a[a$type == "sentence"])
        if(!length(a))
            stop("no sentence token annotations found")
        if(!any(sapply(a, length) > 0L))
            stop("no word token annotations found")

        y <- lapply(a,
                    function(e) {
                        result <- f(s[e])
                        if(!inherits(result, "Annotation"))
                            stop("Invalid result from underlying name finder.")
                        result$start <- e$start[result$start]
                        result$end <- e$end[result$end]
                        result
                    })
                        
        y <- do.call(c, y)
        y$id <- .seq_id(.next_id(a$id), length(y))

        y
    }        
    
    Annotator(g, description, classes)
}

Simple_Chunk_Annotator <-
function(f, description = NULL, classes = NULL)
{
    ## f should be a simple chunker, which takes character vectors
    ## giving the word tokens and the corresponding POS tags as inputs,
    ## and returns either a character vector with the chunk tags, or a
    ## list of feature maps with the tags as 'chunk_tag' feature and
    ## possibly other features.

    ## The generated annotator simply extracts the word token
    ## annotations for the sentences, obtains the chunk features for
    ## these, and returns the word token annotations with these features
    ## (only).

    default <- "Simple_Chunk_Annotator"
    classes <- .classes_with_default(classes, default)

    g <- function(s, a) {
        s <- as.String(s)

        a <- annotations_in_spans(a[a$type == "word"],
                                  a[a$type == "sentence"])
        if(!length(a))
            stop("no sentence token annotations found")
        if(!any(sapply(a, length) > 0L))
            stop("no word token annotations found")

        y <- lapply(a,
                    function(e)
                    f(s[e], sapply(e$features, `[[`, "POS")))
        if(all(sapply(y, is.character)))
            features <- .simple_feature_map(unlist(y), "chunk_tag")
        else if(all(sapply(y, is.list)))
            features <- unlist(y, recursive = FALSE)
        else 
            stop("Invalid result from underlying chunker.")

        a <- do.call(c, a)
        a$features <- features
        a
    }

    Annotator(g, description, classes)
}

sentence_constituents <-
function(a)
{
    i <- which(a$type == "sentence")
    constituents <- lapply(a$features[i], `[[`, "constituents")
    if(!all(sapply(constituents, length) > 0L)) {
        ## Looks like we have an annotation with no constituents
        ## features for the sentences ... need to compute these.
        ## Make sure sentences are ordered by character offsets.
        i <- i[order(a$end[i])]
        j <- which(a$type == "word")
        ## Should we also make sure tokens are ordered by character
        ## offsets?
        k <- rowSums(outer(a$start[j], a$start[i], ">="))
        constituents <- split(a$id[j], k)
        names(constituents) <- a$id[i][as.integer(names(constituents))]
        ## Assuming there can not be empty sentences, we could more
        ## simply do
        ##   names(constituents) <- a$id[i]
    }
    else
        names(constituents) <- a$id[i]
    constituents
}

.max_id <-
function(id)
    if(!length(id)) 0L else max(id)

.next_id <-
function(id)
    .max_id(id) + 1L

.seq_id <-
function(f, l)
    as.integer(seq(from = f, length.out = l))

.simple_feature_map <-
function(x, tag)
{
    ## Turn a sequence of values x into a list of feature maps with
    ## given tag and respective values in x.
    lapply(x,
           function(u) {
               v <- list(u)
               names(v) <- tag
               v
           })
}

.classes_with_default <-
function(classes, default)
    c(classes[classes != default], default)
