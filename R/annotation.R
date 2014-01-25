## Annotations.

## Conceptually, a single annotation is a quintuple with "slots" id,
## type, start, end and features, and our Annotation objects are
## sequences (to allow positional access) of annotations, i.e., sequence
## of such quintuples.
## The implementation actually uses a "quintuple" (named list of length
## five) with slots giving the respective sequences of slot values.
## The feature slot of a single annotation is a feature map which we
## represent as named lists (at least for now also allowing NULL for an
## empty feature map), hence the sequence of feature values is a list of
## named lists.
## Subscripting via [ extracts subsets of annotations.
## Subscripting via $ extracts one slot value sequence.
## As Annotation objects have all slots of Span objects, we take them to
## have class "Annotation" and also inherit from class "Span".
## We allow for ids to be missing, and Annotation(id = NULL) creates
## missing ids as needed.

Annotation_classes <- c("Annotation", "Span")
Annotation_slot_names <- c("id", "type", "start", "end", "features")

Annotation <-
function(id = NULL, type = NULL, start, end, features = NULL)
{
    if(nargs() == 0L) {
        ## Could also provide default values (e.g., NULL) for all
        ## arguments ...
        a <- list(id = integer(),
                  type = character(),
                  start = integer(),
                  end = integer(),
                  feature = list())
        return(.Annotation_from_list(a))
    } 

    start < as.integer(start)
    end <- as.integer(end)
    n <- length(start)
    id <- if(is.null(id))
        rep.int(NA_integer_, n)
    else
        as.integer(id)
    type <- if(is.null(type))
        rep.int(NA_character_, n)
    else
        as.character(type)
    features <- if(is.null(features))
        rep.int(list(list()), n)
    else
        lapply(features, as.list)
    ## <TODO>
    ## Should perhaps check that all elements of 'features' are named or
    ## empty lists.
    ## </TODO>

    a <- list(id = id,
              type = type,
              start = start,
              end = end,
              features = features)
    if(any(diff(sapply(a, length)) != 0))
        stop("arguments must have the same length")

    .Annotation_from_list(a)
}

## Let's see how much these really get used.
.Annotation_from_args <-
function(id, type, start, end, features)
{
    x <- list(id = id, type = type, start = start, end = end,
              features = features) 
    .Annotation_from_list(x)
}
.Annotation_from_list <-
function(x)
{
    class(x) <- Annotation_classes
    x
}

as.Annotation <-
function(x, ...)
    UseMethod("as.Annotation")

as.Annotation.Annotation <-
function(x, ...)
    x

as.Annotation.Span <-
function(x, id = NULL, type = NULL, ...)
{
    ## Call Annotation() so we get coercion and length checking.
    Annotation(id, type, x$start, x$end, NULL)
}

is.Annotation <-
function(x)
    inherits(x, "Annotation")

`[.Annotation` <-
function(x, i)
    .Annotation_from_list(lapply(unclass(x), `[`, i))

## <TODO>
## Implement eventually ...
`[<-.Annotation` <-
function(x, i, value)
    .NotYetImplemented()
## </TODO>

`[[.Annotation` <-
function(x, i)
{
    y <- lapply(unclass(x), `[[`, i)
    y$features <- list(y$features)
    .Annotation_from_list(y)
}

## <TODO>
## Implement eventually ...
`[[<-.Annotation` <-
function(x, i, value)
    .NotYetImplemented()
## </TODO>

## $.Annotation is not really necessary.

`$<-.Annotation` <-
function(x, name, value)
{
    n <- length(x)
    x <- unclass(x)
    if(is.na(pos <- pmatch(name, Annotation_slot_names)))
        stop("invalid element name")
    name <- Annotation_slot_names[pos]
    value <- if(name == "type")
        as.character(value)
    else if(name == "features")
        as.list(value)
    else
        as.integer(value)
    ## This is not very elegant: we could record the slot modes as
    ##   Annotation_slot_modes <-
    ##     c("integer", "character", "integer", "integer", "list")
    ## but then coercion via the respective as.MODE functions would need
    ## some trickery ... maybe do this at a later stage, and modify the
    ## Annotation() creator accordingly.
    if(length(value) != n)
        stop("replacement must have the same length as object")
    x[[pos]] <- value
    .Annotation_from_list(x)
}

as.data.frame.Annotation <-
function(x, row.names = NULL, optional = FALSE, ...)
{
    y <- data.frame(id = x$id,
                    type = x$type,
                    start = x$start,
                    end = x$end,
                    stringsAsFactors = FALSE,
                    row.names = row.names)
    y$features <- x$features
    y
}

as.list.Annotation <-
function(x,  ...) 
    lapply(seq_along(x), function(i) x[i])

c.Annotation <-
function(..., recursive = FALSE)
{
    args <- lapply(list(...), function(e) unclass(as.Annotation(e)))
    y <- lapply(Annotation_slot_names,
                function(e) unlist(lapply(args, `[[`, e),
                                   recursive = FALSE))
    names(y) <- Annotation_slot_names
    .Annotation_from_list(y)
}

## This is at the mercy of duplicated() working well on lists ...
duplicated.Annotation <-
function(x, incomparables = FALSE, ...)
{
    Reduce(`&`, lapply(unclass(x), duplicated))
}

format.Annotation <-
function(x, ...)
{
    x <- as.data.frame(x)
    x$features <- lapply(x$features, .format_feature_map)
    NextMethod("format", x, ...)
}

length.Annotation <-
function(x)
    length(x$start)

merge.Annotation <-
function(x, y, ...)
{
    pos <- match(paste(y$id, y$type, y$start, y$end, sep = "\r"),
                 paste(x$id, x$type, x$start, x$end, sep = "\r"),
                 nomatch = 0L)
    ## <FIXME>
    ## This should really combine the unique tag/value pairs.
    ## In fact, duplicated tags are a problem, but how should they be
    ## handled (take the pair from x or from y)?
    x$features[pos] <- Map(c, x$features[pos], y$features[pos > 0L])
    ## </FIXME>
    c(x, y[pos == 0L])
}

names.Annotation <-
function(x)
    NULL

print.Annotation <-
function(x, ...)
{
    print.data.frame(format(x), ..., right = FALSE, row.names = FALSE)    
    invisible(x)
}

subset.Annotation <-
function(x, subset, ...)
{
    e <- substitute(subset)
    i <- eval(e, unclass(x), parent.frame())
    if(!is.logical(i)) 
        stop("'subset' must be logical")
    i <- i & !is.na(i)
    x[i]
}

unique.Annotation <-
function(x, incomparables = FALSE, ...)
    x[!duplicated(x)]
    

## Try formatting feature maps nicely.
## Similar to what we do in package 'sets', I guess ...
.format_feature_map <-
function(x, ...)
{
    if(!length(x)) return("")
    .fmt <- function(v) {
        ## Formatter for a single value.
        if(is.object(v))
            sprintf("<<%s>>", class(v)[1L])
        else if(is.array(v))
            sprintf("<<array,%s>>", paste(dim(v), collapse = ","))
        else if(is.atomic(v) && (length(v) == 1L)) {
            ## <FIXME>
            ## Should this take ... args?
            ## Also, might want to ensure this does not get too long.
            format(v)
            ## </FIXME>
        } else if(is.vector(v))
            sprintf("<<%s,%s>>", typeof(v), length(v))
        else if(is.null(v))
            "NULL"
        else
            "<<???>>"
    }
    paste(names(x), sapply(x, .fmt), sep = "=", collapse = " ")
}
            
annotations_in_spans <-
function(x, y)
{
    y <- as.Span(y)
    
    ## An annotation node is contained in a span if it does not start
    ## ahead of the span and does not end later than the span.

    ind <- outer(x$start, y$start, ">=") & outer(x$end, y$end, "<=")

    lapply(seq_len(ncol(ind)), function(j) x[ind[, j]])
}
