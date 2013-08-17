## annotate() can use a single annotator or an annotation pipeline (a
## list of annotators), and recursively calls the given annotators and
## merges annotations.

annotate <-
function(s, f, a = Annotation())
{
    s <- as.String(s)
    if(is.function(f))
        a <- merge(a, f(s, a))
    else {
        for(e in f)
            a <- merge(a, e(s, a))
    }
    
    a
}

