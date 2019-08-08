
# It is not clear we need a generic and methods for this
# as we could just define a function that immediately coerces its
# first argument to a TextBoundingBox.
# The generics don't hur, just make things a little more indirect and complicated
# but provide more flexibility.


# Used to have
#    txtNodes = getNodeSet(p, getXPathDocFontQuery(p, docFont)),

setGeneric("getTextByCols",

       function(p, threshold = .1, asNodes = FALSE,
                breaks = getColPositions(if(perPage) p else as(p, "XMLInternalDocument"), threshold = threshold, bbox = bbox, perPage = perPage, docFont = docFont, ...),
                perPage = FALSE, docFont = FALSE, order = FALSE, ...)         

           standardGeneric("getTextByCols"))


setMethod("getTextByCols", "DocumentPage",

       function(p, threshold = .1, asNodes = FALSE,
         breaks = getColPositions(if(perPage) p else as(p, "XMLInternalDocument"), threshold = threshold, bbox = bbox, perPage = perPage, docFont = docFont, ...),
         perPage = FALSE, docFont = FALSE,  order = FALSE, ...)         

           getTextByCols(as(p, "TextBoundingBox")))


setMethod("getTextByCols", "Document",

       function(p, threshold = .1, asNodes = FALSE,
         breaks = getColPositions(if(perPage) p else as(p, "XMLInternalDocument"), threshold = threshold, bbox = bbox, perPage = perPage, docFont = docFont, ...),
         perPage = FALSE, docFont = FALSE,  order = FALSE, ...)         

           getTextByCols(as(p, "TextBoundingBox")))



setMethod("getTextByCols", "TextBoundingBox",
         function(p, threshold = .1, asNodes = FALSE,
                  breaks = getColPositions(p, threshold = threshold, bbox = bbox, perPage = perPage, docFont = docFont, ...),
                  perPage = FALSE, docFont = FALSE,
                  order = FALSE, ...)    {



    bb = p
    if(nrow(bb) == 0)
        return(if(asNodes) bb else character())
    
    
    if(asNodes) {
        ans = split(txtNodes, cut(lenft(bb), c(0, breaks[-1], Inf)))
        if(order)
            ans = lapply(ans, function(x) unlist(orderByLine(x)))
        ans
    } else {
        if(order)
            warning('ignoring order in getTextByCols for asNodes = FALSE for now')
        cols = split(bb, cut(left(bb), c(0, breaks[-1], Inf)))
        cols = sapply(cols, function(x) paste(x$text[ order(x$top) ], collapse = "\n"))
    }
             
  })
