getHeader =
    function(obj, lineThreshold = 4, interlineThreshold = 2, ...)
{
    UseMethod("getHeader")
}


getHeader.Document =


    function(obj, lineThreshold = 4, interlineThreshold = 2, ...)
{
    lapply(obj, getHeader, lineThreshold, interlineThreshold, ...)
}

getHeader.DocumentPage =
    function(obj, lineThreshold = 4, interlineThreshold = 2, ...)
{
    bb = as(obj, "TextBoundingBox")
    getHeaderPos(bb, lineThreshold, interlineThreshold, ...)
}

getHeaderPos =
        
    function(bb, lineThreshold = 4, interlineThreshold = 2, ...)
{
    ## Calculate the top coords 1x
    tops = top(bb)
    mn = min(tops, na.rm = TRUE)
    w = tops - mn <= lineThreshold

    ## Find how far the nodes are from the other nodes not within the threshold
    # If this is sufficiently large (relative to the size of the text), then this is
    # a header.
    delta = min(tops - mn)

    if(delta < interlineThreshold)
        return(character())
    
    bb[w]
}



getPageFooter =
function(page, bbox = getTextBBox(page), ignorePageNumber = TRUE)
{
    tops = top(bbox)
    mx = max(tops, na.rm = TRUE)
    w = tops == mx # Might need some wiggle room with OCR
    ans = bbox$text[w][order(left(bbox)[w])]

    ## We have some docs with E57 as a page number (de la Torre-2009)
    if(length(ans) == 1 && (grepl("^[0-9]+$", ans) || grepl("Downloaded from", ans) || grepl("For +personal +use", ans) || (length(strsplit(ans, " +")[[1]]) == 1)))
        getPageFooter(, bbox[!w,])
    else
        paste(ans, collapse = " ")
}


## Moved from footer.R
getFooterPos =
    #
    #  page is the DocumentPage object.
    #  bbox is the bounding box of the individual elements in the page
    #
    #
function(page, docFont = getDocFont(page), 
          bbox = getTextBBox(page), shapes = getShapesBBox(page))
{
    if(nrow(shapes)) {
        
        bottom = max(bottom(shapes))
        # look for a line with all the text below it being smaller than the the document font.
        nodes = bbox[top(bbox) > bottom, ]  #??? Is this > or <  - Have we got the right bottom/top
        if(nrow(nodes)) {
            if(all(fontSize(nodes) < fontSize(docFont)))  
                return(bottom)
        }
    }
    
    NA
}
