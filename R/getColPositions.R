
#  txtNodes = getNodeSet(p, getXPathDocFontQuery(p, docFont, local = local)),

getColPositions =
function(obj, threshold = .1,
         docFont = TRUE, align = "left", local = FALSE, ...)    
    UseMethod("getColPositions")

getColPositions.Document =
    #
    # Do this for each page
    #
function(obj, threshold = .1,
         docFont = TRUE, align = "left", local = FALSE, ...)    
{
   ans = lapply(getPages(obj), getColPositions, threshold, docFont, align, local, ...)
  #??? how do we combine them to learn from other pages when some are uncertain.
}


getColPositions.DocumentPage =
    # For a Page, convert to bounding box and compute
    #
function(obj, threshold = .1,
         docFont = TRUE, align = "left", local = FALSE, ...)    
{
   bb = as(obj, "TextBoundingBox")
   getColPositions(bb, threshold, docFont, align, local, ...)
}


findColPositions =
    #
    # An alternative approach that looks at the density of the positions of the text
    # and looks for very low values.
    # Will be fooled by a few lines having text that spans the gap.
    #
function(vals, bw = 1, d = density(vals, bw), ezero = 0)
{
   r = rle(d$y <= ezero)
   tmp = split(d$x, rep(1:length(r$values), r$length))[r$values & r$lengths > 2]
   c(0, sapply(tmp, median))
}

findColPositionsByLines =
    #
    # looks for columns by finding inter-consecutive word distances
    # and finding larger ones that indicate a potential column
    # and then finding the location of those gaps and determining
    # if they occur at the same location
    #
function(bbox, lines = getTextLines(bbox))
{
    gaps = lapply(ll, function(x) right(x) - c(0, left(x)[-1]))
}


findColPositions2 =
    #
    # Get the tallest p
    #
function(vals, ncols = 2, bw = 1, d = density(vals, bw)) 
{
    ymax = sort(d$y, decreasing = TRUE)[1:ncols]
    x = d$x[ d$y >= min(ymax)]
    # Now back these values to the left until we find a very low density value
    
    x
}
# getColPositions


getColPositions.TextBoundingBox =
function(obj, threshold = .1, docFont = TRUE, align = "left", local = FALSE, ...)        
{
    
    bbox = obj
    if(nrow(bbox) == 0 && !local) 
        # Use the page-specific font count
        return(getColPositions(p, threshold, docFont = docFont, align = align, local = TRUE, ...))
    
#    p = as(p, "PDFToXMLPage")

    vals = switch(align,
                  left = left(bbox),
                  right = right(bbox),
                  center = left(bbox) + width(bbox)/2
        )


    return(findColPositions(vals, ...))
    
    tt = table(vals)

    # Subtract 2 so that we start slightly to the left of the second column to include those starting points
    # or change cut to be include.lowest = TRUE
    ans = as.numeric(names(tt [ tt > nrow(bbox)*threshold])) - 2

    minDiff = 5
    if(length(ans) > 2 && any(delta <- (diff(ans) <= 20))) {
        # See Forrester-2008
        tt = split(bb$text, cut(left(bbox), c(ans, Inf)))
        w = sapply(tt, function(x) any(grepl("References", x)))
        if(any(w)) {
              # Need to check it is the References column
           minDiff = 20
        }

    }
    
    w = which(diff(ans) < minDiff)
    if(length(w))
        ans = ans[ - (w + 1)]


    if(length(ans) == 1 && ans[1] > .4*dim(p)["width"]) {
        ## So only one column and it starts close to the middle of the page. Probably means
        ## there is one to the left that we didn't find. This may be because the text is in a different font.
        ##
        ans = c(margins(p)[1], ans)
     }
    
    ans    
}
