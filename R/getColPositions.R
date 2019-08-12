




#  txtNodes = getNodeSet(p, getXPathDocFontQuery(p, docFont, local = local)),

getColPositions =
function(obj, threshold = .1,docFont = TRUE, align = "left", local = FALSE, ...)    
    UseMethod("getColPositions")

getColPositions.Document =
    #
    # Do this for each page
    #
function(obj, threshold = .1, docFont = TRUE, align = "left", local = FALSE, ...)    
{
   ans = lapply(getPages(obj), getColPositions, threshold, docFont, align, local, ...)
  #??? how do we combine them to learn from other pages when some are uncertain.
}


getColPositions.DocumentPage =
    # For a Page, convert to bounding box and compute
    #
function(obj, threshold = .1, docFont = TRUE, align = "left", local = FALSE, ...)    
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




getColPositions2 =
    #
    # Idea here is to check the typical place for columns, i.e. middle of the page
    #  or at 1/3 and 2/3 of the width of the page
    # Then we check is there a gap between the "words" (text elements) on either side
    #
    #  If there is a header/footer that is centered on the page (e.g. a journal name or the article title, page number)
    #  then there will be text in the gap between 2 columns. However, that is at the top or bottom
    #  So we can discard that.
    #  These are headers/footers, so ideally we would remove these first. But this can be cyclical, i.e we may need to find the 
    #  columns before we find the header and footer.
    #
    #  Pages also have 2 columns but a large block of text that spans those two columns, e.g., the abstract, title.
    #  See Klempa-2003
    #
    # 
function(obj, threshold = .1, docFont = TRUE, align = "left", local = FALSE, ncols = 3, pageWidth = getPageWidth(obj), ...)
{

    pos = switch(ncols, min(left(obj)), pageWidth/2, pageWidth*c(.33, .66))

    sapply(pos, isTextColumn, obj, pageWidth)
}


findEmptyRegion =
    #
    #
    #XXX Images. Amada-2003, p4.
    #
function(pos, bbox, lineBreaks = findLineBreaks(bbox), range = c(0, Inf),
         minNumChars = 3,            
         charSize = median((right(bbox) - left(bbox))/nchar(bbox$text)),
         minNumLines = 3, numLines = -1)
{
    # add an epsilon so that they can't be within 3 or 4  pts/units.  Get the median character size
    delta = minNumChars/2*charSize
    cross = left(bbox) < pos - delta  &  right(bbox) > pos + delta
    if(length(range) == 0)
        range = c(min(top(bbox)), max(bottom(bbox)))
browser()  
    if(any(cross))  {
        # XXX If the lines that cross are not at the top or bottom, this will split the page
        # in 2. Need to correct.  See Klempa-2003, page 2
        cbb = bbox[cross,]

        br = lineBreaks

        if(nrow(cbb) == 1) {
            # so only one text element crosses the possible position
            # find out if it is above or below the rest of the text?
            # If it is in the middle, then it splits the page.
            if(bottom(cbb) >= br[1])
                vert = c(a = bottom(cbb), b = Inf)
            else if(top(cbb) >= br[-length(br)])
                vert = c(a = 0, b = top(cbb))
            else { # split the page. 
                stop("Split the page")
             }
        } else {

            # Check to see if the lines in cross break the page into blocks in between
            # Get the groups by line number.
            lineNum = seq_len(length(br))
            cline = lineNum[ cut(bottom(cbb), c(br, Inf))]
ll = getTextLines(bbox, br)
#names(ll) = sapply(ll, function(x) paste(x$tex, collapse = " ")) # XXX remove when finished debugging.
            others = setdiff(lineNum, cline)
            g = split(others, cumsum(diff(c(0, others)) > 1))

            g = g[ sapply(g, length) > minNumLines ]

            if(FALSE && length(g) == 1) {
                #???  Now same as if more than 1 so consolidate if it works.
                tmp = bbox = do.call(rbind, ll[g[[1]]])
                r = c(max(bottom(cbb)), range[2]) #XXX fix this - case were cross is below means we want c(range[1], min(top(cbb)))
                return(findEmptyRegion(pos, tmp, br, range = c(), minNumChars = minNumChars, charSize = charSize, minNumLines = minNumLines))
            } else {
                #???
                # Need to compute the start and end
               tmp = lapply(g, function(idx) {
                                  tmp = do.call(rbind, ll[idx])
                                  findEmptyRegion(pos, tmp, br, range = c(), minNumChars = minNumChars, charSize = charSize, minNumLines = minNumLines, numLines = length(idx))
                              })
               return(do.call(rbind, tmp))
            }
            
            tmp = cbb[order(bottom(cbb)), ]
            m = c(0, diff(bottom(tmp)))
            i = which.max(m)
            vert = c(a = bottom(tmp)[i-1], b = top(tmp)[i])
        }
    } else {
        # so no line has text that crosses at this pos
        # So the vertical extent for this column is all of the original bbox  or range.
        cbb = bbox
        vert = range # c(0, Inf)
    }

#if(numLines == -1)   browser()
    
    tmp = bbox[bottom(bbox) > vert[1] & top(bbox) < vert[2], ]
    to.left = tmp[right(tmp) <= pos, ]
    to.right = tmp[left(tmp) >= pos, ]                
    hor = c(left = max(right(to.left)), right = min(left(to.right)))


    # list(vertical = vert, horizontal = hor)
    data.frame(left = hor[1], top = vert[1], right = hor[2], bottom = vert[2], numLines = numLines)
}

ee = function(page, ...) {
  findEmptyRegion(getPageWidth(page)/2, getTextBBox(page), ...)
}

isTextColumn =
    #
    #  look for an empty channel/gutter at pos
    #  determine the top and bottom of it.
    #
    #
    #
function(pos, bbox, pageWidth, minWidth = 5) # epsilon should be based on interword skip
{

    to.left = bbox[right(bbox) <= pos, ]
    to.right = bbox[left(bbox) >= pos,]
    browser()
    

    cross = left(bbox) < pos & right(bbox) > pos
    if(any(cross)) {
        cbb = bbox[cross,]
        tmp = cbb[order(bottom(cbb)), ]
        m = c(0, diff(bottom(tmp)))
        i = which.max(m)
        vert = c(a = bottom(tmp)[i-1], b = top(tmp)[i])

        tmp = bbox[bottom(bbox) > vert[1] & top(bbox) < vert[2], ]
        to.left = tmp[right(tmp) <= pos, ]
        to.right = tmp[left(tmp) >= pos, ]                
        hor = c(left = max(right(to.left)), right = min(left(to.right)))
        
        
        w2 = isCentered(bbox[cross,], pageWidth)
        if(!all(w2))
            return(FALSE)
    }

    # what is it about the values that are close to our possible column
    
    w = (left(to.right) - pos) < minWidth
    if(any(w)) {
        # is this part of the header. Does it span
    }
}

