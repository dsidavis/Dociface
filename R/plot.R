plot.Document <-
    # method for plotting all the pages of a Document, or a (sub)set of pages
    #
    # Can turn off the axes and use smaller margins to maximize the area used for display
    #
function(x, y, axes = FALSE, mar = c(1, 1, 2, 1), pages = getPages(x), ...)
{
    np = length(pages)  # getNumPages(x)
    r = ceiling(sqrt(np))
    c = ceiling(np/r)
    opar = par(no.readonly = TRUE)
    on.exit(par(opar))
    par(mfrow = c(r, c))
    
    if(!axes)
        par(mar = mar)
    
    invisible(mapply(function(p, i, axes, ...)
                         plot(p, main = paste0("Page ", i), ...),
                     pages, seq(along = pages), axes,
                     MoreArgs = list(...)))
}


setMethod("plot", "Document", plot.Document)


plot.MultiPageBoundingBox =
    #
    # Displays the pages of a MultiPageBoundingBox by splitting it into separate pages
    # and rendering these.
    #  Reuses plot.Document method, but directly, for now at least..
function(x, y, axes = FALSE, mar = c(1, 1, 2, 1), ...)
{
       # Could have a new ProcessedDocument class (like ProcessedOCRDocument in Rtesseract)
       # and then a method for plotting that which we would move from Rtesseract to here.
       # But haven't set up the class relationships between ProcessedOCRDocument, OCRDocument and a new
       # general ProcessedDocument.  But this would allow us to move code from Rtesseract that would
       # work for both ReadPDF and Rtesseract.
       #    new("ProcessedDocument", split(x, x$page))


    pages = split(x, x$page)
    pages = lapply(pages, function(x) { class(x) = setdiff(class(x), "MultiPageBoundingBox"); x})
    plot.Document(axes = axes, mar = mar, pages = pages, ...)
}

# Compute colors
# pageTitle() function and default value.
setMethod("plot", "DocumentPage",
function(x, y, colors = getTextColors(x), axes = FALSE, shapes = getShapesBBox(x), bbox = as(x, "TextBoundingBox"), ...)
{
    #?? Should this be
    #   getTextBBox(x, color = TRUE)
    # or
    #   as(x, "TextBoundingBox")
    # Or should getTextBBox() have color, rotation, pages, etc. all be TRUE.
    plot(bbox, pageHeight =  getPageHeight(x), colors = colors, axes = axes, shapes = shapes, ...)
})

setMethod("plot", "TextBoundingBox",
function(x, y, pageHeight = getPageHeight(x), colors = getTextColors(x), axes = FALSE, shapes = NULL, boxes = FALSE, cex = .5, ...)
{

    plot(1, xlim = range(c(left(x), right(x))), ylim = range(0, pageHeight), ..., xlab = "", ylab = "", axes = axes, type = "n")
    if(!axes)
        box() # could do it unconditionally.

    # Font information
    # Handle Rotation

    if(length(shapes) && !(is.logical(shapes) && !shapes))  # allow shapes = NULL or FALSE to turn off.
        plot(shapes, pageHeight = pageHeight)
    
    if(boxes)
        rect( left(x), pageHeight - bottom(x), right(x), pageHeight - top(x), border = "gray")

    # Scale the text.
    # caller can put cex = I(value) to force that to be passed directly
    # or can provide a vector for cex. Otherwise, if a scalar, that is scaled
    # by the height.
    if(!is(cex, "AsIs") && length(cex) == 1) {
        if(all(is.na( h <- fontSize(x)))) # XXX do 
            h = abs(bottom(x) - top(x))
        if(any(h > pageHeight*.3))
            warning("large fonts")
        
        cex = cex *  (h-min(h))/diff(range(h))
    }
    
    text(left(x), pageHeight - bottom(x), x$text, adj = c(0, 0), cex = cex, col = colors)


    invisible()
})


setMethod("plot", "ShapeBoundingBox",
plot.ShapeBoundingBox <- function(x, y, pageHeight = getPageHeight(x), colors = getTextColors(x), axes = FALSE, shapes = NULL, boxes = FALSE, cex = .75, lty = 1, ...)
{

    bbox = x
    ll = bbox[ bbox$nodeType == 'line', ]
    w = is.na(x$lineWidth) | x$lineWidth < 1
    x$lineWidth[w] = 1

    opar = par(no.readonly = TRUE)
    on.exit(par(opar))
    par(lend = "square")
    if(FALSE && nrow(ll))
    {
        # do this for the line elements, and the rect's differently

      n = nrow(ll)
         # Put the lines into 2 columns of consecutive pairs of x's and y's and NA 
      X = matrix(NA, 3*n - 1, 2)
      i = seq(1, length = n, by = 3)
      X[i,1] = left(ll)
      X[i + 1,1] = right(ll)
      X[i,2] = pageHeight - bottom(ll)
      X[i + 1,2] = pageHeight - top(ll)
      lwd = rep(NA, 3*n - 1)
      lwd[c(i, i+1)] = as.numeric(ll[, "lineWidth"])
#browser()      
      lines(X, col = ll[, "stroke"], lwd = lwd, lty = lty, lend = "square")    
    }


    rr = bbox[ bbox$nodeType %in% c('line', 'rect', 'img'), ]
    if(nrow(rr))
    {
          # do this for the line elements, and the rect's differently

        rect(left(rr),  pageHeight - top(rr), right(rr),  pageHeight - bottom(rr),  border = ll[, "stroke"],
             lwd =  as.numeric(x[, "lineWidth"]),
             lty = lty, lend = "square")    
    }    
})

# Generic and method for computing colors of the text elements so we can render
# them appropriately when plotting.
setGeneric("getTextColors",
             function(obj, ...) 
               standardGeneric("getTextColors"))

 # Default method is to make every text element black
 # This means we don't have to do anything for OCRDocument objects and OCRResults which don't have color information.
setMethod("getTextColors", "ANY",
          function(obj, ...)
               "black")



# Get the PageHeight and PageWidth to use dim() and then we don't have to write methods for that.
# Dange that dim() would get called on a BoundingBox and get the dim() of the data.frame()
# not the conceptual page dimensions.

getPageHeight =
function(obj, ...)
    UseMethod("getPageHeight")

getPageWidth =
function(obj, ...)
    UseMethod("getPageWidth")


#XXX be careful - mixing dim() of page and dim() of data.frame.
getPageHeight.DocumentPage =
function(obj, ...)
    dim(obj)[1]

getPageWidth.DocumentPage =
function(obj, ...)
    dim(obj)[2]

getPageHeight.TextBoundingBox =
function(obj, ...)
    attr(obj, "pageDimensions")[1]

getPageWidth.TextBoundingBox =
function(obj, ...)
   attr(obj, "pageDimensions")[2]


# The sapply is a method for Document so processes each page.
# See lapply.R.
getPageWidth.Document =
function(obj, ...)
    sapply(obj, getPageWidth, ...)

getPageHeight.Document =
function(obj, ...)
    sapply(obj, getPageHeight, ...)

