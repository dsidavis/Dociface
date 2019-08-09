setMethod("plot", "Document",
function(x, y, axes = FALSE, mar = c(1, 1, 2, 1), ...)
{
    np = getNumPages(x)
    r = ceiling(sqrt(np))
    c = ceiling(np/r)
    opar = par(no.readonly = TRUE)
    on.exit(par(opar))
    par(mfrow = c(r, c))
    if(!axes)
        par(mar = mar)
    invisible(mapply(function(p, i, axes, ...) plot(p, main = paste0("Page ", i), ...), getPages(x), seq(1, np), axes, MoreArgs = list(...)))
})

#XXX Compute colors
# pageTitle() function and default value.
setMethod("plot", "DocumentPage",
function(x, y, colors = getTextColors(x), axes = FALSE, ...)
{
    #?? Should this be
    #   getTextBBox(x, color = TRUE)
    # or
    #  as(x, "TextBoundingBox")
    # Or should getTextBBox() have color, rotation, pages, etc. all be TRUE.
    bb = as(x, "TextBoundingBox") # getTextBBox(x, color = TRUE)
    plot(bb, pageHeight =  getPageHeight(x), colors = colors, axes = axes, ...)
})

setMethod("plot", "TextBoundingBox",
function(x, y, pageHeight = max(bottom(x)), colors = getTextColors(x), axes = FALSE, ...)
{    
    plot(1, xlim = range(c(left(x), right(x))), ylim = range(0, pageHeight), ..., xlab = "", ylab = "", axes = axes)
    if(!axes)
        box() # could do it uncoditionally.
    
    text(left(x), pageHeight - bottom(x), x$text, adj = 0, cex = .5, col = colors)
})


if(FALSE) {
library(Dociface); library(ReadPDF)
doc = readPDFXML(list.files("SamplePDFs", pattern = "Amada-2013.xml", full = TRUE))
plot(doc[[1]])
plot(doc[[1]], colors = "red")
plot(doc[[1]], colors = c("red", "green"))
}

setGeneric("getTextColors",
             function(obj, ...) 
               standardGeneric("getTextColors"))


setMethod("getTextColors", "ANY",
          function(obj, ...)
               "black")



getPageHeight =
function(obj, ...)
    UseMethod("getPageHeight")

getPageHeight.DocumentPage =
function(obj, ...)
    dim(obj)[1]

getPageWidth =
function(obj, ...)
    UseMethod("getPageWidth")

getPageWidth.DocumentPage =
function(obj, ...)
   dim(obj)[2]
