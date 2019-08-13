# Very general classes for documents and pages that are not implementation/representation-specific
# as in Rtesseract and ReadPDF, and generics which we define here so that these other packages can
# provide specific methods for their content and computations.
# When we can implement a method in this package in terms of other generic functions, we provide that implementation.
# e.g. margins() for a
#   + Document by getting the margins for each page,
#   + DocumentPage by converting the page to a BoundingBox and
#   + TextBoundingBox and getting the left() and right values.


if(FALSE) {
setClass("Document", contains = "VIRTUAL")
setClass("DocumentPage", contains = "VIRTUAL")
} else {
 setOldClass("Document")
 setOldClass("DocumentPage")
}
    
setClass("BoundingBox", contains = "data.frame")
setClass("TextBoundingBox", contains = "BoundingBox")
setClass("ShapeBoundingBox", contains = "BoundingBox")


setClass("ProcessedDocument", contains = c("list", "Document"))
setValidity("ProcessedDocument", function(object) all(sapply(object, is, "TextBoundingBox")))


setGeneric("getTextBBox",
           function(obj, ...)
              standardGeneric("getTextBBox"))

setGeneric("getNumPages", 
           function(doc, ...)
             standardGeneric("getNumPages"))

setGeneric("getPages", 
           function(doc, ...)
             standardGeneric("getPages"))

setMethod("getPages", "ProcessedDocument",
          function(doc, ...)
            doc[])

# Default method, but has to inherit from the Document class so doesn't apply to everything.
# Will work for any specific Document class so does not need to be implemented, but
# can be overridden for efficiency, e.g. to use XPath to get the number without creating the
# page objects.
setMethod("getNumPages", "Document",
          function(doc, ...)
            length(getPages(doc)))


if(TRUE) {
setGeneric("margins",
           function(obj, asDataFrame = TRUE, ...) {  # get signature correct
              standardGeneric("margins")
          })


setMethod("margins", "Document",
function(obj, asDataFrame = TRUE, ...)        
{
    ans = lapply(getPages(obj), margins)
    if(asDataFrame)
        as.data.frame(do.call(rbind, ans))
    else
        ans
})

setMethod("margins", "DocumentPage",
margins.DocumentPage <- function(obj, asDataFrame = TRUE, ...)        {
       margins(as(obj, "TextBoundingBox"), asDataFrame, ...)
   })




#setMethod("margins", "TextBoundingBox",
#function(obj, asDataFrame = TRUE, ...) {
#         c(left = min(bbox$x), right = max(bbox$x + bbox$width))
#     })

setMethod("margins", "TextBoundingBox",
#XXX Deal with ignoring headers and footers.
function(obj, asDataFrame = TRUE, ...) {
    #  c(left = min(obj$left), right = max(obj$left + obj$width))
       c(left = min(left(obj)), right = max(right(obj)))
     })
} else {

    # S3 generic and methods.
margins =    
function(obj, asDataFrame = TRUE)
    UseMethod("margins")


margins.Document =
function(obj, asDataFrame = TRUE, ...)        
{
    ans = lapply(getPages(obj), margins)
    if(asDataFrame)
        as.data.frame(do.call(rbind, ans))
    else
        ans
}

margins.DocumentPage =
function(obj, asDataFrame = TRUE, ...)        {
       margins(as(obj, "TextBoundingBox"), asDataFrame, ...)
}



#setMethod("margins", "TextBoundingBox",
#function(obj, asDataFrame = TRUE, ...) {
#         c(left = min(bbox$x), right = max(bbox$x + bbox$width))
#     })

margins.TextBoundingBox =
    function(obj, asDataFrame = TRUE, ...) {
        #XXXX
         c(left = left(bbox), right = right(bbox))
     }
}


setGeneric("left", function(x, ...) standardGeneric("left"))
setGeneric("right", function(x, ...) standardGeneric("right"))
setGeneric("top", function(x, ...) standardGeneric("top"))
setGeneric("bottom", function(x, ...) standardGeneric("bottom"))
setGeneric("width", function(x, ...) standardGeneric("width"))
setGeneric("height", function(x, ...) standardGeneric("height"))

# General as it works for OCR and PDF
setMethod("left", "TextBoundingBox", function(x, ...) x$left)
setMethod("top", "TextBoundingBox", function(x, ...) x$top)


setMethod("left", "DocumentPage", function(x, ...) left(as(x, "TextBoundingBox")))
setMethod("right", "DocumentPage", function(x, ...) right(as(x, "TextBoundingBox")))
setMethod("top", "DocumentPage", function(x, ...) top(as(x, "TextBoundingBox")))
setMethod("bottom", "DocumentPage", function(x, ...) bottom(as(x, "TextBoundingBox")))


if(FALSE) {
###############
# Put here when talking with Jane and Matt just to illustrate how we would write package
# specific methods for the generics we had here. This is in the relevant package.
# Package specific code.
# In ReadPDF,
#    setMethod("right", "PDFTextBoundingBox", function(x, ...) x$y1)
# In Rtesseract
#    setMethod("right", "OCRTextBoundingBox", function(x, ...) x$x + x$width)
}


dim.Document =
function(x)
  t(sapply(getPages(x), dim))


setGeneric("isBold", function(x, ...) standardGeneric("isBold"))
setMethod("isBold", "ANY", function(x, ...) rep(NA, length(x)))
setMethod("isBold", "data.frame", function(x, ...) rep(NA, nrow(x)))

setGeneric("isItalic", function(x, ...) standardGeneric("isBold"))
setMethod("isItalic", "ANY", function(x, ...) rep(NA, length(x)))
setMethod("isItalic", "data.frame", function(x, ...) rep(NA, nrow(x)))


setGeneric("fontName", function(doc, ...) standardGeneric("fontName"))
setMethod("fontName", "TextBoundingBox", function(doc, ...) doc$fontName)

setGeneric("fontSize", function(doc, ...) standardGeneric("fontSize"))
setMethod("fontSize", "TextBoundingBox", function(doc, ...) doc$fontSize)


# Validity
# 

