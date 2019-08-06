
if(FALSE) {
setClass("Document", contains = "VIRTUAL")
setClass("DocumentPage", contains = "VIRTUAL")
} else {
 setOldClass("Document")
 setOldClass("DocumentPage")
}
    
setClass("TextBoundingBox", contains = "data.frame")


setGeneric("getTextBBox",
           function(obj, ...)
              standardGeneric("getTextBBox"))

setGeneric("getNumPages", 
           function(doc, ...)
             standardGeneric("getNumPages"))

setGeneric("getPages", 
           function(doc, ...)
             standardGeneric("getPages"))


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
function(obj, asDataFrame = TRUE, ...) {
         c(left = min(obj$left), right = max(obj$left + obj$width))
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
setGeneric("right", function(x, ...) standardGeneric("left"))
setMethod("left", "TextBoundingBox", function(x, ...) x$x)


if(FALSE) {
###############
# Package specific code.
# In ReadPDF,
setMethod("right", "PDFTextBoundingBox", function(x, ...) x$y1)
# In Rtesseract
setMethod("right", "OCRTextBoundingBox", function(x, ...) x$x + x$width)


setAs("OCRPage", "TextBoundingBox",
         function(from) {
              GetBoxes(from)
          })



setAs("PDFToXMLPage", "TextBoundingBox",
         function(from) {
             getTextBBox(from, asDataFrame = TRUE)
             # getBBox2(from, asDataFrame = TRUE)
          })
}


dim.Document =
function(x)
  t(sapply(getPages(x), dim))

# Validity
# 
