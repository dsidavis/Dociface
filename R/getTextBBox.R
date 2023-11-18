getTextBBox =
function(obj, asDataFrame = TRUE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = TRUE, color = TRUE, ...)
    UseMethod("getTextBBox")


getShapesBBox = 
    #
    # This is for rect and line nodes, not <text> nodes. Use getTextBBox() (or previously getBBox2() in ReadPDF) for that.
    # Here asDataFrame is TRUE. For getBBox() in ReadPDF, it is FALSE for backward-compatability.
function(obj, asDataFrame = TRUE, color = TRUE, diffs = FALSE, dropCropMarks = TRUE, ...)
    #??? Do we have to make an explicit call to getBBox() and pass the arguments or will UseMethod do the right thing.
    # This comment is left over from the ReadPDF implementation when we called getBBox().
  UseMethod("getShapesBBox")



# Coercion methods from a general Document object to different BoundingBox target classes.
setAs("Document", "TextBoundingBox",
      function(from) 
         getTextBBox(from, asDataFrame = TRUE)
     )

setAs("Document", "ShapeBoundingBox",
      function(from) {
         getShapesBBox(from, asDataFrame = TRUE)
     })


bboxForDoc =
    # General function for processing each page object, calling the pageFun specified by the
    #  caller, typically getTextBBox or getShapesBBox
    # resulting in a list of data frames, hopefully BoundingBox objects. Then combine them into one MultiPageBoundingBox
    # if asked to do so.
    # We can use this for applying any function to a collection of pages and do this to implement getTextBBox and getShapesBBox
    # methods for Document objects with one piece of code.
function(pageFun, pages, asDataFrame = FALSE, combinePages = asDataFrame, ...)
{
    ans = lapply(pages, pageFun, asDataFrame, ...)
    if(combinePages) {
        tmp = do.call(rbind, ans)
        tmp$page = rep(seq(along = ans), sapply(ans, function(x) if(is.null(x)) 0L else nrow(x)))
          # Put a label (bless) on the object to indicate it is not a regular TextBoundingBox but multi-page.
        class(tmp) = c("MultiPageBoundingBox", class(ans[[1]]))
        tmp
    } else
        ans
}

#XXX fix the signature.
getTextBBox.Document =
function(obj, asDataFrame = TRUE, color = TRUE, diffs = FALSE, dropCropMarks = TRUE, ...)    
    bboxForDoc(getTextBBox, getPages(obj), asDataFrame, color = color, diffs = diffs, dropCropMarks = dropCropMarks, ...)

getShapesBBox.Document =
function(obj, asDataFrame = TRUE, color = TRUE, diffs = FALSE, dropCropMarks = TRUE, ...)    
    bboxForDoc(getShapesBBox, getPages(obj), asDataFrame, color = color, diffs = diffs, dropCropMarks = dropCropMarks, ...)
