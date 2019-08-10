getTextBBox =
function(obj, asDataFrame = TRUE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = TRUE, color = TRUE, ...)
    UseMethod("getTextBBox")


getShapesBBox = 
    #
    # This bbox function expects an attribute named bbox
    # This is for rect and line nodes, not <text> nodes. Use getBBox2() for that.
    # Here asDataFrame is TRUE. For getBBox() it is FALSE for backward-compatability.
function(obj, asDataFrame = TRUE, color = TRUE, diffs = FALSE, dropCropMarks = TRUE, ...)
    #??? Do we have to make an explicit call to getBBox() and pass the arguments or will UseMethod do the right thing.
  UseMethod("getShapesBBox")



setAs("Document", "TextBoundingBox",
      function(from) {
         getTextBBox(from, asDataFrame = TRUE)
#        pgs = getPages(from)
#        tmp = lapply(pgs, as, "TextBoundingBox")
#        ans = do.call(rbind, tmp)
#        ans$page = rep(seq(along = pgs), sapply(tmp, nrow))
#        ans
      })

setAs("Document", "ShapeBoundingBox",
      function(from) {
         getTextBBox(from, asDataFrame = TRUE)
     })

getTextBBox.Document =
function(obj, asDataFrame = TRUE, color = TRUE, diffs = FALSE, dropCropMarks = TRUE, ...)    
    bboxForDoc(getTextBBox, getPages(obj), asDataFrame, color = color, diffs = diffs, dropCropMarks = dropCropMarks, ...)

getShapesBBox.Document =
function(obj, asDataFrame = TRUE, color = TRUE, diffs = FALSE, dropCropMarks = TRUE, ...)    
    bboxForDoc(getShapesBBox, getPages(obj), asDataFrame, color = color, diffs = diffs, dropCropMarks = dropCropMarks, ...)


bboxForDoc =
function(pageFun, nodes, asDataFrame = FALSE, combinePages = asDataFrame, ...)
{    
    ans = lapply(nodes, pageFun, asDataFrame, ...)
    if(combinePages) {
        tmp = do.call(rbind, ans)
        tmp$page = rep(seq(along = ans), sapply(ans, function(x) if(is.null(x)) 0L else nrow(x)))
        class(tmp) = c("MultiPageBoundingBox", class(ans[[1]]))
        tmp
    } else
       ans
}
