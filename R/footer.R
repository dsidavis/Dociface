

getFooterPos =
function(page, docFont = getDocFont(page), 
          bbox = getTextBBox(page),  shapes = getShapesBBox(page))
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
