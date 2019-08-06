
setClass("Document", contains = "VIRTUAL")
setClass("DocumentPage", contains = "VIRTUAL")



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
setMethod("getNumPages", "Document",
          function(doc, ...)
            length(doc))
