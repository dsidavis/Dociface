
lapply.Document = # lapply.ConvertedPDFDoc  =  lapply.PDFToXMLDoc = # PDFMinerDoc =
function(X, FUN, ...)
    lapply(getPages(X), FUN, ...)
setMethod("lapply", "Document", lapply.Document)

sapply.Document = # sapply.ConvertedPDFDoc = sapply.PDFToXMLDoc = # PDFMinerDoc =
function(X, FUN, ...)
  sapply(getPages(X), FUN, ...)

setMethod("sapply", "ConvertedPDFDoc", sapply.Document)
