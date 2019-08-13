
library(Dociface)

if(require(ReadPDF)) {
    doc2 = readPDFXML("SamplePDFs/Amada-2013.xml")
    bb = lapply(doc2, as, "TextBoundingBox")
    pdoc = new("ProcessedDocument", bb)
    class(getPages(pdoc))    
}


if(require(Rtesseract)) {
    doc = ProcessedOCRDocument("ScannedEgs/Ogunkoya-1990.pdf")
    getNumPages(doc)
    class(getPages(doc))
}
