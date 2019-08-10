
library(Dociface)

if(require(ReadPDF)) {
 doc = readPDFXML(list.files("SamplePDFs", pattern = "Amada-2013.xml", full = TRUE))
 plot(doc[[1]])
 plot(doc)


    # MultiPageBoundingBox and plot() method.
    doc = readPDFXML("SamplePDFs/Amada-2013.xml")
    bb = getTextBBox(doc, asDataFrame = TRUE, combinePages = TRUE)
    class(bb)
    plot(bb)
}


if(require(Rtesseract)) {
    doc = OCRDocument("ScannedEgs/Ogunkoya-1990.pdf")
    plot(doc[[1]])
    plot(doc)




  # multi page.
    doc = OCRDocument("ScannedEgs/Ogunkoya-1990.pdf")
    bb = getTextBBox(doc, asDataFrame = TRUE, combinePages = FALSE)
    doc2 = ProcessedOCRDocument(bboxes = bb)
    plot(doc2)


    doc3 = ProcessedOCRDocument("ScannedEgs/Ogunkoya-1990.pdf")
    length(doc3)
    names(doc3)
    sapply(doc3, class)
    plot(doc3)


    # MultiPageBoundingBox and plot() method.
    # Do for XML document also
    doc = OCRDocument("ScannedEgs/Ogunkoya-1990.pdf")
    bb = getTextBBox(doc, asDataFrame = TRUE, combinePages = TRUE)
    class(bb)
    plot(bb)
}
