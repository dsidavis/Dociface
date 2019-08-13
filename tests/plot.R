
library(Dociface)

if(require(ReadPDF)) {
 doc = readPDFXML(list.files("SamplePDFs", pattern = "Amada-2013.xml", full = TRUE))
 plot(doc[[1]])
 plot(doc)
 plot(doc[[1]], colors = "red")
 plot(doc[[1]], colors = c("red", "green"))

    # MultiPageBoundingBox and plot() method.
 doc = readPDFXML("SamplePDFs/Amada-2013.xml")
 bb = getTextBBox(doc, asDataFrame = TRUE, combinePages = TRUE)
 class(bb)
 plot(bb)




 plot(doc2[[1]], shapes = NULL, boxes = TRUE)
 plot(doc2[[1]], shapes = NULL, boxes = TRUE, cex = .5)
 plot(doc2[[1]], shapes = NULL, boxes = TRUE, cex = 1)
 plot(doc2[[1]], shapes = NULL, boxes = TRUE, cex = I(.5))

 o = getTextBBox(doc2[[1]], font = TRUE)
 plot(o, shapes = NULL, cex = rep(.4, nrow(o)))

}


if(require(Rtesseract)) {
    doc = OCRDocument("ScannedEgs/Ogunkoya-1990.pdf")
    plot(doc[[1]])
    plot(doc)


    bb = getTextBBox(doc[[1]])
    sh = getLines(doc[[1]], , 3)
    plot(bb, shapes = sh)
    selectMethod("plot", "OCRResults") 




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
