library(Dociface)

if(require(ReadPDF)) {

    doc = readPDFXML(list.files("SamplePDFs", pattern = "Amada-2013.xml", full = TRUE))
    tmp = list(a = getShapesBBox(doc),
               b = getShapesBBox(doc[[1]]),
               c = getTextBBox(doc),
               d = getTextBBox(doc[[1]]))

    sapply(tmp, class)
    sapply(tmp, nrow)
    sapply(tmp, ncol)
    sapply(tmp, names)        
}


if(require(Rtesseract)) {

    doc = OCRDocument("ScannedEgs/Mebatsion-1992.pdf")
    f = "ScannedEgs/Mebatsion-1992_p0000.png")
    tmp = list(#a = getShapesBBox(doc),
               b = getShapesBBox(doc[[1]]),
               #c = getTextBBox(doc),
               d = getTextBBox(doc[[1]]))

    sapply(tmp, class)
    sapply(tmp, nrow)
    sapply(tmp, ncol)
    sapply(tmp, names)        
}
