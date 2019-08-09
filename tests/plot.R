
library(Dociface)

if(require(ReadPDF)) {
 doc = readPDFXML(list.files("SamplePDFs", pattern = "Amada-2013.xml", full = TRUE))
 plot(doc[[1]])
 plot(doc)
}


if(require(Rtesseract)) {
    doc = OCRDocument("ScannedEgs/Ogunkoya-1990.pdf")
    plot(doc[[1]])
    plot(doc)
}
