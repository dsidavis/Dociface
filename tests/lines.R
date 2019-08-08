library(Dociface)

if(require(ReadPDF)) {
 doc = readPDFXML(list.files("SamplePDFs", pattern = "Amada-2013.xml", full = TRUE))
 ll = findLineBreaks(as(doc[[1]], "TextBoundingBox"))
}

if(require(Rtesseract)) {
 bb = GetBoxes("ScannedEgs/Mebatsion-1992_p0000.png")
 ll2 = findLineBreaks(bb)
}

