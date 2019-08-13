
library(Dociface)

if(require(ReadPDF)) {

    doc2 = readPDFXML("SamplePDFs/Amada-2013.xml")
    bb = getTextBBox(doc2[[1]])
    w = isBold(bb)
    table(w)
}
