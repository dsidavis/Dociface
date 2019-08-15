
library(Dociface)

if(require(ReadPDF)){
    doc = readPDFXML(list.files("SamplePDFs", pattern = "Amada-2013.xml", full = TRUE))
    ll = getFooterPos(doc[[1]])
}
