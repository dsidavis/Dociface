
library(Dociface)

if(require(ReadPDF)){
    doc = readPDFXML(list.files("SamplePDFs", pattern = "Amada-2013.xml", full = TRUE))

    hdr = getHeader(doc[[1]])

    hdr = getHeader(doc)


    ftr = getFooter(doc[[1]])
    ftr = getFooter(doc)
    
}


