library(Dociface)
if(require(ReadPDF)) {


d = readPDFXML("SamplePDFs/Amada-2013.xml")

class(d)

length(d)
getNumPages(d)
dm = dim(d)
stopifnot(is.matrix(dm) && ncol(dm) == 2 && all(colnames(dm) == c("width", "height")))

pgs = getPages(d)
x = d[[1]]
class(x)
stopifnot(identical(pgs[[1]], x))

dim(x)

margins(x)
}
