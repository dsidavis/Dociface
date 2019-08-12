library(Dociface); library(ReadPDF)
doc2 = readPDFXML("SamplePDFs/Amada-2013.xml")

wd = getPageWidth(doc2)
tmp = findEmptyRegion(wd[2]/2, getTextBBox(doc2[[2]]))
stopifnot(identical(tmp, structure(list(left = 455L, top = 78, right = 481L, bottom = Inf), class = "data.frame", row.names = "left")))

f = function(page, ...)
{    
    tmp = findEmptyRegion(getPageWidth(page)/2, getTextBBox(page), ...)
    plot(page, shapes = NULL)
    ph = getPageHeight(page)
    sapply(1:nrow(tmp), function(i) { tmp = tmp[i,]; rect(tmp$left, ph - tmp$top,  tmp$right, ph - min(tmp$bottom, ph), col = "blue")})
    tmp
}
f(doc2[[2]])
f(doc2[[3]])
f(doc2[[8]])

# Page 7 of Amada.


doc = readPDFXML("SamplePDFs/Klempa-2003.xml")

tmp = findEmptyRegion(452, getTextBBox(doc[[2]]))

#XXX
# doc = Klempa-2003
# doc2 = Amada-2003
# f(doc[[3]]) # stack overflow
# same with doc2[[4]]
