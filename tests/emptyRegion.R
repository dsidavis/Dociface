library(Dociface); library(ReadPDF)
doc2 = readPDFXML("SamplePDFs/Amada-2013.xml")

wd = getPageWidth(doc2)
tmp = findEmptyRegion(wd[2]/2, getTextBBox(doc2[[2]]))
stopifnot(identical(tmp, structure(list(left = 455L, top = 78, right = 481L, bottom = Inf), class = "data.frame", row.names = "left")))

f = function(page, ...)
{
    pos = getPageWidth(page)/2
    tmp = findEmptyRegion(pos, getTextBBox(page), ...)
    if(nrow(tmp) > 0) {
        plot(page, shapes = NULL)
        abline(v = pos, col = "red")            
        ph = getPageHeight(page)
        sapply(1:nrow(tmp), function(i) { tmp = tmp[i,]; rect(tmp$left, ph - tmp$top,  tmp$right, ph - min(tmp$bottom, ph), col = "blue")})
    }
    tmp
}
f(doc2[[2]])
f(doc2[[3]])
# pages 4 and 5 have images which we don't yet handle. Actually we do seem to do pretty well.
# These both exhibit the phenomenon of including a line that "crosses" @pos but which has
# two consecutive words that end and start around pos, but neither word actually overlaps pos.
# As a result, we end up with a very skinny empty region that starts too high up the page (as we look at it in a plot.)

f(doc2[[8]])
f(doc2[[7]])  # 2 blocks - one in a table and the other correct in the text.
  # the first bloc isn't centered on @pos
  #  pos = 459, and the left is 453 and the right is 482.
  # The second block which is correct is 438 and 465. This is also not 

# Page 7 of Amada.

ans1 = structure(list(left = 438L, top = 625L, right = 465L, bottom = 1129L, 
    numLines = 879L), row.names = "10", class = c("EmptyRegion", 
"BoundingBox", "data.frame"))
stopifnot(identical(f(doc2[[1]]), ans1))


z = doc2[[6]]
# Slightly odd - gets the margin, but it runs through a line that does cross.



#######################################

doc = readPDFXML("SamplePDFs/Klempa-2003.xml")

tmp = findEmptyRegion(452, getTextBBox(doc[[2]]))

#XXX
# doc = Klempa-2003
# doc2 = Amada-2003
# f(doc[[3]]) # stack overflow
#
# The 10 lines that cross the mid point are at the bottom.
# The page has an image just above these 10 lines in the figure caption.
# The regular text is above this.
# There is one line below the figure caption that is short and so does not cross the @pos
# So these 10 lines that cross divide the regular text into 2 groups - above and below.
# However, getTextLines() puts this with the previous line. So that is not the issue.
#
# When we compute cline - we get 10 values identify
#
# The length of lineBreaks is 47. The length of ll - the actual number of lines is 33.
# Bad things happen as a result.
# cline (the indices of the lines that cross pos) is  [1] 38 39 40 41 42 43 44 45 46 47
# If we compute the names and then evaluate
#    names(ll)[cline]
# we get all NAs
# others is 1:37. 
#
# same with doc2[[4]]





fi = system.file("samples", "3Column.pdf", package = "ReadPDF")
doc3 = readPDFXML(fi)
pw = getPageWidth(doc3)
p1 = findEmptyRegion(pw[1]/3, getTextBBox(doc3[[1]]))



fi = system.file("samples", "2Column.pdf", package = "ReadPDF")
doc4 = readPDFXML(fi)
f(doc4[[1]])
f(doc4[[2]])



fi = system.file("samples", "RClang.pdf", package = "ReadPDF")
# page 18 and 27 has 2 columns - sort of.
#  And our function gets both.
doc4 = readPDFXML(fi)
ans = lapply(doc4, function(p) 
                      findEmptyRegion(getPageWidth(p)/2, getTextBBox(p)))


# f(doc4[[2]])  triggers the "what do we do in this case?"
# Fixed now - does return empty data.frame().
