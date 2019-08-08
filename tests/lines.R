library(Dociface)

if(require(ReadPDF)) {
 doc = readPDFXML(list.files("SamplePDFs", pattern = "Amada-2013.xml", full = TRUE))
 ll = findLineBreaks(as(doc[[1]], "TextBoundingBox"))
}

if(require(Rtesseract)) {
 bb = GetBoxes("ScannedEgs/Mebatsion-1992_p0000.png")
 ll = findLineBreaks(bb, asPositions = FALSE)
 unname(sapply(ll, function(x) paste(x$text, collapse = " ")))
 # It also puts the "is collaborating..." and the "The Journal of Infectious Diseases" on the same line.


 bb = GetBoxes("ScannedEgs/Mebatsion-1992_p0001.png")
 ll = findLineBreaks(bb, asPositions = FALSE)
 unname(sapply(ll, function(x) paste(x$text, collapse = " ")))


 # check the results with nchar() and look for very large outliers.
 bb = GetBoxes("ScannedEgs/Mebatsion-1992_p0002.png")
 ll = findLineBreaks(bb, asPositions = FALSE)
 txt = unname(sapply(ll, function(x) paste(x$text, collapse = " ")))
 summary(nchar(txt))

  # We can see where we miss the line divides in a plot.
 plot(density(bottom(bb), bw = 2), type = "b")
 br = findLineBreaks(bb)
 abline(v = br, col = "red")

  # If we change the minInRun to 2 rather than 3, we get more sensible answers.
 ll = findLineBreaks(bb, asPositions = FALSE, minInRun = 2)
 br = findLineBreaks(bb, minInRun = 2)
 abline(v = br, col = "green")
 txt = unname(sapply(ll, function(x) paste(x$text, collapse = " "))) 

 
}

