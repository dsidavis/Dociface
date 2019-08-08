library(Dociface)

if(require(Rtesseract)) {

d = OCRDocument("ScannedEgs/Ogunkoya-1990.pdf")

class(d)

length(d)
getNumPages(d)
# Why is this an integer of length 2. There is a method for this in Rtesseract/R/OCRDocument.R
# Now that method is not evaluated/defined via an if(FALSE) so inherit the one from Dociface.
dm = dim(d)
stopifnot(is.matrix(dm) && ncol(dm) == 2) # col names are rows and columns.

pgs = getPages(d)
x = d[[1]]
class(x)
stopifnot(identical(pgs[[1]], x))

dim(x)

bb = as(d[[1]], "TextBoundingBox")

# WordOCRResults - no method.
# Ok now: Remove the setOldClass(c("OCRResults", "data.frame")) in OCRResults.R

margins(bb)

# Will do the OCR again (repeating bb computation) - expensive.
margins(x)
}
