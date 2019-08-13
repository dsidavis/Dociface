
library(Dociface); library(Rtesseract)
doc = OCRDocument("ScannedEgs/Ogunkoya-1990.pdf")
bb = readRDS("bb.rds")
sh = getShapesBBox(doc[[1]])
plot(bb, shapes = sh, cex = 1)


plot(doc[[2]])
