library(devtools)
library(roxygen2)

pkg.dir <- "~/Dropbox/Projects/HexGrid_Package"
setwd(pkg.dir)
# create("HexGrid")
document(paste(pkg.dir, "/HexGrid", sep = ""))