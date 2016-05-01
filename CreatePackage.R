library(devtools)
devtools::install_github("klutometis/roxygen")

pkg.dir <- "~/Dropbox/Projects/HexGrid"
setwd(pkg.dir)
create("HexGrid")
document(paste(pkg.dir, "/HexGrid", sep = ""))