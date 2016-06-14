library("devtools")

pkg.dir <- "~/Dropbox/Projects/HexGrid_Package"
devtools::use_vignette("my-vignette", pkg = paste(pkg.dir, "/HexGrid", sep = ""))