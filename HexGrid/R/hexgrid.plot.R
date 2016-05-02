#' Plots hexagonal grids
#'
#' @param x.coords matrix of x coordinates (columns) for each y value (rows)
#' @param y.coords matrix of y coordinates (rows) for each y value (columns)
#' @param cell.coords list of 2 (x and y, from hexcell.coords()) with length of 6, for each of the hexagonal corners
#' @param ... passes arguments to polygon
#' @description I will describe more late
#' @examples
#' # Example 1, default where cell.radius = 1 and hexagon is flat-topped
#'    rad <- 1
#'    par(mfrow = c(2,2), mar = c(2,2,2,2), oma = rep(1,4), cex = 0.75)
#'    hd <- hexgrid.coords(5, offset ="even", cell.radius = rad, rotation = "flat-topped")
#'    hexgrid.plot(hd[,,1,1], hd[,,1,2], cell.coords = hexcell.coords(rad, rotation = "flat-topped"), col = rgb(0,0,1,.1), border = "grey50")
#'    axis(1, labels = T)
#'    axis(2, labels = T, las = 1)
#'    mtext("Flat-topped, even offset", side = 3, line = 0, cex = 0.75)
#'    text(0, 0, labels = "(0, 0)", cex = 0.75)


hexgrid.plot <- function(x.coords, y.coords, cell.coords = hexcell.coords(1), ...){
	n.row <- nrow(x.coords)
	n.col <- ncol(x.coords)
	coords <- cell.coords
	print(coords)
	plot(0, type = "n", xlim = c(min(x.coords) - abs(min(coords$x)), max(x.coords) + max(coords$x)), ylim = c(min(y.coords) - abs(min(coords$y)), max(y.coords) + max(coords$y)), axes = F)
	for (i in 1:n.col){
		for (j in 1:n.row){
		polygon(x.coords[i,j] + coords$x, y.coords[i,j] + coords$y, ...)
		}
	}
}