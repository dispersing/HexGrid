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