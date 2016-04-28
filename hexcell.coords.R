hexcell.coords <- function(cell.radius = 1, rotation = "flat-topped"){
	s <- 3^0.5
	if (rotation == "flat-topped") {
		x <- c(cell.radius, cell.radius/2, -cell.radius/2, -cell.radius, -cell.radius/2, cell.radius/2)
		y <- c(0, -cell.radius*s/2, -cell.radius*s/2, 0, cell.radius*s/2, cell.radius*s/2)
		return(list(x = x, y = y))
		}
	if (rotation == "point-topped") {
		x <- c(0, -cell.radius*s/2, -cell.radius*s/2, 0, cell.radius*s/2, cell.radius*s/2)
		y <- c(cell.radius, cell.radius/2, -cell.radius/2, -cell.radius, -cell.radius/2, cell.radius/2)
		return(list(x = x, y = y))
		}
	 else {
	 	warning("Unidentified rotation")
	 	}
	}