#' Coordinates for a single hexagonal cell
#'
#' @param cell.radius length from the ceter of the cell to the six corners
#' @returns a list of with horizontal and vertical coordinates, respectively names x and y.
#' @examples
#' hexcell.coords(1)


hexcell.coords <- function(cell.radius = 1, rotation = "flat-topped"){
	s <- sqrt(3)
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