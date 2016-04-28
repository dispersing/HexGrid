hexgrid.coords <- function(grid.dim){
if (length(grid.dim) > 2) {stop("grid.dim can only be length 1 or 2 (hexagonal grid radius or rectangular grid, respectively)")}
if (length(grid.dim) == 1) {
	if (grid.dim %% 2 == 0) {stop("Hexagonal grid must be odd")} else {
	col.dist <- 1.5
	row.dist <- sqrt(3)
	grid.arr <- array(data = c(
		rep(seq(col.dist, col.dist*grid.dim, col.dist), each = grid.dim),
		rep(seq(row.dist, row.dist*grid.dim, row.dist), times = grid.dim)
	), dim = c(grid.dim, grid.dim, 2))

	dist.arr <- array(data = NA, dim = c(grid.dim, grid.dim, grid.dim^2, 2))

	dist.arr[,,,1] <- outer(grid.arr[,,1], grid.arr[,,1], "-")	
	dist.arr[,,,2] <- outer(grid.arr[,,2], grid.arr[,,2], "-")

	even.q <- seq(2, grid.dim, 2)
	odd.q <- seq(1, grid.dim, 2)
	
	odd.mats <- as.vector(outer(seq(1, grid.dim^2, grid.dim*2), 0:(grid.dim-1), "+"))
	even.mats <- as.vector(outer(seq((grid.dim+1), grid.dim^2, grid.dim*2), 0:(grid.dim-1), "+"))

	dist.arr[, even.q, odd.mats, 2] <- dist.arr[, even.q, odd.mats, 2] + row.dist/2
	dist.arr[, odd.q, even.mats, 2] <- dist.arr[, odd.q, even.mats, 2] - row.dist/2
	return(dist.arr)
		} # odd hexagon
	} # grid.dim ==1
	

	}