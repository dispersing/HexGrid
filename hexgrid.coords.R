hexgrid.coords <- function(grid.dim, rotation = "flat-topped", offset = "even", cell.radius = 1){
if (length(grid.dim) > 2) {stop("grid.dim can only be length 1 or 2 (hexagonal grid radius or rectangular grid, respectively)")}
if (length(grid.dim) == 1) {
	if (grid.dim %% 2 == 0) {stop("Hexagonal grid must be odd")} else {
	q.dist <- 1.5*cell.radius
	r.dist <- sqrt(3)*cell.radius
	grid.arr <- array(data = c(
		rep(seq(q.dist, q.dist*grid.dim, q.dist), each = grid.dim),
		rep(seq(r.dist, r.dist*grid.dim, r.dist), times = grid.dim)
	), dim = c(grid.dim, grid.dim, 2))

	dist.arr <- array(data = NA, dim = c(grid.dim, grid.dim, grid.dim^2, 2))

	dist.arr[,,,1] <- outer(grid.arr[,,1], grid.arr[,,1], "-")	
	dist.arr[,,,2] <- outer(grid.arr[,,2], grid.arr[,,2], "-")

	even.q <- seq(2, grid.dim, 2)
	odd.q <- seq(1, grid.dim, 2)

	odd.mats <- as.vector(outer(seq(1, grid.dim^2, grid.dim*2), 0:(grid.dim-1), "+"))
	even.mats <- as.vector(outer(seq((grid.dim+1), grid.dim^2, grid.dim*2), 0:(grid.dim-1), "+"))

	dist.arr[, even.q, odd.mats, 2] <- dist.arr[, even.q, odd.mats, 2] - r.dist/2
	dist.arr[, odd.q, even.mats, 2] <- dist.arr[, odd.q, even.mats, 2] + r.dist/2
	return(dist.arr)

		} # odd hexagon
	} # grid.dim ==1

	



	}

hexgrid.coords <- function(grid.dim, rotation = "flat-topped", offset = "even", cell.radius = 1){
if (length(grid.dim) > 2) {stop("grid.dim can only be length 1 or 2 (hexagonal grid radius or rectangular grid, respectively)")}
if (length(grid.dim) == 1) {
	if (grid.dim %% 2 == 0) {stop("Hexagonal grid must be odd")} else {
	r.dist <- 1.5*cell.radius
	q.dist <- sqrt(3)*cell.radius
	grid.arr <- array(data = c(
		rep(seq(r.dist, r.dist*grid.dim, r.dist), times = grid.dim),
		rep(seq(q.dist, q.dist*grid.dim, q.dist), each = grid.dim)
	), dim = c(grid.dim, grid.dim, 2))

print(grid.arr)
	dist.arr <- array(data = NA, dim = c(grid.dim, grid.dim, grid.dim^2, 2))

	dist.arr[,,,1] <- outer(grid.arr[,,1], grid.arr[,,1], "-")	
	dist.arr[,,,2] <- outer(grid.arr[,,2], grid.arr[,,2], "-")

	even.r <- seq(2, grid.dim, 2)
	odd.r <- seq(1, grid.dim, 2)

	odd.mats <- as.vector(outer(seq(1, grid.dim^2, grid.dim*2), 0:(grid.dim-1), "+"))
	even.mats <- as.vector(outer(seq((grid.dim+1), grid.dim^2, grid.dim*2), 0:(grid.dim-1), "+"))

	dist.arr[even.r,, odd.mats, 2] <- dist.arr[even.r,, odd.mats, 2] + r.dist/2
	dist.arr[odd.r, , even.mats, 2] <- dist.arr[odd.r, , even.mats, 2] - r.dist/2
	return(dist.arr)

		} # odd hexagon
	} # grid.dim ==1

	



	}
hexgrid.coords(5)
plot.hex(hexgrid.coords(5))