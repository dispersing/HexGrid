hexgrid.coords <- function(grid.dim, rotation = "flat-topped", offset = "even", cell.radius = 1){
if (length(grid.dim) > 2) {stop("grid.dim can only be length 1 or 2 (hexagonal grid radius or rectangular grid, respectively)")}
if (any(offset == c("even","odd")) == F) {stop("offset must be either `even' or `odd'.")}
if (any(rotation == c("flat-topped","point-topped")) == F) {stop("rotation must be either `flat-topped' or `point-topped'.")}
if (length(grid.dim) == 1) {

if (rotation == "flat-topped"){
	q.dist <- 1.5*cell.radius
	r.dist <- sqrt(3)*cell.radius
	grid.arr <- array(data = c(
		rep(seq(q.dist, q.dist*grid.dim, q.dist), each = grid.dim),
		rep(seq(r.dist, r.dist*grid.dim, r.dist), times = grid.dim)
	), dim = c(grid.dim, grid.dim, 2))

	dist.arr <- array(data = NA, dim = c(grid.dim, grid.dim, grid.dim^2, 2))

	dist.arr[,,,1] <- outer(grid.arr[,,1], grid.arr[,,1], "-")	
	dist.arr[,,,2] <- outer(grid.arr[,,2], grid.arr[,,2], "-")

print(dist.arr)

	even.q <- seq(2, grid.dim, 2)
	odd.q <- seq(1, grid.dim, 2)

	odd.mats <- as.vector(outer(seq(1, grid.dim^2, grid.dim*2), 0:(grid.dim-1), "+"))
	even.mats <- as.vector(outer(seq((grid.dim+1), grid.dim^2, grid.dim*2), 0:(grid.dim-1), "+"))

	if (offset == "even"){
	dist.arr[, even.q, odd.mats, 2] <- dist.arr[, even.q, odd.mats, 2] - r.dist/2
	dist.arr[, odd.q, even.mats, 2] <- dist.arr[, odd.q, even.mats, 2] + r.dist/2
		} else { # even offset
	dist.arr[, even.q, odd.mats, 2] <- dist.arr[, even.q, odd.mats, 2] + r.dist/2
	dist.arr[, odd.q, even.mats, 2] <- dist.arr[, odd.q, even.mats, 2] - r.dist/2
		} #end odd offset
	} else { # flat-topped else

	r.dist <- 1.5*cell.radius
	q.dist <- sqrt(3)*cell.radius
	grid.arr <- array(data = c(
		rep(seq(q.dist, q.dist*grid.dim, q.dist), each = grid.dim),
		rep(seq(r.dist, r.dist*grid.dim, r.dist), times = grid.dim)
	), dim = c(grid.dim, grid.dim, 2))

	print(grid.arr)

	dist.arr <- array(data = NA, dim = c(grid.dim, grid.dim, grid.dim^2, 2))

	dist.arr[,,,1] <- outer(grid.arr[,,1], grid.arr[,,1], "-")	
	dist.arr[,,,2] <- outer(grid.arr[,,2], grid.arr[,,2], "-")

	even.r <- seq(2, grid.dim, 2)
	odd.r <- seq(1, grid.dim, 2)

	odd.mats <- as.vector(outer(seq(1, grid.dim^2, grid.dim*2), 0:(grid.dim-1), "+"))
	even.mats <- as.vector(outer(seq((grid.dim+1), grid.dim^2, grid.dim*2), 0:(grid.dim-1), "+"))

	if (offset == "even"){
	dist.arr[even.r, , odd.mats, 1] <- dist.arr[even.r, , odd.mats, 1] + r.dist/2
	dist.arr[odd.r, , even.mats, 1] <- dist.arr[odd.r, , even.mats, 1] - r.dist/2
		} else { # even offset
	dist.arr[even.r, , odd.mats, 1] <- dist.arr[even.r, , odd.mats, 1] - r.dist/2
	dist.arr[odd.r, , even.mats, 1] <- dist.arr[odd.r, , even.mats, 1] + r.dist/2
		} #end odd offset

	} # point-topped
	} # grid.dim ==1

	

	return(dist.arr)
	}

rad <- 1
hd <- hexgrid.coords(5, offset ="odd", cell.radius = rad, rotation = "flat-topped")
hexgrid.plot(hd[,,13,1], hd[,,13,2], cell.coords = hexcell.coords(rad, rotation = "flat-topped"))
# abline(v = c(-2:2, sqrt(2)/2, -sqrt(2)/2), h = -2:2)
# hexgrid.plot <- function(x.coords, y.coords, cell.coords = hexcell.coords(1)){
	# n.row <- nrow(x.coords)
	# n.col <- ncol(x.coords)
	# coords <- cell.coords
	# print(coords)
	# plot(0, type = "n", xlim = c(min(x.coords) - abs(min(coords$x)), max(x.coords) + max(coords$x)), ylim = c(min(y.coords) - abs(min(coords$y)), max(y.coords) + max(coords$y)))
# points(x.coords, y.coords)
	# for (i in 1:n.col){
		# for (j in 1:n.row){
		# polygon(x.coords[i,j] + coords$x, y.coords[i,j] + coords$y, col = rgb(0, 0 , 1,.2), border = "grey25")
		# }
	# }
# }

# rm(list=ls())