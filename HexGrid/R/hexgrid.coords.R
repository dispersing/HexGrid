

hexgrid.coords <- function(grid.dim, rotation = "flat-topped", offset = "even", cell.radius = 1){
if (length(grid.dim) > 2) {stop("grid.dim can only be length 1 or 2 (hexagonal grid radius or rectangular grid, respectively)")}
if (any(offset == c("even","odd")) == F) {stop("offset must be either `even' or `odd'.")}
if (any(rotation == c("flat-topped","point-topped")) == F) {stop("rotation must be either `flat-topped' or `point-topped'.")}
if (length(grid.dim) == 1) {

if (rotation == "flat-topped"){
	q.dist <- 1.5*cell.radius #rows are 1.5 radii apart
	r.dist <- sqrt(3)*cell.radius #columns are 3^0.5 radii apart
	grid.arr <- array(data = c(
		rep(seq(q.dist, q.dist*grid.dim, q.dist), each = grid.dim),
		rep(seq(r.dist, r.dist*grid.dim, r.dist), times = grid.dim)
	), dim = c(grid.dim, grid.dim, 2)) #grid with distances away from 0 (first element in thrid dimention is column distances, second is row)
print(grid.arr)
	dist.arr <- array(data = NA, dim = c(grid.dim, grid.dim, grid.dim^2, 2)) #empty array with a third dimention for distance from each coordiante, for columns [,,,1] and rows [,,,2]

	dist.arr[,,,1] <- outer(grid.arr[,,1], grid.arr[,,1], "-")	# column distances
	dist.arr[,,,2] <- outer(grid.arr[,,2], grid.arr[,,2], "-") # row distances

	even.q <- seq(2, grid.dim, 2) # even columns
	odd.q <- seq(1, grid.dim, 2) #odd columns

	odd.mats <- as.vector(outer(seq(1, grid.dim^2, grid.dim*2), 0:(grid.dim-1), "+")) # odd third dimensions
	even.mats <- as.vector(outer(seq((grid.dim+1), grid.dim^2, grid.dim*2), 0:(grid.dim-1), "+")) #even third dimensions

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
	dist.arr[even.r, , odd.mats, 1] <- dist.arr[even.r, , odd.mats, 1] + q.dist/2
	dist.arr[odd.r, , even.mats, 1] <- dist.arr[odd.r, , even.mats, 1] - q.dist/2
		} else { # even offset
	dist.arr[even.r, , odd.mats, 1] <- dist.arr[even.r, , odd.mats, 1] - q.dist/2
	dist.arr[odd.r, , even.mats, 1] <- dist.arr[odd.r, , even.mats, 1] + q.dist/2
		} #end odd offset

	} # point-topped
	} # grid.dim ==1

	

	return(dist.arr)
	}

# # fig.dir <- "~/Dropbox/Projects/HexGrid_package/HexGrid/man/figures"
# # setwd(fig.dir)
# # png(paste(fig.dir, "/FlatPointEvenOdd.png",sep = ""), res = 100)
# rad <- 1
# par(mfrow = c(2,2), mar = c(2,2,2,2), oma = rep(1,4), cex = 0.75)
# hd <- hexgrid.coords(5, offset ="even", cell.radius = rad, rotation = "flat-topped")
# hexgrid.plot(hd[,,1,1], hd[,,1,2], cell.coords = hexcell.coords(rad, rotation = "flat-topped"), col = rgb(0,0,1,.1), border = "grey50")
# axis(1, labels = T)
# axis(2, labels = T, las = 1)
# mtext("Flat-topped, even offset", side = 3, line = 0, cex = 0.75)
# text(0, 0, labels = "(0, 0)", cex = 0.75)

# hd <- hexgrid.coords(5, offset ="odd", cell.radius = rad, rotation = "flat-topped")
# hexgrid.plot(hd[,,13,1], hd[,,1,2], cell.coords = hexcell.coords(rad, rotation = "flat-topped"), col = rgb(0,0,1,.1), border = "grey50")
# axis(1, labels = T)
# axis(2, labels = T, las = 1)
# mtext("Flat-topped, odd offset", side = 3, line = 0, cex = 0.75)
# text(0, 0, labels = "(0, 0)", cex = 0.75)

# hd <- hexgrid.coords(5, offset ="even", cell.radius = rad, rotation = "point-topped")
# hexgrid.plot(hd[,,1,1], hd[,,1,2], cell.coords = hexcell.coords(rad, rotation = "point-topped"), col = rgb(0,0,1,.1), border = "grey50")
# axis(1, labels = T)
# axis(2, labels = T, las = 1)
# mtext("Point-topped, even offset", side = 3, line = 0, cex = 0.75)
# text(0, 0, labels = "(0, 0)", cex = 0.75)

# hd <- hexgrid.coords(5, offset ="odd", cell.radius = rad, rotation = "point-topped")
# hexgrid.plot(hd[,,1,1], hd[,,1,2], cell.coords = hexcell.coords(rad, rotation = "point-topped"), col = rgb(0,0,1,.1), border = "grey50")
# axis(1, labels = T)
# axis(2, labels = T, las = 1)
# mtext("Point-topped, odd offset", side = 3, line = 0, cex = 0.75)
# text(0, 0, labels = "(0, 0)", cex = 0.75)

# # dev.off()