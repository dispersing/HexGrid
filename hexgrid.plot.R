plot.hex <- function(hex.dist.obj, grid.text = F, text.cex){
	dim.obj <- dim(hex.dist.obj)
	par(mar = rep(0.1, 4), mfrow = c(dim.obj[1], dim.obj[2]), oma = rep(0.1,4))


	d <- dim(hex.dist.obj)[1]
	hd
	med <- ((d^2) %/% 2) + 1
	med
	r <- (d %/% 2 )*(sqrt(3)) + 0.00000000000001

	distance <- ((hex.dist.obj[,,,1]^2) + (hex.dist.obj[,,,2]^2))^0.5

	large <- which(distance[,,med] > r, T)
	large.F <- which(distance[,,med] > r)
	for (i in 1:(d*d)){
		for (j in 1:nrow(large)){
		distance[large[j,1],large[j,2],i] <- NA
	}}

	for(m in 1:(dim.obj[1]*dim.obj[2])){
		col.ramp <- colorRampPalette(c("black", "blue", "red", "white"))
		col.pal <- viridis((dim.obj[1]*dim.obj[2]), alpha = 1)[as.numeric(cut(distance[,,m],breaks = (dim.obj[1]*dim.obj[2])))]
		
		x.lims <- c(min(hex.dist.obj[,,m,1], na.rm = T), max(hex.dist.obj[,,m,1], na.rm = T)) + c(-1, 1)
		y.lims <- c(min(hex.dist.obj[,,m,2], na.rm = T), max(hex.dist.obj[,,m,2], na.rm = T)) + c(-1, 1)
		
		plot(hex.dist.obj[,,m,1], hex.dist.obj[,,m,2] , xaxt = "n", yaxt = "n", type = "n", xlim = x.lims, ylim = y.lims)
		hex.coords <- hexcoords(dx = 3/4, dy = sqrt(3)/4, sep = NA)
		size <- 1.17
		hex.coords$x <- hex.coords$x*size
		hex.coords$y <- hex.coords$y*size
		names(hex.coords) <- c("y", "x", "no.sep")
		hexpolygon(hex.dist.obj[,,m,1], hex.dist.obj[,,m,2], hexC = hex.coords, fill = F, col = paste(col.pal, "" ,sep = ""), lty = 1)
		if(grid.text == T) {
			if (missing(text.cex) == T){text.cex <- 3/d}
			text(hex.dist.obj[,,m,1], hex.dist.obj[,,m,2], round(distance[,,m],2), col = "red", cex = text.cex)
		}
	}
}