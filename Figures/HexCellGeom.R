pdf("~/Dropbox/Projects/HexGrid/Figures/HexCellPoint.pdf", height = 6, width = 6)

# use hexcell.coords to give hexagonal cell coordinates
	cr <- 1
	coords <- hexcell.coords(cr, rotation = "point-topped")

#blank plot
	par(mar = rep(4,4))
	plot(coords, xlab = "", ylab = "", las = 1, type = "n", xlim = c(-cr*1.1, cr*1.1), ylim = c(-cr*1.1, cr*1.1), axes = F, frame = T)

# background lines where the axis tick marks will be
	# abline(v = c(cr, cr*3/4, 0, -cr*3/4, -cr), h = c(cr*sqrt(3)/2, cr*sqrt(3)/4, 0, -cr*sqrt(3)/4, -cr*sqrt(3)/2), col = "grey80", lwd = 3/4)
	abline(h = c(cr, cr/2, 0, -cr/2, -cr), v = c(cr*sqrt(3)/2, 0, -cr*sqrt(3)/2), col = "grey80", lwd = 3/4)


# hexagon
	polygon(coords, lwd = 1.5)

# radii
	arrows(rep(0,6), rep(0,6), coords$x, coords$y, length = 0, col = "grey50", lty = 2, lwd = 1.5)
	# arrows(0, 0, coords$x[1], coords$y[1], length = 0, lty = 1, lwd = 2)
	arrows(0, 0, coords$x[5], coords$y[5], length = 0, lty = 1, lwd = 2)

# right triangle
	# arrows(0, 0, cr*3/4, cr*-sqrt(3)/4, length = 0, lty = 1, lwd = 2)
	arrows(0, 0, cr*sqrt(3)/2, 0, length = 0, lty = 1, lwd = 2)

	# arrows(cr*3/4, cr*-sqrt(3)/4, cr, 0, length = 0, lty = 1, lwd = 2)
	arrows(cr*sqrt(3)/2, 0, cr*sqrt(3)/2, -cr/2, 0, length = 0, lty = 1, lwd = 2)
# points
	points(0, 0, pch = 16, cex = 1.5)
	points(coords, pch = 16)

# text
	# text(cr/2, 0, labels = "cell radius (r)", adj = c(0.5,-0.5))
	# text(cr*sqrt(3)/4, cr*-sqrt(3)/8, labels = expression(sqrt(r)/2), adj = c(0.5,0), srt = 330)
	# text(cr*3/4, cr*-sqrt(3)/8, labels = expression(r/2), adj = c(0.2,0.85), srt = 60)

	text(cr/2, -cr/4, labels = "cell radius (r)", adj = c(0.5,0), srt = 330)
	text(cr/2, 0, labels = expression(sqrt(r)/2), adj = c(0.5,-0.25))
	text(cr*sqrt(3)/2, -cr*sqrt(3)/8, labels = expression(r/2), adj = c(-0.25,0.5))


# axis labels
	# axis(1, at = c(cr, cr*3/4, 0, -cr*3/4, -cr), labels = F)
	# mtext(side = 1, c("r", expression(frac(3*r*sqrt(3),4)), 0, expression(frac(-3*r*sqrt(3),4)), "-r"), at = c(cr, cr*3/4, 0, -cr*3/4, -cr), line = 0.5, padj = 1)
axis(2, at = c(cr, cr/2, 0, -cr/2, -cr), labels = F)
	mtext(side = 2, c("r", expression(frac(r,2)), 0, expression(frac(r,2)), "-r"), at = c(cr, cr/2, 0, -cr/2, -cr), line = 1, las = 1)

	# axis(2, at = c(cr*sqrt(3)/2, cr*sqrt(3)/4, 0, -cr*sqrt(3)/4, -cr*sqrt(3)/2), labels = F)
	# mtext(side = 2, c(expression(frac(r*sqrt(3),2)), expression(frac(r*sqrt(3),4)),0, expression(frac(-r*sqrt(3),4)), expression(frac(-r*sqrt(3),2))), at = c(cr*sqrt(3)/2, cr*sqrt(3)/4, 0, -cr*sqrt(3)/4, -cr*sqrt(3)/2), las = 1, line = 1)
	axis(1, at = c(cr*sqrt(3)/2, 0, -cr*sqrt(3)/2), labels = F)
	mtext(side = 1, c(expression(frac(r*sqrt(3),2)),0, expression(frac(-r*sqrt(3),2))), at = c(cr*sqrt(3)/2, 0, -cr*sqrt(3)/2), las = 1, line = 0.5, padj = 1)


	# axis(3, at = c(cr, cr*3/4, 0, -cr*3/4, -cr), labels = c(cr, cr*3/4, 0, -cr*3/4, -cr))
	axis(4, at = c(cr, cr/2, 0, -cr/2, -cr), labels = c(cr, cr/2, 0, -cr/2, -cr), las = 1)
	
	# axis(4, at = c(cr*sqrt(3)/2, cr*sqrt(3)/4, 0, -cr*sqrt(3)/4, -cr*sqrt(3)/2), labels = round(c(cr*sqrt(3)/2, cr*sqrt(3)/4, 0, -cr*sqrt(3)/4, -cr*sqrt(3)/2),2), las = 1)
	axis(3, at = c(cr*sqrt(3)/2, 0, -cr*sqrt(3)/2), labels = round(c(cr*sqrt(3)/2, 0, -cr*sqrt(3)/2),2))


#legend
text(0, cr*1.1, labels = paste("cell radius = ", cr, sep = ""), cex = 1.25)

dev.off()