color.plot <- 
function(x, y, type="p", col.line="darkblue", col.point="blue", col.area="transparent", 
	col.border="transparent", col.grid="grey90", col.bg="ivory", col.box="black", xy.ticks=TRUE, ...) {
	
	if (type != "p" && type != "l" && type != "b") 
		stop("Parameter 'type' can only be p for points, l for line or b for both.")

	#set up plot space with axes
	plot (x,y, type="n", axes=FALSE, ...)
	if (xy.ticks == TRUE) {
		axis(1); axis(2)
	}

	# colored plotting area
	usr <- par("usr")
	rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border=col.box)
	
	# grid lines
	vx <- pretty(x)
	abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid)
	vy <- pretty(y)
	abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid)
	
	# plot
	if (type == "l") {
		# fill area under curve
		polygon(c(x[1],x,x[length(x)]), c(min(y),y,min(y)), col=col.area, border=col.border)
	}
	if (type =="l" | type == "b") {
		lines(x,y, col=col.line, ...)
	}
	if (type == "p" | type == "b") {
		points(x,y, col=col.point, ...)
	}
}
