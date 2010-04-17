color.plot <- 
function(x, y, col.fg="darkblue", col.bg="ivory", col.area="transparent", 
	col.grid="grey90") {

	#set up plot space with axes
	plot (x,y, type="n", xlab="", ylab="")
	
	# colored plotting area
	usr <- par("usr")
	rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border=col.fg)
	
	# grid lines
	vx <- pretty(x)
	abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid)
	vy <- pretty(y)
	abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid)
	
	# fill area under curve
	colborder="black"
#	if (col.area == "transparent") col.fg="transparent"l
	polygon(c(x[1],x,x[length(x)]), c(min(y),y,min(y)), col=col.area, border=col.fg)
	
	# plot
	lines(x,y, col=col.fg, lwd=1.5)

}

