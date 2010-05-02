color.plot <- 
function(x, y=NULL, type="p", col.line="darkblue", col.point="blue", 
           col.area="transparent", col.border="transparent", col.grid="grey90", 
           col.bg="ivory", col.box="black", xy.ticks=TRUE, xlab=NULL, ylab=NULL, 
           center.line=NULL, ...) {
	
	if (type != "p" && type != "l" && type != "b") 
		stop("Parameter 'type' can only be p for points, l for line or b for both.")
		
	if (length(center.line) > 0)
		if (center.line != "mean" && center.line != "median") 
			stop ("center.line must be \"mean\" or \"median\" or ignored.")
	
	if (is.null(y)) {  # only passed one variable, so create x index
		if (is.null(ylab)) y.lbl <- deparse(substitute(x)) else y.lbl <- ylab
		x.lbl <- "Index"
		y <- x
		x <- seq(0,length(y)-1,1)
	}
	else {  # assign axes labels, with default going to variable names
		if (xy.ticks == TRUE) {
			if (is.null(xlab)) x.lbl <- deparse(substitute(x)) else x.lbl <- xlab
			if (is.null(ylab)) y.lbl <- deparse(substitute(y)) else y.lbl <- ylab
		}
		else {
			x.lbl <- ""
			y.lbl <- ""
		}
	}	
	
	# margins
	par(mar=c(4,4,4,3))
	
	# plot
	plot(x, y, type="n", axes=FALSE, xlab=x.lbl, ylab=y.lbl, ...)
	if (xy.ticks == TRUE) {
		axis(1); axis(2)
	}

	# colored plotting area
	usr <- par("usr")
	rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border=col.box)
	
	# grid lines
	vx <- pretty(c(usr[1],usr[2]))
	vy <- pretty(c(usr[3],usr[4]))
	abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid)
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
	
	# plot center line
	if (!is.null(center.line)) {
		cat("----------------------------------------------------\n")
		if (center.line == "mean") {
			m.y <- mean(y)
			lbl <- "mean"
			lbl.cat <- "Mean"
		}
		else if (center.line == "median") {
			m.y <- median(y)
			lbl <- "median"
			lbl.cat <- "Median"
		}
		abline(h=m.y, col="gray50", lty="dashed")
		mtext(lbl, side=4, cex=.9, col="gray50", las=2, at=c(m.y), line=0.1)
		cat(lbl.cat, m.y, "\n")
		cat("----------------------------------------------------\n")
	}

}
