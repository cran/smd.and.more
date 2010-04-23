smd.t.test.default <-
function(x, y, Ynm = "Y", Xnm = "X", 
           X1nm = "Group1", X2nm = "Group2", conf.level = 0.95,
           digits = 2, bw1 = "nrd", bw2 = "nrd", ...) {


ODDSMD <-
function(YA, YB) {

	cat("------------------------------------------------------------\n")
	cat("Compare", Ynm, "across", Xnm, "levels", X1nm, "and", X2nm, "\n")
	cat("------------------------------------------------------------\n\n")


	cat("------ Description ------\n\n")

	n1 <- length(YA)
	n2 <- length(YB)
	m1 <- mean(YA)
	m2 <- mean(YB)
	s1 <- sd(YA)
	s2 <- sd(YB)
	v1 <- var(YA)
	v2 <- var(YB)

	clpct <- paste(toString(round((conf.level)*100, 2)), "%", sep="")

	m1.out <- round(m1,digits)
	m2.out <- round(m2,digits)
	s1.out <- round(s1,digits)
	s2.out <- round(s2,digits)
	Xnmval <- paste(Xnm, X1nm)
	cat(Ynm, " for ", Xnmval, ":  n = ", n1, ",   mean = ", m1.out, ",   sd = ", s1.out, sep="", "\n")
	Xnmval <- paste(Xnm, X2nm)
	cat(Ynm, " for ", Xnmval, ":  n = ", n2, ",   mean = ", m2.out, ",   sd = ", s2.out, sep="", "\n\n")

	# sw
	df1 <- n1 - 1
	df2 <- n2 - 1
	swsq <- (df1*v1 + df2*v2) / (df1 + df2)
	sw <- sqrt(swsq)
	sw.out <- round(sw,digits)
	cat("Equal Group Variances Assumed, Within-group Standard Deviation:  ", sw.out, "\n\n")

	# mean diff and standardized mean diff
	mdiff <- m1 - m2
	mdiff.out <- round(mdiff,digits)
	cat("Mean Difference of ", Ynm, ":  " , mdiff.out, sep="", "\n\n")
	
	# smd
	d <- mdiff/sw
	d.out <- round(d,digits)
	cat("Standardized Mean Difference of ", Ynm, ", Cohen's d:  ", d.out, sep="", "\n")



	cat("\n\n------ Assumptions ------\n\n")

	cat("Note:  These hypothesis tests perform poorly in small samples, and", "\n")
	cat("       the t-test is typically robust to violations of assumptions.", "\n")
	cat("       Use as heuristic guides instead of interpreting literally.", "\n\n")

	# Normality
	cat("Null hypothesis, for each group, is a normal distribution of ", Ynm, ".", sep="", "\n")
	if (n1 > 30) {
		cat("Group " , X1nm, ": ", "Sample mean is normal because n>30, so no test needed.", sep="", "\n")
	}
	else {
		cat("Group", X1nm, " ")
		if (n1 > 2 && n1 < 5000) {
			nrm1 <- shapiro.test(YA)
			W.1 <- round(nrm1$statistic,min(4,digits+1))
			p.val1 <- round(nrm1$p.value,min(4,digits+1))
			cat(nrm1$method, ":  W = ", W.1, ",  p-value = ", p.val1, sep="", "\n")
		}
		else
			cat("Sample size out of range for Shapiro-Wilk normality test.", "\n")
	}	
	if (n2 > 30) {
		cat("Group " , X2nm, ": ", "Sample mean is normal because n>30, so no test needed.", sep="", "\n")
	}
	else {
		cat("Group", X2nm, " ")
		if (n2 > 2 && n2 < 5000) {
			nrm2 <- shapiro.test(YB)
			W.2 <- round(nrm2$statistic,min(4,digits+1))
			p.val2 <- round(nrm2$p.value,min(4,digits+1))
			cat(nrm2$method, ":  W = ", W.2, ",  p-value = ", p.val2, sep="", "\n")
		}
		else
			cat("Sample size out of range for Shapiro-Wilk normality test.", "\n")
	}	
	cat("\n")

	# Homogeneity of Variance
	# Var Ratio
	if (v1 >= v2) {
		vratio <- v1/v2
		vr <- paste(toString(round(v1,digits+1)), "/", toString(round(v2,digits+1)), sep="")
		df.num <- df1
		df.den <- df2
	}
	else {
		vratio <- v2/v1
		vr <- paste(toString(round(v2,digits+1)), "/", toString(round(v1,digits+1)), sep="")
		df.num <- df2
		df.den <- df1
	}

	v.out <- round(vratio,digits+1)

	p.var <- pf(vratio, df1=df.num, df2=df.den)
	p.var <- 2 * min(p.var, 1-p.var)  # adjust for two-sided test, results same as var.test{stats}
	pv.out <- round(p.var,min(4,digits+1))

	cat("Null hypothesis is equal variances of ", Ynm, ", i.e., homogeneous.", sep="", "\n")

	cat("Variance Ratio test:  F = ", vr, " = ", v.out, ",  df = ", df.num, ";", df.den, ",  p-value = ", 
			pv.out, sep="", "\n")

	# Levene
	YAm <- abs(YA - median(YA))
	YBm <- abs(YB - median(YB))
	t.bf <- t.test(YAm, YBm, var.equal=TRUE)
	tvalue.bf <- round(t.bf$statistic,min(4,digits+1))
	df.bf <- round(t.bf$parameter,min(4,digits+1))
	pvalue.bf <- round(t.bf$p.value,min(4,digits+1))
	title <- "Levene's test, Brown-Forsythe"
	cat(title,":  t = ", tvalue.bf, ",  df = ", df.bf, ",  p-value = ", pvalue.bf, sep="", "\n")



	cat("\n\n------ Inference ------\n\n")
	
	sterr <- sw * sqrt(1/n1 + 1/n2)
	cat("Standard Error of Mean Difference: SE = ", round(sterr,digits), "\n\n")

	# t-test
	tt <- t.test(YA, YB, var.equal=TRUE, conf.level=conf.level)
	tvalue <- round(tt$statistic,min(2,digits))
	pvalue <- round(tt$p.value,min(4,digits+1))
	lb <- round(tt$conf[1],digits)
	ub <- round(tt$conf[2],digits)
	E <- round((ub-lb)/2,digits)
	df <- tt$parameter
	cat("Hypothesis Test of 0 Mean Diff:  t = ", tvalue, ",  df = ", df, ",  p-value = ", pvalue, sep="", "\n\n")
	cat("Margin of Error for ", clpct, " Confidence Level:  ", E, sep="", "\n")
	cat(clpct," Confidence Interval for Mean Difference:  ", lb, " to ", ub, sep="", "\n\n")

	# smd confidence interval	
	check.MBESS <- suppressWarnings(require(MBESS, quietly=TRUE))
	if (check.MBESS) {
		cid <- ci.smd(smd=d, n.1=n1, n.2=n2, conf.level=conf.level)
		deltaL <- round(cid$Lower.Conf.Limit.smd,digits)
		deltaU <- round(cid$Upper.Conf.Limit.smd,digits)
		cat(clpct," Confidence Interval for smd:  ", deltaL, " to ", deltaU, sep="", "\n")
	}
	else {
		cat(">>> The confidence interval for smd requires package MBESS.", "\n")
		cat(">>> Confidence interval for smd not provided here, but all other output unaffected.", "\n")
		cat(">>> To get the MBESS package, run one time only: install.packages('MBESS')", "\n")
		cat(">>> IGNORE resulting 'Error in eval' error message below.", "\n")
	}



	cat("\n\n------ Graphics Smoothing Parameter ------\n\n")

	# densities
	dYA <- density(YA, bw1)
	dYB <- density(YB, bw2)
	cat("Density bandwidth for ", Xnm, " ", X1nm, ": ", round(dYA$bw,digits), sep="", "\n")
	cat("Density bandwidth for ", Xnm, " ", X2nm, ": ", round(dYB$bw,digits), sep="", "\n\n")

	cat("--------------------------------------------------\n")


	# values needed for graph
	minYA.x <- min(dYA$x)
	maxYA.x <- max(dYA$x)
	minYB.x <- min(dYB$x)
	maxYB.x <- max(dYB$x)

	min.x <- min(minYA.x,minYB.x)  # min x coordinate for graph
	max.x <- max(maxYA.x,maxYB.x)  # max x coordinate for graph
	max.y <- max(max(dYA$y),max(dYB$y))  # max y coordinate
	max.y <- max.y+.1*max.y  # allow room in graph region for d info

	# graph
	col.1 <- rgb(.63,.46,.15)
	col.1t <- rgb(.63,.46,.15, alpha=.5)
	col.2 <- rgb(.49,.56,.69)
	col.2t <- rgb(.49,.56,.69, alpha=.5)

	par(mar=c(3,3,8,.4), mgp=c(2,.6,0), cex.axis=1, cex.lab=1)
	plot.new()
	plot.window(xlim=c(min.x,max.x), ylim=c(0,max.y))  # set up coordinate system
	axis(1); axis(2); box()
	title(xlab=Ynm, ylab="Density")

	xleft <- par("usr")[1]  # left side of graph
	xright <- par("usr")[2]  # right side of graph
	ytop <- par("usr")[4]  # height of graph
	ybot <- par("usr")[3]  # bottom of graph

	lines(c(m1,m1), c(0,ytop), lty="solid", lwd=2, col=col.1)  # vertical line for mean
	lines(c(m2,m2), c(0,ytop), lty="twodash", lwd=2, col=col.2)
	polygon(c(minYA.x,dYA$x,maxYA.x), c(0,dYA$y,0), col=col.1t, border=NA, density=10, angle=45)  # curve area
	polygon(c(minYB.x,dYB$x,maxYB.x), c(0,dYB$y,0), col=col.2t, border=NA, density=10, angle=-45)
	segments(minYA.x, 0, maxYA.x, 0, col=col.1)  # bottom border of density curve
	segments(minYB.x, 0, maxYB.x, 0, col=col.2)
	lines(dYA, col=col.1t, lty="solid", lwd=1.5)  # density curve
	lines(dYB, col=col.2t, lty="twodash", lwd=1.5)
	col.lgnd <- "gray25"
	cex.lgnd <- .9

	# legends with descriptive stats (m1 > m2)
	textR <- paste(Xnm,X1nm);  nR <- n1;  mR <- m1.out;  sR <- s1.out;  col.R <- col.1;  aR <- 45
	textL <- paste(Xnm,X2nm);  nL <- n2;  mL <- m2.out;  sL <- s2.out;  col.L <- col.2;  aL <- -45

	radj <- xleft + .02*(max.x-min.x)
	legend("topleft", legend = textL, fill=col.L, density=20, angle=aL, bty="n",
		text.col=col.lgnd, cex=cex.lgnd)
	text(radj, ytop-.08*max.y, label=bquote(paste("n = ", .(nL))),  adj=0, col=col.lgnd, cex=cex.lgnd)
	text(radj, ytop-.12*max.y, label=bquote(paste("m = ", .(mL))),  adj=0, col=col.lgnd, cex=cex.lgnd)
	text(radj, ytop-.16*max.y, label=bquote(paste("s = ", .(sL))),  adj=0, col=col.lgnd, cex=cex.lgnd)
	
	ladj <- xright - .02*(xright-xleft)
	legend("topright", legend = textR, fill=col.R, density=20, angle=aR, bty="n",
		text.col=col.lgnd, cex=cex.lgnd)
	text(ladj, ytop-.08*max.y, label=bquote(paste("n = ", .(nR))), adj=1, col=col.lgnd, cex=cex.lgnd)
	text(ladj, ytop-.12*max.y, label=bquote(paste("m = ", .(mR))), adj=1, col=col.lgnd, cex=cex.lgnd)
	text(ladj, ytop-.16*max.y, label=bquote(paste("s = ", .(sR))), adj=1, col=col.lgnd, cex=cex.lgnd)

	# scale for s-pooled, d, mdiff at top of graph
	mlow <- min(m1, m2)
	mhi  <- max(m1, m2)
	col.d.unit <- "gray30"
	segments(mlow, max.y-.01*max.y, mlow, ytop, lwd=1, col=col.d.unit) # connect first seg to top
	max.i <- max(ceiling(abs(d)), 2)  # provide at least 2 labeled d units on sd scale at top
	for (i in 0:max.i) {  # sd scale at top
		x.i <- mlow+i*sw
		segments(x.i, max.y+.025*max.y, x.i, ytop, col=col.d.unit, lwd=1)  # sd units
		text(x.i, max.y+.01*max.y, labels=i)  # d units counted
		segments(mlow, ytop, x.i, ytop, col=col.d.unit, lwd=4)  # horiz bar connects endpoints
		last.coord.x <- x.i
	}
	segments(last.coord.x, max.y+.025*max.y, last.coord.x, ytop, lwd=1, col=col.d.unit) # connect last seg to top
	text((m1+m2)/2, ytop-.07*max.y, label=d.out)  # print d value towards top
	segments(mlow, ytop-.09*max.y, mhi, ytop-.09*max.y, col=col.d.unit, lwd=2)  # horiz bar connects means
	text((m1+m2)/2, ytop-.11*max.y, label="d")  # print d towards top
	
	text((m1+m2)/2, ybot+.11*max.y, label=mdiff.out)  # print mdiff value towards bottom
	segments(mlow, ybot+.09*max.y, mhi, ybot+.09*max.y, col=col.d.unit, lwd=2)  # horiz bar connects means
	text((m1+m2)/2, ybot+.07*max.y, label="diff")  # print diff towards bottom

	# title area, above graph
	mtext(paste("ODDSMD Plot"), side=3, line=6.6, font=2)
	mtext(paste("Compare",Ynm,"for",Xnm,X1nm,"and",X2nm), side=3, line=5.6, font=3)
	mtext(bquote(paste("    Classic t-test of 0 mean diff:   t = ", .(tvalue), ",  df = ", .(df),
		",   p-value = ", .(pvalue))), side=3, line=4.4, cex=1.08, adj=0)
	mtext(bquote(paste("    ",.(clpct), " Confidence Interval for Mean Difference: ",
		.(lb), " to ", .(ub))), side=3, line=3.3, cex=1.08, adj=0)
	mtext(bquote(paste("    ",.(clpct), " Confidence Interval for Stnd Mean Diff:   ", 
		.(deltaL), " to ", .(deltaU))), side=3, line=2.1, cex=1.08, adj=0)
	mtext(bquote(paste("s-within")), side=3, line=.9, at=(mlow+(last.coord.x))/2, col="gray40")
	mtext(bquote(paste(.(round(sw,2)))), side=3, line=.2, at=(mlow+(last.coord.x))/2, col="gray40")

}

	if ( (length(x) < 2) | (length(y) < 2) ) stop("Need at least two observations per sample.")

	# Always put the group with the largest mean first
	if (mean(x) > mean(y))
		ODDSMD(x, y)
	else {  # switch
		Xtmp <- X2nm
		X2nm <- X1nm
		X1nm <- Xtmp
		ODDSMD(y, x)
	}

}