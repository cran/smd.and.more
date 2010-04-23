powercurve.t.test <- 
function(n=NULL, s=NULL, n1=NULL, n2=NULL, s1=NULL, s2=NULL, sme=NULL, mu0=NULL, ...) {

if ( (length(n) == 0) && (length(n1) == 0) && (length(n2) == 0) ) 
	stop("Need to specify either a common sample size for both groups, n,
	 or specify both group sample sizes, n1 and n2, from which 
	 the within-group or pooled standard deviation, sw, is computed.")
if ( (length(s) == 0) && (length(s1) == 0) && (length(s2) == 0) ) 
	stop("Need to specify either a within-group standard deviation, sw,
	 or specify both group standard deviations, s1 and s2, and a
	 common sample size n or individual sample sizes n1 and n2, from which 
	 sw is computed.")
	 

cat("------------------------------------------------------------\n")
if (length(mu0) == 0) {
	mytype <- "two.sample"
	cat("Two Independent Samples Power Analysis for t-test", "\n")
}
else {
	mytype <- "one.sample"
	cat("One Sample Power Analysis for t-test", "\n")
}
cat("------------------------------------------------------------\n")

# power curve for two groups, assuming mean diff of 0
if (mytype == "two.sample") {
		
	# within-group standard deviation	 
	if (length(s) == 0) {
		if (length(n) > 0) {
			n1 <- n
			n2 <- n
		}
		df1 <- n1 - 1
		df2 <- n2 - 1
		ssq <- (df1*s1^2 + df2*s2^2) / (df1 + df2)
		s <- sqrt(ssq)
		cat("\n", "Equal Group Variances Assumed", sep="", "\n")
		cat("Within-group Standard Deviation:  sw = ", s, "\n")
	}
	
	# harmonic mean of two sample sizes
	if ( length(n1)>0 && length(n2)>0 ) {
		n = 2 / ( 1/n1 + 1/n2 )
		cat("\n", "Harmonic mean of the two sample sizes: n = ", n, sep="", "\n")
	}

	# configure range of deltas
	pp <- power.t.test(n=n, sd=s, power=.9999, type=mytype)
	mydeltas <- seq(-pp$delta,pp$delta,pp$delta/100)
	rm(pp)
	
	# values for power curve
	mypower <- power.t.test(n=n, sd=s, delta=mydeltas, type=mytype)
	mytitle <- "Power Curve for Independent Groups t-test"
	myxlab <- bquote(paste("Alternative Values of ", mu[1] - mu[2]))
	s.out <- paste("s=", toString(round(s,4)), sep="")
	H0 <- 0
}

# power values for a single sample, triggered by nonzero mu0
else {
	# configure range of deltas
	pp <- power.t.test(n=n, sd=s, power=.9999, type=mytype)
	mydeltas <- seq(-pp$delta,pp$delta,pp$delta/100)
	rm(pp)
	# values for power curve
	mypower <- power.t.test(n=n, sd=s, delta=mydeltas, type=mytype)
	mytitle <- "Power Curve for One Sample t-test"
	myxlab <- bquote(paste("Alternative Values of ", mu))
	s.out <- toString(round(s,4))
	H0 <- mu0
}

# power curve
color.plot(H0+mydeltas, mypower$power, type="l", xlab=myxlab, ylab="Power", ...)
mtext(mytitle, side=3, line=2.5, cex=1.1, font=2)
mtext(paste("n=", toString(n), ", s=", s.out, sep=""), side=3, line=1, font=3)

# for when the smallest meaningful effect, sme, is provided
if (length(sme) > 0) {

	# power for sme
	pp <- power.t.test(n=n, sd=s, delta=sme, type=mytype)
	p.out <- round(pp$power,3)
	cat("\n", "Power for the smallest meaningful effect, sme, of ", sme, ": power = ", p.out, sep="", "\n")
	rm(pp)
	
	# n needed to achieve a power of 0.8 for sme
	pp <- power.t.test(sd=s, delta=sme, power=0.8, type=mytype)
	n.out <- ceiling(pp$n)
	cat("Needed n to achieve power=0.8 for sme of ", sme, ": n = ", n.out, sep="", "\n")
	if (mytype == "two.sample") cat("Sample size n applies to *each* group.", "\n")
	rm(pp)
	
}

# delta for a power of 0.8
pp <- power.t.test(n=n, sd=s, power=0.8, type=mytype)
delta.out <- round(pp$delta,3)
cat("\n", "Effect size that achieves a power of 0.8: effect = ", delta.out, sep="", "\n\n")
rm(pp)

}
