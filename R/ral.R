ral <- 
function(ref=NULL, ...) {
# read, attach, list

	if (length(ref) == 0) {
		cat("Invoking R statement: mydata <- read.csv(file.choose())", "\n\n")
		mydata <- read.csv(file.choose(), ...)
	}
	else {
		cat("Invoking R statement: mydata <- read.csv(file=ref, ...)", "\n\n")
		mydata <- read.csv(file=ref, ...)
	}
	cat("Invoking R statement: attach(mydata)", "\n\n")
	attach(mydata)
	cat("Invoking R statement: head(mydata)", "\n\n")
	head(mydata)
	
}
