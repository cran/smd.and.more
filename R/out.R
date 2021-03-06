out <- 
function(myfile="mydata.csv") {

if (!exists("mydata")) stop("First need to have a data frame called mydata.")

capture.to.console <-
function (...) {
	args <- substitute(list(...))[-1L]
	sink(stdout())
	on.exit({ sink() })
	pf <- parent.frame()
	evalVis <- function(expr) withVisible(eval(expr, pf))
		for (i in seq_along(args)) {
			expr <- args[[i]]
			tmp <- switch(mode(expr), expression = lapply(expr, evalVis), 
				call = , name = list(evalVis(expr)), stop("bad argument"))
			for (item in tmp) if (item$visible) print(item$value)
		}
	on.exit()
}

pre <- ">"
line <- "------------------------------------------------------------\n"

cat(line, pre, sep="")
cat(" write.csv(mydata, file=\"",myfile, "\", row.names=FALSE)", sep="", "\n")
cat(line)

write.csv(mydata, file=myfile, row.names=FALSE)

if (getwd() =="/")
  workdir <- "top level of your file system"
else
  workdir <- getwd()
cat("csv file of mydata contents written at current working directory.\n")
cat("       ", myfile, "at:  ", workdir, "\n")

}
