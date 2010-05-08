reg <-
function(my.formula) {

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

model <<- lm(my.formula)
summary(model)
confint(model)

if (nargs() > 1) cat("Plus the optional arguments that were entered in reg.", "\n")

cat(line, pre, " my.formula <- ", sep="")
capture.to.console(my.formula)
cat(pre, "model <- lm(my.formula)\n")
cat(pre, " summary(model)", "\n", line, sep="")
capture.to.console(summary(model))

cat("\n", line, pre, " confint(model)", "\n", sep="", line, "\n")
capture.to.console(confint(model))

cat("\n\n", line, sep="")
cat("More analyses, all but anova apply to each row of data\n")
cat(line)
cat("anova(model)     Analysis of Variance\n")
cat("resid(model)     Residuals, i.e., modeling error\n")
cat("fitted(model)    Fitted or estimated values, Yhat\n")
cat("rstudent(model)  Studentized residuals\n")
cat("cooks.distance(model) Cook's distance for influence diagnostics\n")
cat("predict(model, interval=\"prediction\") Forecasting error\n")
cat(line, "\n")

}
