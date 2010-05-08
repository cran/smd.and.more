smd.t.test.formula <-
function (formula, data, ...) {

	if ((length(formula) != 3) || (length(attr(terms(formula[-2]),"term.labels")) != 1)) 
			stop("'Formula' missing or incorrect.")
	m <- match.call(expand.dots = FALSE)
	if (is.matrix(eval(m$data, parent.frame()))) 
			m$data <- as.data.frame(data)
	m[[1L]] <- as.name("model.frame")
	m$... <- NULL
	mf <- eval(m, parent.frame())
	Ynm <- names(mf)[1]
	Xnm <- names(mf)[2]
	DNAME <- paste(names(mf), collapse = " by ")
	names(mf) <- NULL
	response <- attr(attr(mf, "terms"), "response")
	g <- factor(mf[[-response]])
	if (nlevels(g) != 2) 
			stop("Grouping factor must have exactly 2 levels.")
	gu <- unique(g)
	DATA <- split(mf[[response]], g)
	names(DATA) <- c("Y1", "Y2")
	attach(DATA, warn.conflicts=FALSE)
	
	smd.t.test(Y1, Y2, Ynm=Ynm, Xnm=Xnm, X1nm=levels(gu)[1], X2nm=levels(gu)[2], ...)

}
