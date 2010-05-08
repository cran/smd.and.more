\name{reg}
\alias{reg}

\title{Regression Analysis}

\description{
Regression analysis successively invokes three standard R functions \code{\link{lm}}, \code{\link{summary}} and \code{\link{confint}}.  The purpose of \code{reg} is to combine these three function calls into one, and to provide suggestions for further analysis.
}

\usage{
reg(my.formula)
}


\arguments{
  \item{my.formula}{Standard R \code{\link{formula}} for specifying a model.  For
       example,  for a response variable named Y and two predictor variables, X1 and X2, specify the corresponding linear model as Y ~ X1 + X2.}
}


\details{
The function \code{reg} invokes the subsequent R statements: \cr\cr
\code{model <- lm(my.formula)} \cr
\code{summary(model)} \cr
\code{confint(model)} \cr\cr
Output of these statements is directed to the console.  Also listed are the R statements invoked by \code{reg}.  Function calls for potential successive analyses complete the output.
}

\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{formula}}, \code{\link{lm}}, \code{\link{summary.lm}}, \code{\link{confint}}.
}

\examples{
# Generate three vectors of uncorrelated, random data
X1 <- rnorm(20)
X2 <- rnorm(20)
Y <- rnorm(20)

# Call reg according to specified model
reg(Y ~ X1 + X2)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }
