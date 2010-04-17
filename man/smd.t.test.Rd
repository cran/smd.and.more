\name{smd.t.test}
\alias{smd.t.test}
\alias{smd.t.test.default}
\alias{smd.t.test.formula}

\title{Generic Method for t-test and Standardized Mean Difference with Enhanced Graphics}

\description{
Provides enhanced output from the standard \code{t.test} function applied to
the independent groups analysis of the mean difference.  
This output includes the basic descriptive statistics, the pooled or 
within-group standard deviation, and the standardized mean difference or Cohen's \emph{d} and its
confidence interval, as well
as an evaluation of assumptions. The output
introduces the \acronym{ODDSMD} plot, which displays the Overlapping Density Distributions of the 
two groups as well as the means, mean difference and Standardized Mean Difference. 
The plot also includes the results of the descriptive and inferential analyses.
}

\usage{
smd.t.test(x, ...)

\method{smd.t.test}{default}(x, y, Ynm = "Y", Xnm = "X", 
           X1nm = "Group1", X2nm = "Group2", conf.level = 0.95,
           digits = 2, bw1 = "nrd", bw2 = "nrd", ...)

\method{smd.t.test}{formula}(formula, data, ...)
}

\arguments{
  \item{formula}{Formula of the form Y ~ X, where Y is the numeric response variable 
        compared across the two groups, and X is a grouping variable (factor) with two levels that define
        the corresponding groups.}
  \item{data}{An optional matrix or data frame containing the variables in 
        the formula. By default the variables are taken from environment (formula).}
  \item{conf.level}{Confidence level of the interval, expressed as a proportion.}
  \item{digits}{Number of decimal places for which to display numeric values. Suggestion only.}
  \item{bw1}{Bandwidth for the computation of the densities for the first group.}
  \item{bw2}{Bandwidth for the computation of the densities for the second group.}
  \item{x}{Values of response variable for first group.}
  \item{y}{Values of response variable for second group.}
  \item{Ynm}{Name of response variable.}
  \item{Xnm}{Name of predictor variable, the grouping variable or factor with exactly two levels.}
  \item{X1nm}{Value of grouping variable, the level that defines the first group.}
  \item{X2nm}{Value of grouping variable, the level that defines the second group.}
  \item{...}{Further arguments to be passed to or from methods.}
}

\details{
The two methods for the generic function \code{smd.t.test} are formula and default.  
Apply the formula method when the data are in a standard dataframe
that includes a variable, a factor generically referred to as X, that has exactly two values, 
and a numerical response variable, generically referred to as Y.
The formula is of the form Y ~ X, with the names Y and X replaced by the actual variable names specific
to a particular analysis.  The formula method automatically retrieves the names of the variables and
data values for display on the resulting output.

The default method requires two vectors of data, the values of the
response variable Y for each of the two groups.  When submitting data to \code{smd.t.test} as two vectors,
the results are enhanced if  
the actual names of the variables referred to generically as X and Y, as well as the names of the 
levels of the factor X, are explicitly provided. 

This version of \code{smd.t.test} assumes homogeneity of variance in the computation of the
standard error of the mean difference. Also, only a two-sided test is provided.  The null hypothesis is a
population mean difference of 0.

The bandwidth parameter controls the smoothness of the estimated density curve.  To obtain a smoother curve,
increase the bandwidth from the default value.

For the output, the two groups are automatically arranged so that the group with the larger mean 
is listed as the first group.  The result is 
that the resulting mean difference, as well as the standardized mean difference, is always non-negative.
}

\author{David W. Gerbing \email{davidg@sba.pdx.edu}}

\examples{
# ----------------------------------------------------------
# Data stored in a dataframe, call smd.t.test with a formula
# ----------------------------------------------------------

# Create dataframe with simulated data
n <- 10
X <- sample(c("Group1","Group2"), size=n, replace=TRUE)
Y <- rnorm(n=n, mean=50, sd=10)
mydata <- data.frame(cbind(X,Y))
names(mydata) <- c("X","Y")
# Attach the dataframe
attach(mydata)

# Analyze data with formula version
# Y is numeric, X has two values only
# Variable names and levels of X are automatically obtained from data
smd.t.test(Y ~ X)


# -------------------------------------------------------
# Data stored in two vectors, call smd.t.test accordingly
# -------------------------------------------------------

# Create two separate vectors of response variable Y
n <- 10
Y1 <- rnorm(n=n/2, mean=50, sd=10)
Y2 <- rnorm(n=n/2, mean=60, sd=10)

# Analyze the two vectors directly
# Usually explicitly specify variable names and levels of X
#   to enhance the readability of the output
smd.t.test(Y1, Y2, Ynm="MyY", Xnm="MyX", X1nm="Group1", X2nm="Group2")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ t.test }
\keyword{ smd }