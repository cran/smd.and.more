\name{powercurve.t.test}
\alias{powercurve.t.test}

\title{Compute a Power Curve with Colors}

\description{
From the sample size and either the within-cell or pooled standard deviation, or the two separate group standard deviations, automatically calibrate and calculate a power curve for the independent-groups t-test or one-sample t-test, as well as ancillary statistics.  Uses the \code{\link{color.plot}} function in this package to automatically display the power curve with colors, either by default or explicit specification of values of the relevant parameters. Also, for the two-sample test, automatically calculates the within-group standard deviation from the two separate group standard deviations if not provided directly.
}

\usage{
powercurve.t.test(n=NULL, s=NULL, n1=NULL, n2=NULL, s1=NULL, s2=NULL, 
         mmd=NULL, msmd=NULL, mdp=.8, mu0=NULL, \dots)
}


\arguments{
  \item{n}{Sample size for each of the two groups.}
  \item{s}{Within-group, or pooled, standard deviation.}
  \item{n1}{Sample size for Group 1.}
  \item{n2}{Sample size for Group 2.}
  \item{s1}{Sample standard deviation for Group 1.}
  \item{s2}{Sample standard deviation for Group 2.}
  \item{mmd}{Minimum Mean Difference of practical importance, the difference of the
       response variable between two group means. The concept is optional, and only one of mmd and msmd is provided.}
  \item{msmd}{For the Standardized Mean Difference, Cohen's d, the Minimum value of
       practical importance. The concept is optional, and only one of mmd and msmd is provided.}
  \item{mdp}{Minimum Desired Power, the smallest value of power considered to provide sufficient power. Default is 0.8. If changed to 0 then the concept is dropped from the analysis.}
  \item{mu0}{Hypothesized mean, of which a provided value triggers a one-sample 
       analysis.}
  \item{\dots}{Other parameter values, such as lwd and cex.lab from \code{\link{plot}} and col.line and col.bg from \code{\link{color.plot}}.}
}


\details{
This function relies upon the standard \code{\link{power.t.test}} function to calibrate and then calculate the power curve according to the relevant non-central t-distribution. The \code{\link{color.plot}} function from this package, which in turn relies upon the standard \code{\link{plot}} function, plots the power curve. As such, parameters in \code{\link{color.plot}} for controlling the different colors and other aspects of the display are also available, as are many of the more basic parameters in the usual \code{\link{plot}} function.

Also plotted, if provided, is the minimal meaningful difference, mmd, as well as the minimal desired power, mdp, provided by default.  Relevant calculations regarding these values are also displayed at the console.  One or both concepts can be deleted from the analysis.  Not providing a value mmd implies that the concept will not be considered, and similarly for setting mdp to 0.

Invoke the function with the either the within-group (pooled) standard deviation, s, or the two separate group standard deviations, s1 and s2, from which s is computed.  If the separate standard deviations are provided, then also provide the sample sizes, either as a single value of n or as two separate sample sizes, n1 and n2.  If separate sample sizes n1 and n2 are entered, their harmonic mean serves as the value of n.

For power analysis of the two-sample t-test, the null hypothesis is a zero population mean difference.  For a one-sample test, the null hypothesis is specified, and it is this non-null specification of mu0 that triggers the one-sample analysis. Only non-directional or two-tailed tests are analyzed.

The  effect size that achieves a power of 0.8 is displayed. If a minimal meaningful difference, mmd, is provided, then the  associated power is also displayed, as well as the needed sample size to achieve a power of 0.8.
}

\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{color.plot}}, \code{\link{plot}}, \code{\link{power.t.test}}.
}
\examples{
# default power curve and colors
powercurve.t.test(n=20, s=5)

# default power curve and colors
# plus optional smallest meaningful effect to enhance the analysis
powercurve.t.test(n=20, s=5, mmd=2)

# power curve from both group standard deviations and sample sizes
# also provide the minimum standardized mean difference of 
#   practical importance to obtain corresponding power
powercurve.t.test(n1=15, n2=25, s1=4, s2=6, msmd=.5)
  
# power curve from both group standard deviations but common sample size
# color and display options from plot and color.plot functions
powercurve.t.test(n=20, s1=4, s2=6, lwd=2, col.line="darkred", 
  col.bg="moccasin", col.grid="lightsteelblue", mmd=2, mdp=.6)
  
# power curve for one sample t-test, triggered by non-null mu0
powercurve.t.test(n=20, s=5, mu0=30, mmd=2)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ power }
\keyword{ t.test }
