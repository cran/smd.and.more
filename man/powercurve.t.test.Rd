\name{powercurve.t.test}
\alias{powercurve.t.test}

\title{Compute a Power Curve with Colors}

\description{
From the sample size and either the within-cell or pooled standard deviation, or the two
separate group standard deviations, automatically calibrate and calculate a power curve for the
independent-groups t-test or one-sample t-test, as well as ancillary statistics.  
Use the \code{color.plot} function in this package to automatically
display the power curve plot with colors.
}

\usage{
powercurve.t.test(n=NULL, s=NULL, n1=NULL, n2=NULL, s1=NULL, s2=NULL, 
    sme=NULL, mu0=NULL, \dots)
}


\arguments{
  \item{n}{Sample size for each of the two groups.}
  \item{s}{Within-group, or pooled, standard deviation.}
  \item{n1}{Sample size for Group 1.}
  \item{n2}{Sample size for Group 2.}
  \item{s1}{Sample standard deviation for Group 1.}
  \item{s2}{Sample standard deviation for Group 2.}
  \item{sme}{Smallest meaningful effect, for which, if the value is provided, the power 
       is specifically displayed.}
  \item{mu0}{Hypothesized mean, triggers a one-sample analysis.}
  \item{\dots}{Other parameter values consistent with the usual \code{plot} function
       and the provided \code{color.plot} function, such as 
       lwd and cex.lab for \code{plot} and col.line and col.bg for \code{color.plot}.}
}


\details{
This function relies upon the
standard \code{\link{power.t.test}} function to calibrate and then calculate the power curve. 
The \code{\link{color.plot}} function provided in this package, which in turn relies upon the 
standard \code{plot} function, calculates the display. All of the parameters
in \code{\link{color.plot}} for controlling the different colors and other aspects of the display are also
available.  Also, many of the more basic parameters in the usual
\code{plot} function are available. 

Invoke the function with the
either the within-group (pooled) standard deviation, s, or the two separate group
standard deviations, s1 and s2, from which s is computed.  If the separate standard deviations are
provided, then also provide the sample sizes, either as a single value of n or as
two separate sample sizes, n1 and n2.  If separate sample sizes n1 and n2 are entered, their
harmonic mean serves as the value of n.

For power analysis of the two-sample t-test, the null hypothesis is a zero population mean 
difference.  For a one-sample test, the null hypothesis is specified. Only two-tailed 
tests are analyzed. 

The exact effect size that achieves a power of 0.8 is displayed. If a smallest meaningful effect size,
sme, is provided, then the exact associated power is also displayed, as well as the needed sample size
to achieve a power of 0.8.
}

\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\examples{
# default power curve and colors
# plus optional smallest meaningful effect
powercurve.t.test(n=20, s=5, sme=2)

# power curve from both group standard deviations and sample sizes
# also provide the smallest meaningful effect to obtain corresponding power
powercurve.t.test(n1=15, n2=25, s1=4, s2=6, sme=2)
  
# power curve from both group standard deviations but common sample size
# color and display options from plot and color.plot functions
powercurve.t.test(n=20, s1=4, s2=6, lwd=2, col.line="darkred", 
  col.bg="moccasin", col.grid="lightsteelblue")
  
# power curve for one sample, triggered by non-null mu0
powercurve.t.test(n=20, s=5, sme=2, mu0=30)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ power }
\keyword{ t.test }
