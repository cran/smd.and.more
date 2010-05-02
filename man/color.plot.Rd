\name{color.plot}
\alias{color.plot}

\title{Scatter-Plot or Line-Plot with Color}

\description{
Plots individual points, such as for a scatterplot, or plots connected line segments, such as for the plot of a function or a run chart.  The primary enhancement over the usual \code{\link{plot}} function is the automatic inclusion of color.  The color of the line segments and/or the points, background, area under the plotted line segments, gridlines, and border can each be explicitly specified.  Default colors are provided.  Also provided is an option for a center line, which is particularly applicable to the generation of a run chart, plotting the values of a variable in order of occurrence over time.
}

\usage{
color.plot(x, y=NULL, type="p", col.line="darkblue", 
           col.point="blue", col.area="transparent", 
           col.border="transparent", col.grid="grey90",
           col.bg="ivory", col.box="black", xy.ticks=TRUE,
           xlab=NULL, ylab=NULL, center.line=NULL, \ldots)}


\arguments{
  \item{x}{Coordinates of points in the plot on the horizontal axis.}
  \item{y}{Coordinates of points in the plot on the vertical axis. If not specified,
      then the values of this variable are copied from x, and x is set to an index, from 0 to one less than the number of values.}
  \item{type}{1-character string that indicates the type of plot, either "p" for 
        points, "l" for line, or "b" for both.}
  \item{col.line}{Color of any plotted line segments.}
  \item{col.point}{Color of any plotted points.}
  \item{col.area}{Color of area under the plotted line segments, default is 
        "transparent".}
  \item{col.border}{Border of area under the plotted line segments, usually set to
        the same color as the line segments, with a default of "transparent".}
  \item{col.grid}{Color of the grid lines.}
  \item{col.bg}{Color of the plot background.}
  \item{col.box}{Color of border around the plot background, the box, that encloses 
      the plot.}
  \item{xy.ticks}{Flag that indicates if tick marks and associated values on the 
      axes are to be displayed.}
  \item{xlab}{Label for x-axis. If not specified, then the label becomes the name of
      the corresponding variable. If xy.ticks is FALSE, then no label is displayed. If no y variable is specified, then y becomes x and x becomes an index and is labled Index.}
  \item{ylab}{Label for y-axis. If not specified, then the label becomes the name of the corresponding variable. If xy.ticks is FALSE, then no label displayed.}
  \item{center.line}{Plots a dashed line through the middle of the plot and changes
      the line type to line instead of points.  The two possible values are "mean" and "median".}
  \item{\dots}{Other parameter values for graphics as defined by and then processed 
      by \code{\link{plot}} and \code{\link{par}}, including xlim, ylim, lty, lwd, pch and cex.lab, except those paramater values explicitly set here, which are xlab, ylab and type.}
}


\details{
Results are based on the standard \code{\link{plot}} and related graphic functions, with the additional provided color capabilities and other options.

Commonly used graphical parameters in standard R graphics functions used by and available to \code{color.plot}:

\describe{
\item{pch}{Vector of plotting characters or symbols for the individual points, see \code{\link{points}}.}
\item{lwd}{Line width, see \code{\link{par}}.}
\item{cex}{Numerical vector giving the amount by which plotting characters and symbols should be scaled relative to the default. This works as a multiple of \code{\link{par}}("cex"). NULL and NA are equivalent to 1.0. Note that this does not affect annotation.}
\item{cex.main, col.lab, font.sub, etc}{Settings for main- and sub-title and axis annotation, see \code{\link{title}} and \code{\link{par}}.}
\item{main}{Title of the graph, see \code{\link{title}}.}
\item{xlim}{The limits of the plot on the x-axis, expressed as c(x1,x2), where x1 and x2 are the limits. Note that x1 > x2 is allowed and leads to a reversed axis.}
\item{ylim}{The limits of the plot on the y-axis.}
}

}


\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{plot}}, \code{\link{title}}, \code{\link{par}}.
}

\examples{
# generate two vectors with random normal data
x.var <- rnorm(25)
y.var <- rnorm(25)
# default plot is a scatterplot
color.plot(x.var, y.var)
# scatter plot with dark red, solid circles for points
color.plot(x.var, y.var, col.point="darkred", pch=19)

# plot function curve according to specified colors
x <- seq(10,500,by=1) 
y <- 18/sqrt(x)
color.plot(x, y, type="l", ylab="My Y", , xlab="My X", 
  col.line="blue", col.bg="snow", col.area="slategray2",
  col.border="blue", col.grid="lightsalmon")
  
# run chart
# only provide one variable to color.plot, 
# which generates the sequential values of the x variable
y <- rnorm(25)
# run chart with centerline
color.plot(y, type="l", center.line="median")
# run chart with centerline and other options
color.plot(y, type="b", lwd=2, col.point="sienna3", col.bg="mintcream", 
  ylim=c(-3,3), center.line="median")
  
# modern art
n <- sample(2:30, size=1)
x <- rnorm(n)
y <- rnorm(n)
clr <- colors()
color1 <- clr[sample(1:length(clr), size=1)]
color2 <- clr[sample(1:length(clr), size=1)]
color.plot(x, y, type="l", lty="dashed", lwd=3, col.area=color1, 
   col.line=color2, xy.ticks=FALSE, main="Modern Art", 
   cex.main=2, col.main="lightsteelblue")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ color }
