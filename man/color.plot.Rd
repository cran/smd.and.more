\name{color.plot}
\alias{color.plot}

\title{Scatter-Plot or Line-Plot with Color}

\description{
Plots individual points, such as for a scatterplot, or plots connected line segments,
such as for the plot of the values of a function.  The primary enhancement over the usual \code{plot}
function is the enhancement with color.  The color of the line segments and/or the points, background, 
area under the plotted line segments, gridlines, and border can each be explicitly specified.  
Default colors are provided.  
}

\usage{
color.plot(x, y, type="p", col.line="darkblue", col.point="blue", 
   col.area="transparent", col.border="transparent", col.grid="grey90", 
   col.bg="ivory", col.box="black", xy.ticks=TRUE, \dots)
}


\arguments{
  \item{x}{Coordinates of points in the plot on the horizontal axis.}
  \item{y}{Coordinates of points in the plot on the vertical axis.}
  \item{type}{1-character string that indicates the type of plot, either "p" for points, 
       "l" for line, or "b" for both.}
  \item{col.line}{Color of any plotted line segments.}
  \item{col.point}{Color of any plotted points.}
  \item{col.area}{Color of area under the plotted line segments, default is "transparent".}
  \item{col.border}{Border of area under the plotted line segments, usually set to the same
        color as the line segments, with a default of "transparent".}
  \item{col.grid}{Color of the grid lines.}
  \item{col.bg}{Color of the plot background.}
  \item{col.box}{Color of border around the plot background, the box, that encloses the plot.}
  \item{xy.ticks}{Flag that indicates if tick marks and associated values on the two axes 
       should be printed.}
  \item{\dots}{Other parameter values consistent with the usual \code{plot} function, such as 
       xlab, ylab, xlim, ylim, lty, lwd, pch and cex.lab.}
}


\details{
Results are based on the default \code{plot} function for two variables, 
except with the added color options.
}

\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\examples{
# default (scatter) plot, then with some options
x <- rnorm(25)
y <- rnorm(25)
color.plot(x, y)
color.plot(x, y, col.point="darkred", pch=19)

# plot function curve according to specified colors
x <- seq(10,500,by=1) 
y <- 18/sqrt(x)
color.plot(x, y, type="l", col.line="blue", col.bg="snow", 
  col.area="slategray2", col.border="blue", col.grid="lightsalmon")
  
# run chart
index <- seq(0,24,1)
y <- rnorm(25)
color.plot(index, y, type="b", col.point="sienna3", col.bg="mintcream", 
  xlab="index", ylim=c(-3,3))
abline(h=median(y), lty="dashed", col="gray60")

# modern art
n <- sample(2:50, size=1)
x <- rnorm(n)
y <- rnorm(n)
clr <- colors()
color1 <- clr[sample(1:657, size=1)]
color2 <- clr[sample(1:657, size=1)]
color.plot(x, y, type="l", lty="dashed", lwd=3, col.area=color1, 
   col.line=color2, xlab="", ylab="", xy.ticks=FALSE, main="Modern Art", 
   cex.main=2, col.main="lightsteelblue")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
