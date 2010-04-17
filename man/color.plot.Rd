\name{color.plot}
\alias{color.plot}

\title{Line Plot with Color}

\description{
Plots connected line segments, such as when plotting a function, with color.  Default colors are provided.
The foreground, background, area under the curve and the grid can each be explicitly specified.  
}

\usage{
color.plot(x, y, col.fg="darkblue", col.bg="ivory", 
  col.area="transparent", col.grid="grey90")
}


\arguments{
  \item{x}{Coordinates of points in the plot on the horizontal axis.}
  \item{y}{Coordinates of points in the plot on the vertical axis.}
  \item{col.fg}{Foreground color.}
  \item{col.bg}{Background color.}
  \item{col.area}{Color of area under the curve.}
  \item{col.grid}{Color of the grid lines.}
}



\details{
Results are similar to the default \code{plot} function for two variables with the line option, 
\code{type="l"}, except with the 
added color options.
}

\author{David W. Gerbing \email{davidg@sba.pdx.edu}}

\examples{
x <- seq(10,500,by=1) 
y <- 18/sqrt(x)

color.plot(x,y)
color.plot(x, y, col.fg="blue", col.bg="snow", 
  col.area="slategray2", col.grid="lightsalmon")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
