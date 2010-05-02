\name{help.me}
\alias{help.me}

\title{Help System for Statistics that Suggests Specified Functions}

\description{
R works by entering function names and arguments.  R provides extensive help for each available function based on a function's name, but these names are not apparent to someone who has not memorized them or has access to their definitions.  To alleviate this problem, this help system suggests and briefly explains various function calls regarding a requested type of basic statistical analysis.
}

\usage{
help.me(ref=NULL)
}


\arguments{
  \item{ref}{Message reference, either null to specify a list of available topics or a specific argument to reference a specific help message.}
}


\details{
\code{help.me()} displays a list of help topics, listed below.

\code{help.me("reference")} displays a message regarding a specific function or related functions for the specified topic.

---

\code{create.data.file}: Create a csv data file from a worksheet application.

\code{libraries}: Run charts, scatterplots, function graphs.

\code{read.data}: Read an external data file in csv format.

---

\code{stats}: Summary statistics.

\code{histogram}: Histogram of a numeric variable.

\code{bar.chart}: Bar chart of a categorical variable.

\code{one.group}: Analyzing a single sample of data.

\code{compare.groups}: Analysis of group mean differences and more.

\code{power}: Power analysis for the t-test.

\code{cor}: Correlation analysis.

\code{regression}: Regression analysis.

\code{plot.values}: Run charts, scatterplots, function graphs.
}

\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{help}}.
}

\examples{
# list the information needed to access a specific topic
help.me()

# specific help message regarding summary statistics
help.me("stats")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ help }
