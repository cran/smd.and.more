\name{ral}
\alias{ral}

\title{Read, Attach and List Contents of a csv File}

\description{
Reads the contents of the specified csv data file into an R dataframe called \code{mydata}.  
Then attaches and lists the first
six rows of data as well as the variable names.  The purpose of \code{ral} is to 
save some keyboard entry, speeding up the process of reading a csv data file and reducing entry mistakes.
}

\usage{
ral(ref=NULL, ...)
}


\arguments{
  \item{ref}{File reference, either null to specify the default of \code{file.choose()} to browse
       for the function, or a full path name or Internet URL, included in quotes.  A URL begins 
       with \code{http://}.}
  \item{...}{Other parameter values consistent with the usual \code{read.csv} function, such as 
       row.names and header.}
}


\details{
Invokes one of the two R statements. When called with no arguments, \code{ral()}, first invokes the following.

    \code{mydata <- read.csv(file.choose())}
    
Or, when called with, \code{ral("file_reference")}, invokes the following.

    \code{mydata <- read.csv("file_reference")}
    
Then, the subsequent R instructions \code{attach(mydata)} and \code{head(mydata)} are also invoked. 

One way to create a csv data file is with MS Excel or other worksheet application.  Place the
variable names in the first row of the worksheet.  Each subsequent row is data for a specific
observation, such as a person or a company.  All numeric data should be the General format, so that
the only non-digit character for a numeric data value is a decimal point.  The General format
removes all dollar signs and commas, for example. Then, for the File option, do a Save As and choose
the csv format.
}

\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\examples{
# to browse for a csv data file on the computer system, invoke ral with 
# the ref argument empty, which, in turn, invokes read.csv(file.choose())
# then attach and list variable names and some values
# ral()

# same as above, but include read.csv option to indicate no variable names 
# in first row and then provide the names
# ral(header=FALSE, col.names=c("X", "Y"))

# read a csv data file from the web
# then attach and list variable names and some values
ral("http://web.pdx.edu/~gerbing/data/twogroup.csv")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ read }
