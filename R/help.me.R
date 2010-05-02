help.me <- 
function(ref=NULL) {

more <-
function(fname) {
cat("\n---------------------------------------------------------------------")
cat("\nList of help topics, enter: help.me()")
cat("\nHelp on a function, enter ? in front of name, eg., ?", fname, sep="")
cat("\n---------------------------------------------------------------------")
cat("\n\n")
}

if (is.null(ref)) {
cat("---------------------------------------------------------------------
Enter a help.me statement for help on the indicated topic area.
---------------------------------------------------------------------
help.me(\"create.data.file\"): Create csv data file from worksheet application
help.me(\"libraries\"): Download and manage libraries of functions
help.me(\"read.data\"): Read an external data file in csv format
---------------------------------------------------------------------
help.me(\"stats\"): Summary statistics
help.me(\"histogram\"): Histogram of a numeric variable
help.me(\"bar.chart\"): Bar chart of a categorical variable
help.me(\"one.group\"): Analyzing a single sample of data
help.me(\"compare.groups\"): Comparing groups, such as a mean difference
help.me(\"power\"): Power analysis for the t-test
help.me(\"cor\"): Correlation analysis
help.me(\"regression\"): Regression analysis
help.me(\"plot.values\"): Run charts, scatterplots, function graphs
---------------------------------------------------------------------")
}

else if (ref == "read.data") {
cat("---------------------------------------------------------------------")
cat("
<<rad>> Read a csv data file into an R data frame called mydata.

rad() Browse for a csv data file available on the local computer system.

rad(\"file_expression\") Read the specified file. The file can be a path name to a data file available on the local computer system, or to a file on the web.  

Example:  rad(\"http://web.pdx.edu/~gerbing/data/6-13Paint.csv\")

The rad function, which stands for Read, Attach and Display, accomplishes its work by invoking four specific R functions: read.csv, attach, head and tail.

To see how to create a csv data file, enter: help.me(\"create.data.file\")
")
more("rad")
}

else if (ref == "create.data.file") {
cat("---------------------------------------------------------------------")
cat("
R can read csv data files, for \"comma separated values\".  These files are standard text files with commas separating adjacent values in each row.

One way to create a csv data file is with MS Excel or other worksheet application.  Place the variable names in the first row of the worksheet. Each column of the worksheet contains the data for the corresponding variable. Each subsequent row contains the data for a specific observation, such as for a person or a company.

All numeric data should be displayed in the General format, so that the only non-digit character for a numeric data value is a  decimal point.  The General format removes all dollar signs and commas, for example, leaving only the pure number, stripped of any extra characters which R will not properly read as a numeric data value.

To create the csv file from a worksheet, under the File option, do a Save As and choose the csv format.")
more("rad")
}

else if (ref == "libraries") {
cat("---------------------------------------------------------------------")
cat("
<<install.packages>> Download an external library or package.
<<library>> Load an installed library into R for access.
<<update.packages>> Update with a new download of an external library.

To illustrate for the external library smd.and.more.  One time only, with quotes:
  install.packages(\"smd.and.more\")
Then, each time you start the R-application, including after the install, load the library, without using quotes:
  library(smd.and.more)
To see the full manual of the library, do:
  library(help=smd.and.more)
To access new versions of all installed external libraries, do:
  update.packages()
  
All of R works with functions contained in specific libraries.  The distinction is that some of those libraries are included with the default installation of R, and are pre-loaded every time the application is run. Examples are the stat library and the graphic library. To see a list of all installed libraries, 
  library()
")
more("install.packages")
}

else if (ref == "stats") {
cat("---------------------------------------------------------------------
  
Each of these functions applies to the analysis of single variable.  For example, to calculate the mean of variable called Y, enter: mean(Y).

<<length>> sample size
<<mean>> mean
<<sd>> standard deviation
<<median>> median
<<min>> minimum
<<max>> maximum
<<range>> range
<<quantile>> min, 1st quartile, median, 3rd quartile, max
<<scale>> standardize

<<apply>> Calculate a statistic for all columns of a data frame

R provides many summary statistics.  To see the entire list, 
   enter: library(help=\"stats\").
")
more("mean")
}

else if (ref == "histogram") {
cat("---------------------------------------------------------------------
<<hist>> Histogram function.

The generic variable in the examples below is Y. Replace with the actual name of the variable in a specific analysis.

Default histogram:
hist(Y)

Specify a title, labels for the x and y axes, and a color:
hist(Y, main=\"My Title\", xlab=\"Y (mm)\", ylab=\"Counts\", col=\"seagreen3\")

Manually specify bins, starting at 60, going to 100, with a bin width of 10:
  hist(Y, breaks=seq(60,100,10), col=\"wheat\")

When specifying bins, best to have the stopping point at the end of the last bin, such as above with 60, 70, 80, 90, 100.  For example, 
  hist(Y, breaks=seq(60,100,9))
would not accomplish this, yielding bins of width 9 that start at 60 and end at 96.  Values of Y between 96 and 100 would not be included in the histogram, which would generate an error message.
")
more("hist")
}

else if (ref == "bar.chart") {
cat("---------------------------------------------------------------------")
cat("
<<barplot>> Produce a bar chart.
<<pareto.chart>> Produce a Pareto chart.
<<table>> Count the values of one or more categorical variables.

The pareto.chart function is part of the external library called gcc.  To view an explanation of dealing with these libraries, enter help.me(\"libraries\").  

The generic variable in the examples below is a categorical variable Y, called a factor. Replace with the actual name of the variable in a specific analysis. 

The key is to first use the table function to provide the counts of each value or level of Y.  Then construct the bar chart or Pareto chart from the table.

Use table function to get counts:
Ycount <- table(Y) 

Default bar chart plus a color:
barplot(Ycount, count=\"plum\")

Default Pareto chart (after downloading and loading library gcc):
pareto.chart(Ycount)
")
more("barplot")
}

else if (ref == "one.group") {
cat("---------------------------------------------------------------------
These inference tests analyze a mean from a numeric variable or a proportion of a value of a categorical variable. These tests provide a hypothesis test and a confidence interval.

<<t.test>> Inference for a mean.
<<binom.test>> Inference for a proportion from exact binomial probability.
<<prop.test>> Inference for a proportion from approximate normal probability.

The prop.test function can be specified with or without the Yate's correction factor.

Example: t.test(Y, mu=100), for a variable named Y and a null hypothesis of 100.
")
more("t.test")
}

else if (ref == "compare.groups") {
cat("---------------------------------------------------------------------
When responses to a variable are organized into two or more groups, compare the responses across the different groups, such as their respective means.  For example, compare average salary for mean and for women.

First consider functions for comparing two or more numeric response variables, including the respective means.  For this, use a t-test.

<<t.test>> Compare two group means with a t-test using the standard R function.
<<smd.t.test>> Compare two group means with an enhanced version of t.test.

To compare the means of two or more groups, use analysis of variance.

<<aov>> Analysis of variance.

Also can compare the proportions of two or more groups, that is for a categorical response variable, or factor.

<<chisq.test>> Begin with a table of joint frequencies, a pivot table.
")
more("smd.t.test")
}

else if (ref == "power") {
cat("---------------------------------------------------------------------")
cat("
<<power.t.test>> Standard R function for the power analysis of the t-test.
<<powercurve.t.test>> Enhanced power function, also provides power curve.

The powercurve.t.test function uses the standard R power.t.test function to calculate a range of power values and automatically provide a power curve. To accomplish this analysis otherwise requires setting up the range of alternative mean or mean difference values, usually by trial and error, invoking the power.t.test function, saving the results, and then invoking the plot function, including the labeling of each axis.  The enhanced powercurve.t.test function does all of this automatically, and also plots the power curve in color.  
")
more("powercurve.t.test")
}

else if (ref == "cor") {
cat("---------------------------------------------------------------------")
cat("
<<cor>> Correlation coefficient(s) between two or more variables.
<<cor.test>> Correlation coefficient with statistical inference.
<<pairs> Graphics, a scatterplot matrix.

To compute a correlation, the cor function can be applied to a single pair of values, such as cor(X,Y), or to all numeric variables in the data frame, such as cor(mydata) for a data frame named mydata.  The graphic function, pairs, applies to the data frame, or a subset of variables from the data frame, producing two scatterplots for each pair of specified variables.  For variables X and Y, the two scatterplots are X vs Y and Y vs X.

Example of cor and pairs applied to a subset of variables in the data frame called mydata:
cor(subset(mydata, select=c(Y,X1:X3)))
pairs(subset(mydata, select=c(Y,X1:X3)))

The cor.test function applies only to a single pair of variables, and provides output similar to the t.test and related functions.
")
more("cor")
}

else if (ref == "regression") {
cat("---------------------------------------------------------------------")
cat("
<<lm>> Linear Model function in which the regression model is defined.
<<summary>> Summarizes the analysis.
<<confint>> Provides confidence intervals of the model coefficients.
<<anova>> Provides an ANOVA table of the analysis.

The lm function defines the linear model, but reports no output other than the estimated coefficients of the model. Typically, first the output of this function is stored and then analyzed with subsequent functions.

Sample analysis, with the response variable called Y and the predictor variables X1 and X2:
model <- lm(Y ~ X1 + X2)
summary(model)
confint(model)
anova(model)

Many more functions for analyzing the output of the lm function exist, including resid, fitted, rstdnt, dffit, cook and predict. 
")
more("lm")
}

else if (ref == "plot.values") {
cat("---------------------------------------------------------------------")
cat("
<<plot>> Plot values of one or two variables.
<<color.plot>> Enhances some of the capabilities of the plot function.
<<curve>> Specifically plot the values of a function.

The plot function is very general, capable of producing a wide range of plots.  The color.plot function access the plot function while providing easier access to color enhancement.  Either function plots run charts, scatter plots and the values of a function.  The sole purpose of the curve function is to plot the values of a function.

These functions can access a wide range of graphics parameters, such as the size of the margins, the annotations, the font, etc.  

<<par>> Describes many of the available graphics parameters.
<<title>> Describes some of the graphics parameters for annotating a plot.
")
more("color.plot")
}

else {
cat("
Value ", ref," for help.me not recognized.")
more()
}

}
