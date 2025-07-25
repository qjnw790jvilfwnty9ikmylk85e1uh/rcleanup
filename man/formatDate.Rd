% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package.r
\name{formatDate}
\alias{formatDate}
\title{Adjust the data type for each column of the \code{date} type.}
\usage{
formatDate(data)
}
\arguments{
\item{data}{The \code{dataframe} that needs to be cleaned.}
}
\value{
The cleaned up \code{dataframe}.
}
\description{
It dynamically and "automatically" changes the datatype of the provided data based on the format for each column.
}
\details{
More information
Changes the type to a \code{date} if the \code{string} has the format .
\itemize{
\item dd/mm/yyyy
\item dd-mm-yyyy
\item dd.mm.yyyy
WARNING: The format needs to be consistent in the column.
}
}
\examples{
```r
 #pretty self explanatory how to use it
 formatDate(data)
```
}
