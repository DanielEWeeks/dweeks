% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/safer_joins.R
\name{left_join_check}
\alias{left_join_check}
\title{left_join augmented with checks}
\usage{
left_join_check(x, y, by = NULL, copy = FALSE, suffix = c(".x",
  ".y"), ...)
}
\arguments{
\item{x}{table to left join into.}

\item{y}{table to be joined into x.}

\item{by}{a character vector of variables to join by.}

\item{copy}{If x and y are not from the same data source, and copy is TRUE, then y will be copied into the same src as x.}

\item{suffix}{If there are non-joined duplicate variables in x and y, these suffixes will be added to the output to disambiguate them. Should be a character vector of length 2.}
}
\description{
This augments \code{left_join} from the \code{dplyr} package with important quality control checks.
When \code{by} is not NULL, then it checks for duplicated IDs (e.g., join variables).
When \code{by} is NULL, if there is only one column in common between \code{x} and \code{y}, then it checks for duplicated IDs using that single column as the join variable.
Also checks if the resulting join has more records than the input table \code{x}.
}
\examples{
x <- data.frame(ID=c(1,2,3),ID1=c(1,2,3),x=c(1,1,1))
y <- data.frame(ID=c(1,2,2),ID2=c(1,2,2),y=c(2,2,2))
x
y
left_join_check(x,y)
left_join_check(x,y,by="ID")
left_join_check(x,y,by = c("ID" = "ID"))
left_join_check(x,y,by = c("ID1" = "ID2"))
}
