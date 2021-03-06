% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replace_umlaute.R
\name{replace_umlaute}
\alias{replace_umlaute}
\title{Replaces german umlaute}
\usage{
replace_umlaute(names, and = c("-", " "), by = c("_"))
}
\arguments{
\item{names}{Character vector that contains german umlaute.}

\item{and}{Character vector.If not NULL the vector "and"
(default: c("-"," ")) will be replaced by the element "by"}

\item{by}{Character element. Set to NULL, if no replacement is desired.}
}
\value{
Returns a character vector with replaced german umlaute.
}
\description{
This function replaces german umlaute in an appropriate way to avoid
issues with special characters. In addition, user defined elements
can be replaced by an desired element, too.
}
\author{
Dennis Freuer
}
