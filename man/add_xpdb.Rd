% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xpose_set.R
\name{add_xpdb}
\alias{add_xpdb}
\title{Add one or more \code{xpdb} objects to an \code{xpose_set}}
\usage{
add_xpdb(xpdb_s, ..., .relationships = NULL)
}
\arguments{
\item{xpdb_s}{<\code{\link{xpose_set}}> An xpose_set object}

\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}> One or more \code{xpdb} objects to add to the set}

\item{.relationships}{<\code{\link{list}}> A list of relationships between the \code{xpdb} objects.}
}
\value{
An \code{xpose_set} object with the new \code{xpdb} objects added
}
\description{
Add one or more \code{xpdb} objects to an \code{xpose_set}
}
\examples{
data("xpdb_ex_pk", package = "xpose")

add_xpdb(xpdb_set, ttt=xpdb_ex_pk)

}
