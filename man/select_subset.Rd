% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xpose_set.R
\name{select_subset}
\alias{select_subset}
\title{Convenience wrapper for tidyselect}
\usage{
select_subset(xpdb_s, ...)
}
\arguments{
\item{xpdb_s}{<\code{\link{xpose_set}}> An xpose_set object}

\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}> One or more tidyselect selectors}
}
\value{
<\code{\link{numeric}}> vector of indices for selected xpdb objects
}
\description{
This is intended for use as an internal function to select a subset of xpdb objects from an xpose_set.

It is a lower level version of <\code{\link{select.xpose_set}}>
}
\examples{


select_subset(mod2, xpdb_s=xpdb_set)

select_subset(dplyr::starts_with("fix"), xpdb_s=xpdb_set)


}
\keyword{internal}
