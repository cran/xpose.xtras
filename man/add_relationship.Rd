% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xpose_set.R
\name{add_relationship}
\alias{add_relationship}
\alias{remove_relationship}
\title{Add relationship(s) to an xpose_set}
\usage{
add_relationship(xpdb_s, ..., .warn = TRUE, .remove = FALSE)

remove_relationship(xpdb_s, ...)
}
\arguments{
\item{xpdb_s}{<\code{\link{xpose_set}}> An xpose_set object}

\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}> One or more formulas that define relationships between models. One list of formulas can also be used, but a warning is generated.}

\item{.warn}{<\code{\link{logical}}> Should warnings be generated for non-formula inputs? (default: \code{TRUE})}

\item{.remove}{<\code{\link{logical}}> Should listed relationships be removed? (default: \code{FALSE})}
}
\value{
An \code{xpose_set} object with relationships added
}
\description{
Add relationship(s) to an xpose_set
}
\examples{

xpdb_set \%>\%
  add_relationship(mod1~fix2) # ouroboros

xpdb_set \%>\%
  remove_relationship(fix1~mod2) # split down the middle

}
