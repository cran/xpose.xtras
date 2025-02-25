% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xset_plots.R
\name{franken_xpdb}
\alias{franken_xpdb}
\title{Combine several \code{xpose_data} objects into one}
\usage{
franken_xpdb(
  ...,
  .cols = NULL,
  .types = NULL,
  prop_transforms = NULL,
  problem,
  quiet = TRUE
)
}
\arguments{
\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}> \code{xpose_data} or \code{xp_xtra} objects}

\item{.cols}{<\code{tidyselect}> of data columns}

\item{.types}{<\code{character}> of data types in addition to columns}

\item{prop_transforms}{<\code{function}> Extra processing using <\code{\link{franken_prop}}>}

\item{problem}{<\code{numeric}> Problems to look for \code{cols} and \code{types} (defaults all)}

\item{quiet}{Prevents extra output.}
}
\value{
The first \code{xpose_data} object with new data columns
}
\description{
This is an internal function designed to meet the needs
of specific plotting functions
}
\examples{


franken_xpdb(pheno_base, pheno_final, .types="catcov") \%>\%
  xpose::get_data() \%>\%
  select(starts_with("APGR"))

}
\keyword{internal}
