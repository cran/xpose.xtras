% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xset_shark.R
\name{shark_colors}
\alias{shark_colors}
\title{Change colors of shark plots}
\usage{
shark_colors(
  xpdb,
  upcolor = xp_xtra_theme(base_on = xpdb$xp_theme)$sharkup_color,
  dncolor = xp_xtra_theme(base_on = xpdb$xp_theme)$sharkdn_color
)
}
\arguments{
\item{xpdb}{<\code{xpose_data}> object}

\item{upcolor}{Color for increasing dOFV}

\item{dncolor}{Color for decreasing dOFV}
}
\value{
<\code{xpose_data}> object
}
\description{
This changes the point and text color
in the \code{xp_theme} of an \code{xpose_data} object.
}
\examples{
\donttest{

# Where this would fit in a particular workflow
xpose_set(pheno_base, pheno_final) \%>\%
  # forward functions affecting xpdb objects
  focus_xpdb(everything()) \%>\%
  # Add iOFVs
  focus_function(backfill_iofv) \%>\%
  # Change color of all xpdb xp_themes (though only the first one needs to change)
  focus_function(
  function(x) shark_colors(
      x,
      upcolor = "purple",
      dncolor = "green"
    )) \%>\%
  # See new plot
  shark_plot()
}
}
\seealso{
\code{\link[=shark_plot]{shark_plot()}}
}
