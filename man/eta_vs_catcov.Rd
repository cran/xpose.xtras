% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covariates.R
\name{eta_vs_catcov}
\alias{eta_vs_catcov}
\title{Eta categorical covariate plots (typical)}
\usage{
eta_vs_catcov(
  xpdb,
  mapping = NULL,
  etavar = NULL,
  drop_fixed = TRUE,
  orientation = "x",
  show_n = check_xpdb_x(xpdb, .warn = FALSE),
  type = "bol",
  title = "Eta versus categorical covariates | @run",
  subtitle = "Based on @nind individuals, Eta shrink: @etashk",
  caption = "@dir",
  tag = NULL,
  facets,
  .problem,
  quiet,
  ...
)
}
\arguments{
\item{xpdb}{<\verb{xp_xtras> or  <}xpose_data`> object}

\item{mapping}{\code{ggplot2} style mapping}

\item{etavar}{\code{tidyselect} for \code{eta} variables}

\item{drop_fixed}{As in \code{xpose}}

\item{orientation}{Passed to \code{xplot_boxplot}}

\item{show_n}{Add "N=" to plot}

\item{type}{Passed to \code{xplot_boxplot}}

\item{title}{Plot title}

\item{subtitle}{Plot subtitle}

\item{caption}{Plot caption}

\item{tag}{Plot tag}

\item{facets}{Additional facets}

\item{.problem}{Problem number}

\item{quiet}{Silence output}

\item{...}{Any additional aesthetics.}
}
\value{
The desired plot
}
\description{
Eta categorical covariate plots (typical)
}
\details{
The ability to show number per covariate level is inspired
by the package \code{pmplots}, but is implements here within
the \code{xpose} ecosystem for consistency.
}
\examples{
\donttest{

eta_vs_catcov(xpdb_x)

# Labels and units are also supported
xpdb_x \%>\%
  xpose::set_var_labels(AGE="Age", MED1 = "Digoxin") \%>\%
  xpose::set_var_units(AGE="yrs") \%>\%
  set_var_levels(SEX=lvl_sex(), MED1 = lvl_bin()) \%>\%
  eta_vs_catcov()
}
}
