% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covariates.R
\name{eta_vs_contcov}
\alias{eta_vs_contcov}
\title{Eta continuous covariate plots (typical)}
\usage{
eta_vs_contcov(
  xpdb,
  mapping = NULL,
  etavar = NULL,
  drop_fixed = TRUE,
  linsm = FALSE,
  type = "ps",
  title = "Eta versus continuous covariates | @run",
  subtitle = "Based on @nind individuals, Eta shrink: @etashk",
  caption = "@dir",
  tag = NULL,
  log = NULL,
  guide = TRUE,
  facets,
  .problem,
  quiet,
  ...
)
}
\arguments{
\item{xpdb}{<\verb{xp_xtras> or <}xpose_data`> object}

\item{mapping}{\code{ggplot2} style mapping}

\item{etavar}{\code{tidyselect} for \code{eta} variables}

\item{drop_fixed}{As in \code{xpose}}

\item{linsm}{If \code{type} contains "s" should the smooth method by \code{lm}?}

\item{type}{Passed to \code{xplot_scatter}}

\item{title}{Plot title}

\item{subtitle}{Plot subtitle}

\item{caption}{Plot caption}

\item{tag}{Plot tag}

\item{log}{Log scale covariate value?}

\item{guide}{Add guide line?}

\item{facets}{Additional facets}

\item{.problem}{Problem number}

\item{quiet}{Silence output}

\item{...}{Any additional aesthetics.}
}
\value{
The desired plot
}
\description{
Eta continuous covariate plots (typical)
}
\examples{
\donttest{

eta_vs_contcov(xpdb_x)

# Labels and units are also supported
xpdb_x \%>\%
  xpose::set_var_labels(AGE="Age", MED1 = "Digoxin") \%>\%
  xpose::set_var_units(AGE="yrs") \%>\%
  set_var_levels(SEX=lvl_sex(), MED1 = lvl_bin()) \%>\%
  eta_vs_contcov()
}
}
