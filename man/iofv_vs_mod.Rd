% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xset_plots.R
\name{iofv_vs_mod}
\alias{iofv_vs_mod}
\title{Objective function changes across models}
\usage{
iofv_vs_mod(
  xpdb_s,
  ...,
  .lineage = FALSE,
  auto_backfill = FALSE,
  mapping = NULL,
  orientation = "x",
  type = "bjc",
  title = "Individual OFVs across models",
  subtitle = "Based on @nind individuals, Initial OFV: @ofv",
  caption = "Initial @dir",
  tag = NULL,
  axis.text = "@run",
  facets,
  .problem,
  quiet
)
}
\arguments{
\item{xpdb_s}{<\code{xpose_set}> object}

\item{...}{<\code{tidyselect}> of models in set. If empty, all models are
used in order of their position in the set. May also use a formula,
which will just be processed with \code{all.vars()}.}

\item{.lineage}{<\code{logical}> where if \code{TRUE}, \code{...} is processed}

\item{auto_backfill}{<\code{logical}> If \code{TRUE}, apply <\code{\link[=backfill_iofv]{backfill_iofv()}}>
automatically. \code{FALSE} by default to encourage data control as a
separate process to plotting control.}

\item{mapping}{\code{ggplot2} style mapping}

\item{orientation}{Defaults to \code{x}}

\item{type}{Passed to <\code{\link{xplot_boxplot}}>}

\item{title}{Plot title}

\item{subtitle}{Plot subtitle}

\item{caption}{Plot caption}

\item{tag}{Plot tag}

\item{axis.text}{What to label the model. This is parsed on a per-model
basis.}

\item{facets}{Additional facets}

\item{.problem}{Problem number}

\item{quiet}{Silence output}
}
\value{
The desired plot
}
\description{
Another visualization of how individual objective functions change over
the course of model development.
}
\examples{
\donttest{

pheno_set \%>\%
  focus_qapply(backfill_iofv) \%>\%
  iofv_vs_mod()

pheno_set \%>\%
  focus_qapply(backfill_iofv) \%>\%
  iofv_vs_mod(run3,run11,run14,run15)

pheno_set \%>\%
  focus_qapply(backfill_iofv) \%>\%
  iofv_vs_mod(.lineage = TRUE)

}
}
