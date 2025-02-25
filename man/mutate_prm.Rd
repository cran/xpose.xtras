% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xtra_pars.R
\name{mutate_prm}
\alias{mutate_prm}
\title{Transform parameter values in place}
\usage{
mutate_prm(
  xpdb,
  ...,
  .autose = TRUE,
  .problem = NULL,
  .subprob = NULL,
  .method = NULL,
  .sesim = 1e+05,
  quiet
)
}
\arguments{
\item{xpdb}{<\code{xp_xtras}> object}

\item{...}{... <\code{\link[rlang:dyn-dots]{dynamic-dots}}> One or more formulae that
define transformations to parameters. RHS of formulas can be function or a
value. That value can be a function call like in \code{mutate()} (\code{the1~exp(the1)}).}

\item{.autose}{<\code{logical}> If a function is used for the transform then simulation
is used to transform the current SE to a new SE. Precision of this transformation
is dependent on \code{.sesim}. If parameter values are not assigned with a function,
this option will simply scale SE to maintain the same RSE. See Details.}

\item{.problem}{<\code{numeric}> Problem number to apply this relationship.}

\item{.subprob}{<\code{numeric}> Problem number to apply this relationship.}

\item{.method}{<\code{numeric}> Problem number to apply this relationship.}

\item{.sesim}{<\code{numeric}> Length of simulated \code{rnorm} vector for \code{.autose}.}

\item{quiet}{Silence extra output.}
}
\value{
An updated \code{xp_xtras} object with mutated parameters
}
\description{
Apply transformations to fitted parameter values.

As fitted, sometimes parameter values are not as easy to communicate, but
to transform them outside of the \code{xpose} ecosystem limits some available
features. To have the best experience, this function can update the
parameter values that are used by \code{xpose} \code{get_prm} functions. At this
time these transformations are not applied to \code{param} vars (\code{\link{list_vars}}), but that can
already be done with the \code{mutate} method.

This only works for theta parameters.

All valid mutations are applied sequentially, so a double call to \code{the2~the2^3}
will result in effectively \code{the2~the2^9}, for example.

RSE values are calculated at runtime within \code{get_prm}, so they are not updated (or
updatable) with this function.
}
\details{
\subsection{Important points about covariance and correlation}{

Covariance and correlation parameters are adjusted when standard error (SE)
values are changed directly or with \code{.autose}. When a transformation is applied
as a function for the fixed effect parameter (eg, \code{~plogis}), the resulting SE may have
an unexpected scale; this is because it is now reporting the standard deviation
of a transformed and potentially non-normal distribution. If the parameter were fit
in the transformed scale (constrained to any appropriate bounds), it would likely have a
different SE given that most covariance estimation methods (excluding non-parametric and
resampling-based) will treat the constrained parameter as continuous and unconstrained.

The updates to variance-covariance values (and the correlation values, though that is mostly
invariant) are applied to the entire matrices. When piped directly into
\code{get_prm}, only the SE estimate is shown, but <\code{\link[xpose:get_file]{get_file}}> can be used
to see the complete updated variance-covariance values. This could be useful if those
matrices are being used to define priors for a Bayesian model fitting, as the re-scaling
of off-diagonal elements is handled automatically.

A function to transform parameters will result in a more accurate \code{autose} result. If a call
(\code{the1~exp(the)}) or a value (\code{the1~2}) are used, the standard error will be simply scaled.
}
}
\examples{

vismo_pomod \%>\%
  # Function
  mutate_prm(THETA11~exp) \%>\%
  # Value (se will not be scaled); plogis = inverse logit
  mutate_prm(THETA12~plogis(THETA12)) \%>\%
  get_prm()


}
