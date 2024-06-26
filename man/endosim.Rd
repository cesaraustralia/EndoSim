% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/endosim.R
\name{endosim}
\alias{endosim}
\title{EndoSim}
\usage{
endosim(
  Pest,
  Endosymbiont,
  Crop,
  Parasitoid = NULL,
  init,
  conds,
  plot = TRUE,
  progress = TRUE,
  vert_trans = FALSE,
  hori_trans = FALSE,
  imi = FALSE,
  emi = FALSE,
  para = FALSE
)
}
\arguments{
\item{Pest}{object of class \code{pest} defining the pest}

\item{Endosymbiont}{object of class \code{endosym} defining the endosymbiont}

\item{Crop}{object of class \code{crop} defining the crop}

\item{Parasitoid}{object of class \code{parasitoid} defining the parasitoid}

\item{init}{object of class \code{initial} defining the starting conditions for the simulation}

\item{conds}{object of class \code{sim_conds} defining the simulation conditions}

\item{plot}{if \code{TRUE} (default) generate plots}

\item{progress}{if \code{TRUE} (default) print progress bar}

\item{vert_trans}{set to \code{TRUE} to turn on vertical transmission}

\item{hori_trans}{set to \code{TRUE} to turn on horizontal transmission}

\item{imi}{set to \code{TRUE} to turn on immigration into paddock}

\item{emi}{set to \code{TRUE} to turn on emigration from paddock}

\item{para}{set to \code{TRUE} to turn on parasitoids}
}
\value{
object of class \code{endosim_mod}; see \linkS4class{endosim_mod}.
}
\description{
Run simulation using the endosymbiont model
}
\details{
Constructs the endosymbiont model using the provided \linkS4class{pest}, \linkS4class{endosym}, \linkS4class{crop},\linkS4class{parasitoid}, \linkS4class{initial}, and \linkS4class{sim_conds} objects.

The \code{Pest} species feeds on the provided \code{Crop}.

Vertical and horizontal transmission of endosymbiont are defined using functions in the \code{Endosymbiont}.

Parasitism rates are defined usined functions in the \code{Parasitoid}.
}
