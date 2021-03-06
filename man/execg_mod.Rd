% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module_EXECG.R
\name{execg_UI}
\alias{execg_UI}
\alias{execg_mod}
\title{EXECG Submodule}
\usage{
execg_UI(id)

execg_mod(input, output, session, uid, ADSL, ADEX, ADPC, ADEG)
}
\arguments{
\item{id}{A \code{character}. The namespace for the module.}

\item{uid}{A \code{reactive} \code{character}. A unique subject identifier.}

\item{ADSL}{A \code{reactive} \code{data.frame} of ADaM formatted Subject Level data.
This cannot be \code{NULL}.}

\item{ADEX}{A \code{reactive} \code{data.frame} of ADaM formatted EXposure data.}

\item{ADPC}{A \code{reactive} \code{data.frame} of ADaM formatted Pharmacokinetics data.}

\item{ADEG}{A \code{reactive} \code{data.frame} of ADaM formatted Electrocardiogram data.}

\item{innput, output, session}{Shiny server arguments.}
}
\description{
Functions to enable the exposure (EX) & electrocardiogram (ECG) submodule of the
patient profile.
}
