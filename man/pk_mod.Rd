% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module_PK.R
\name{pk_UI}
\alias{pk_UI}
\alias{pk_mod}
\title{PK Submodule}
\usage{
pk_UI(id)

pk_mod(input, output, session, uid, ADPC)
}
\arguments{
\item{id}{A \code{character}. The namespace for the module.}

\item{uid}{A \code{reactive} \code{character}. A unique subject identifier.}

\item{ADPC}{A \code{reactive} \code{data.frame} of ADaM formatted Pharmacokinetics data.}

\item{innput, output, session}{Shiny server arguments.}
}
\description{
Functions to enable the pharmacokinetics (PK) submodule of the patient profile.
}
