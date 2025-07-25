% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/faasr_client_api_slurm.R
\name{faasr_register_workflow_slurm}
\alias{faasr_register_workflow_slurm}
\title{faasr_register_workflow_slurm}
\usage{
faasr_register_workflow_slurm(faasr, cred)
}
\arguments{
\item{faasr}{a list form of the JSON file}

\item{cred}{a list form of the credentials}
}
\description{
Register workflow for SLURM cluster.
Parse faasr to get the server list and actions.
Verify connectivity to SLURM REST API endpoints.
}
\keyword{internal}
