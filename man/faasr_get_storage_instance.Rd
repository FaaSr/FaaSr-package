% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/faasr_client_tools.R
\name{faasr_get_storage_instance}
\alias{faasr_get_storage_instance}
\title{faasr_get_storage_instance}
\usage{
faasr_get_storage_instance(faasr, cred)
}
\arguments{
\item{faasr}{list with parsed and validated Payload}

\item{cred}{credentials}
}
\value{
a list with log server and s3 configuration for paws
}
\description{
Helper function to assist log querying/downloading
from S3 bucket
}
\keyword{internal}
