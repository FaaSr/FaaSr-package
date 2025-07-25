% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/faasr_client_tools.R
\name{faasr_check_token_expiration}
\alias{faasr_check_token_expiration}
\title{faasr_check_token_expiration}
\usage{
faasr_check_token_expiration(token)
}
\arguments{
\item{token}{JWT token string}
}
\value{
POSIXct expiration time or NULL if cannot be determined
}
\description{
Helper function to get token expiration time for display purposes
}
\keyword{internal}
