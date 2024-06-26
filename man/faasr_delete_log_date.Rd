% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/faasr_client_tools.R
\name{faasr_delete_log_date}
\alias{faasr_delete_log_date}
\title{faasr_delete_log_date}
\usage{
faasr_delete_log_date(target_date)
}
\arguments{
\item{target_date}{the date (yyyy-mm-dd) of the log data}
}
\value{
nothing is returned
}
\description{
deletes all FaaSr logs of a given date  
This function deleted all FaaSr log files in a given
date from the user's S3 FaaSrLog bucket
}
\examples{
if (interactive()){
test <- faasr("test.json", "env")
logs <- test$faasr_delete_log_date('2024-06-05')
}

}
