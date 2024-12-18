# FaaSr 1.4.3
* Support triggering of multiple parallel executions from an action
* Added faasr_rank() method to determine the rank of a parallel invocation
* Support passing of payloads through API invocation for GitHub Actions
* Improvement of faasr_s3_check() to support AWS S3 buckets

# FaaSr 1.3.0

* Support anonymous access to arrow/S3 buckets 
* Self-abort on a timeout while waiting for a lock
* Improve credential management in Rstudio
* Add logging functions to faasr_client_tools.R

# FaaSr 1.2.1

* Fix bugs for local execution functions.

# FaaSr 1.2.0

* Print the UUID and logs folder to help users locate logs when an action fails
* Implement faasr_get_folder_list function to retrieve S3 folder contents as a list
* Return without action failure on faasr_abort_on_multiple_invocations
* Support local execution on client for development and debugging
* Fixed bug that limited maximum memory size in AWS Lambda
* Increased default size of /tmp storage in AWS Lambda

# FaaSr 1.1.2

* This version incorporates CRAN review feedback from initial submission

# FaaSr 1.1.1

* CRAN New Submission