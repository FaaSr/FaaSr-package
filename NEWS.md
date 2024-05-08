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