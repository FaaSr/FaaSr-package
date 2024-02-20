# FaaSr (1.1.0.1)

* Bug fix
  + Add condition for checking the account_id of given image
  + Add basic_ld_image_account_id
* CRAN New Submission

# FaaSr (1.1.0.0)

* Edit function's descriptions
  + Add "@examples" in the function description.
  + Add "@Keywords internal" in the function that will not be exported.
  + Delete "@export" in some functions that will not be exported.
* Update the word list file by `devtools::spell_check()` & `spelling::update_wordlist()`
* Update Rd files.

#FaaSr (1.0.9.0)

* Add support for user-client functions to register AWS Lambda functions without "aws" CLI.
* Edit R Scripts meta-data.

# FaaSr (1.0.8.0)

* Add "faasr_delete_file.R" to delete the file in the S3 storage.
* Register functions - openwhisk : define default ssl & add `toupper` function
* Bug Fix for typo

# FaaSr (1.0.7.0)

* Bug fix for "httr"" and "paws" "config" functions.

# FaaSr (1.0.6.0)

* Bug fix for openwhisk related to SSL.
  + Handling "SSL" in the `faasr_ow_httr_request`.
  + "config" function is not only used in "httr": specify the library.

# FaaSr (1.0.5.0)

* Register github actions.
  + Use template in the repository for workflow: able to manage it dynamically.
  + no "gh" dependency: now use "httr" library, which is based on "Curl".
* Register openwhisk.
  + As IBMcloud functions is deprecated, no more ibmcloud functions CLI nor API is involved.
  + no "wsk"/"ibmcloud" dependencies: now use "httr" library, which is based on "Curl".
* Improvement.
  + Schema: Region is not required for openwhisk / SSL is included(not required though).
  + Use "cli" library to make software look clean.
  + triggering lambda is conducted by `lambda$invoke_async` instead of `lambda$invoke` to speed up.


# FaaSr (1.0.4.0)

* Improvement : Bring the list from the bucket and check the '.done' files instead of sending multiple get requests to the bucket.
* Error correction : Let the system sleep if the action fails to get the flag.

# FaaSr (1.0.3.0)

* Users can set 'cron' timer in Github/Openwhisk/Lambda
* Bug fix
  + if container is not specified, it uses tidyverse basic image.
  + if `faasr$FunctionList$'userfunction'$Arguments` is empty, it returns empty list: `list()`
* Add openwhisk native as IBMcloud functions will be deprecated

# FaaSr (1.0.2.0)

* Backoff for the locking: faasr_lock.R / faasr_acquire
  + After try to get a lock, if it fails, system will sleep exponentially.
  + 'cnt' is for counting the number, and 'max_cnt' is maximum count.
* README file: `faasr_client_api_github-actions.R` / `faasr_register_workflow_github_readme`
  + README file is added to describe the repository.
* Bug fix: Bucket name for arrow function 

# FaaSr (1.0.1.0)

* Actionname -> FunctionName.
  + Each node(names of FunctionList) is the action and its name is the action name.
  + Each node has FunctionName.
* Ref -> Branch
  + In the json file, "Ref" is now "Branch"
* Changing some messages
  + In the user-side API, messages are changed.
  + message are starting with "['faasr_msg']"

# FaaSr (1.0.0.0)

* Initial version.