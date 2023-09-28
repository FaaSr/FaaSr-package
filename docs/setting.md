# Introduction
### FaaSr is ...

# Prerequisites
## R library
* FaaSr
* uuid
* jsonlite
## github actions
* git
* gh
## OpenWhisk - ibmcloud
* ibmcloud
* ibmcloud function plug-in
## Labmda
* aws

# JSON format
``` r
{   "FunctionInvoke": "first_action",
    "InvocationID": "uuid_format_id",
    "FaaSrLog": "logfolder_name/default:FaaSrLog",
    "LoggingServer": "Servername for logging/should be one of DataStores Server name",
    "FunctionList": {
        "F1": {
            "FaaSServer": "target_faas_server/should be one of ComputeServers Server name",
            "Actionname": "actionname for function",
            "Arguments": {
                "input1": "input1.csv",
                "input2": "input2.csv"
            },
            "InvokeNext": [
                "F2",
                "F3"
            ]
        },
        "F2": {
            "FaaSServer": "My_Github_Account",
            "Actionname": "workflow.yml",
            "Arguments": {
                "input": "input3.csv"
            }
        },
        "F3": {
            "FaaSServer": "My_AWS_Account",
            "Actionname": "f3_action",
            "Arguments": {
                "input": "input123.csv"
            }
    },
    "ActionContainers": {
        "f3_action": "user_docker_id/user_custom_image"
    },
    "FunctionCRANPackage": {
        "F2": [
            "cran_package",
            "cran_package_1"
        ],
        "F3": [
            "dpkg",
            "dplyr"
        ]
    },
    "FunctionGitHubPackage": {
        "F1": [
            "github_package",
            "github_package_1"
        ],
        "F2": [
            "FLARE-forecast/FLAREr",
            "FaaSr/FaaSr-package"
        ]
    },
    "FunctionGitRepo": {
        "F1": [
            "git_repository",
            "git_repository1"
        ],
        "F3": [
            "user_id/user_repo",
            "user_id_1/user_repo_1"
        ]
    },
    "ComputeServers": {
        "My_Github_Account": {
            "FaaSType": "GitHubActions",
            "UserName": "user_name",
            "ActionRepoName": "repo_name",
            "Ref": "branch1",
            "Token": "git_token" 
        },
        "My_AWS_Account": {
            "FaaSType": "Lambda",
            "AccessKey": "lambda_access_key",
            "SecretKey": "lambda_secret_key",
            "Region": "test"
        },
        "My_IBM_Account": {
            "FaaSType": "OpenWhisk",
            "Endpoint": "http://00.00.00.00",
            "Namespace": "my_namespace",
            "API.key": "ow_api_key",
            "Region": "test"
        }
    },
    "DataStores": {
        "My_S3_Account": {
            "Endpoint": "",
            "Bucket": "my_bucket",
            "Region": "us-east-1",
            "Writable": "TRUE",
            "AccessKey": "s3_access_key",
            "SecretKey": "s3_secret_key"
        },
        "My_Minio_Account": {
            "Endpoint": "https://play.min.io",
            "Bucket": "my_bucket1",
            "Region": "",
            "Writable": "TRUE",
            "AccessKey": "minio_access_key",
            "SecretKey": "minio_secret_key"
        }
    }
}
```

# How to use

### Set your credentials(keys) for FaaS and Storage

There are three ways to set keys

**1. define the real key into the JSON file<br>**
We don't recommend this because of hazard to upload JSON file
 with credentials on the Internet
``` r
"ComputeServers":{
  "My_IBMcloud_Account":{
    ...,
    "API.key":"my_ibmcloud_api_key"
  }
}
```
**2. use `Sys.setenv()`, which is recommended<br>**
In the Key:value pair, Key is "your server name" + "_" +"key type(capital)"<br>
In the Key:value pair, Value is "your real key value"<br>
Server name and key type should be strictly matching to what you define in the JSON
``` r
Sys.setenv("My_IBMcloud_Account_API_KEY"="ibmcloud api key")
Sys.setenv("My_Lambda_Account_ACCESS_KEY"="aws lambda access key")
Sys.setenv("My_Lambda_Account_SECRET_KEY"="aws lambda secret key")
Sys.setenv("My_Githubactions_Account_TOKEN"="github actions token")
Sys.setenv("My_S3_Account_ACCESS_KEY"="s3 access key")
Sys.setenv("My_S3_Account_ACCESS_KEY"="s3 secret key")
```

**3. use `env` file. You can use when starting the `faasr()`<br>**
`env` file contains Key:value pair.<br>
In the Key:value pair, Key is "your server name" + "_" +"key type(capital)"<br>
In the Key:value pair, Value is "your real key value"<br>
Each Key:value pair is separated by either "," or "\n"
``` r
env
``` 
``` r
"My_IBMcloud_Account_API_KEY"="ibmcloud api key",
"My_Lambda_Account_ACCESS_KEY"="aws lambda access key",
"My_Lambda_Account_SECRET_KEY"="aws lambda secret key",
"My_Githubactions_Account_TOKEN"="github actions token"
"My_S3_Account_ACCESS_KEY"="s3 access key"
"My_S3_Account_ACCESS_KEY"="s3 secret key"
```

### Set configurations under different JSON files and requirements.
You can use the FaaSr library `faasr()`
``` r
faasr_run <- FaaSr::faasr(json_path="your_json_file_path", env_path="your_cred_file_path")
```
NOTE: json_path is only requirement. env_path is optional.

You can find that new directory named `faasr_json` has been created.
This directory is to archive the json file. You may see nothing inside, unless listing hidden files.

Under the `faasr_run` you defined, there are functions to register and invoke and configurations.
  - `register_workflow`: register functions
  - `invoke_workflow`: invoke workflows
  - `json`: json you provide
  - `cred`: cred you provide
  - `json_path`: json file path


### Register functions
NOTE: You must have FaaS Provider logged in and FaaS CLI be installed.
To register workflow, you can run the command:
``` r
faasr_run$register_workflow()
```
No arguments are required. It will read the JSON and set it up.

While registering functions, it may need you to provide inputs such as:
``` r
[faasr_msg] lambda function --  [function_name] already exists.
[faasr_msg] Do you want to update it?[y/n]
```
``` r
[faasr_msg] Directory for the repository already exists
[faasr_msg] Update?[y/n]
```

If there's an error, it will leave a message and stop.
``` r
Target Resource group(Type "Enter" to proceed with default value): 
[wrong_user_inputs]


Target Resource group Failed, please check resource group

Error in faasr_register_workflow_ibmcloud_target_group() :
```

### Invoke workflows
NOTE: You must have FaaS Provider logged in and FaaS CLI be installed.

After successfully register functions, you can invoke workflow with following command:
``` r
faasr_run$invoke_workflow(FunctionInvoke=NULL)
```
NOTE: FunctionInvoke is optional, if it is set, then given FunctionInvoke will be executed instead of faasr$FunctionInvoke

It reads the JSON file in the path `faasr_run$json_path`
It collects the credentials in `faasr_run$cred` and `environments`

You can check the result and log in the FaaS provider's monitor or Object-oriented Storage.
