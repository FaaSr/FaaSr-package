---
title: 'FaaSr: R Package for Function-as-a-Service Cloud Computing'
tags:
  - R
  - cloud computing
  - function-as-a-service
authors:
  - name: Sungjae Park
    orcid: 0009-0000-5357-804X
    equal-contrib: true
    affiliation: 1
  - name: Yun-Jung Ku
    equal-contrib: false
    affiliation: 1
  - name: Nan Mu
    equal-contrib: false
    affiliation: 1
  - name: Vahid Daneshmand
    orcid: 0000-0003-4181-1806
    equal-contrib: false
    affiliation: 1
  - name: R. Quinn Thomas
    orcid: 0000-0003-1282-7825
    equal-contrib: false
    affiliation: 3
  - name: Cayelan C. Carey
    orcid: 0000-0001-8835-4476
    equal-contrib: false
    affiliation: 2
  - name: Renato J. Figueiredo
    orcid: 0000-0001-9841-6060
    equal-contrib: true
    affiliation: 1
affiliations:
 - name: Department of Electrical and Computer Engineering, University of Florida, FL, USA
   index: 1
 - name: Department of Biological Sciences and Virginia Tech Center for Ecosystem Forecasting, Virginia Tech, VA, USA
   index: 2
 - name: Department of Forest Resources and Environmental Conservation and Virginia Tech Center for Ecosystem Forecasting, Virginia Tech, VA, USA
   index: 3

date: 15 Jul 2024
bibliography: paper.bib

# Optional fields if submitting to a AAS journal too, see this blog post:
# https://blog.joss.theoj.org/2018/12/a-new-collaboration-with-aas-publishing
#aas-doi: 10.3847/xxxxx <- update this with the DOI from AAS once you know it.
#aas-journal: Astrophysical Journal <- The name of the AAS journal.
---

# Summary
The FaaSr software makes it easy for scientists to execute computational workflows developed natively using the R programming language in Function-as-a-Service (FaaS) serverless cloud infrastructures and using S3 cloud object storage [@amazon-s3;@minio]. A key objective of the software is to reduce barriers to entry to cloud computing for scientists in domains such as environmental sciences, where R is widely used [@lai2019evaluating]. To this end, FaaSr is designed to hide complexities associated with using cloud Application Programming Interfaces (APIs) for different FaaS and S3 providers, and exposes to the end user a set of simple function interfaces to: 1) register and invoke FaaS functions, 2) compose functions to create workflow execution graphs, and 3) access cloud storage at run time. The software supports encapsulation of execution environments in Docker images that can be deployed reproducibly across multiple providers: AWS Lambda [@amazonwebsite-lambda], GitHub Actions [@githubwebsite], and OpenWhisk [@apache-openwhisk], where users are able to leverage a baseline image with the widely-used Rocker/Tidyverse runtime [@Nust2020Rocker], as well as customize their execution environment if needed. FaaSr is available as a CRAN package to facilitate its installation in R environments. 

# Statement of Need
Scientific research increasingly requires extensive data and computing resources to execute complex workflows that are increasingly event-driven. Cloud computing has emerged as a scalable solution to meet these demands. However, traditional Infrastructure-as-a-Service (IaaS) models often prove to be costly and require server management, presenting challenges to many scientists. In particular, these challenges present barriers to entry for small to medium teams and in domains where users are not accustomed to cloud server deployment and management and/or cluster and high-performance computing environments. Function-as-a-Service serverless computing has the potential to address these concerns by providing a cost-effective alternative where users are not burdened with server management and can simply focus on writing application logic instead. Nevertheless, today’s FaaS platforms still present barriers to entry with respect to usability for scientists, particularly those who heavily rely on the R programming language, because: 1) R is not widely supported by commercial and open-source FaaS platforms as a runtime target, and 2) different FaaS providers use different, non-compatible APIs. While there are systems that enable Python applications to be used in FaaS (such as NumpyWren [@Shankar18], PyWren [@Jonas17], and FuncX [@chard20]), there is a growing need to support R-native applications. Two existing packages for R, lambdr [@lambdr] and aws.lambda [@awslambdaR] provide support for AWS, but are specific to a single provider and do not generalize to support workflows across different FaaS providers. This need is addressed by FaaSr through the use of containers that encapsulate an R-based runtime environment supporting the execution of user-provided functions. In addition, while existing systems are tailored to a specific FaaS platform, there is a need to support cross-platform execution to avoid vendor lock-in. This need is addressed by FaaSr by hiding provider-specific APIs behind function interfaces that work consistently across multiple serverless providers, including AWS Lambda, GitHub Actions, and OpenWhisk. Furthermore, there is a need to support complex scientific workflows to express the order of execution of functions, as well as parallelism. This need is addressed by FaaSr in a way that remains serverless in nature and does not require dedicated/managed workflow engines.

# Design
The FaaSr package consists of server-side and client-side functions. The server-side functions are executed when an action is deployed by a FaaS platform. The FaaSr server-side interfaces perform various operations, on behalf of the user, in stubs that are automatically inserted before and after user function invocation. These include: 1) reading the JSON workflow configuration file payload, 2) validating it against the FaaSr schema, 3) checking for reachability of S3 storage, 4) executing the user-provided function, 5) triggering the invocation of downstream function(s) in the workflow, and 6) storing logs. These functions are invoked at runtime by the containers deployed in an event-driven fashion by FaaS providers; the entry point of the container invokes the FaaSr package. Furthermore, some of the server-side interfaces are exposed to users, and implement functions to: 1) use S3 storage to download (get) and upload (put) full objects as files, 2) use Apache Arrow over S3 to efficiently access objects stored in columnar format using Apache Parquet, and 3) store logs. 

The client-side functions are executed iteratively by a user from their desktop environment (e.g., RStudio). The primary client-side functions exposed to users allow them to: 1) register workflows with FaaS providers, 2) invoke workflows as either a one-off or to set timer schedules for triggering workflows at pre-specified intervals, and 3) copy execution logs from S3 storage to their desktop. The client-side interfaces build on the R  `faasr` function, which creates an object instance in memory in an R session for the user, and which can then be subsequently used to register and invoke functions. This function takes as arguments the name of a JSON-formatted [@pezoa2016foundations] workflow configuration file, and (optionally) the name of a file storing FaaS/S3 cloud provider credentials. The JSON schema for this file is also stored in the FaaSr-Package repository.

FaaSr supports the execution of workflows that can be expressed as a Directed Acyclic Graph (DAG) of functions. The graph (specifying functions and their dependences) is described in JSON format, which can be generated automatically from a Web-based graphical editor using the FaaSr-JSON-Builder tool [@faasr-json-builder] . \autoref{fig:workflow} shows an example workflow DAG graph with ten functions for an ecological forecasting application.

![Fig. 1. FaaSr Example Workflow.\label{fig:workflow}](FaaSr_example_workflow.png)


# Description of Software
The FaaSr software is itself written in R. The main GitHub repository, FaaSr-Package, implements the core functionalities to register and invoke functions and to access data at runtime via S3 as well as via Apache Arrow [@arrow] over S3. FaaSr exposes both a client-side interface (intended for end users interactively using R/RStudio environments) and a server-side interface (intended for runtime invocation once functions are executed on FaaS platforms). These use cURL [@hostetter1997curl] and API-based packages httr [@httr] and paws [@paws] for sending requests to three supported FaaS providers: GitHub Actions, OpenWhisk, and AWS Lambda. Users are only required to have accounts, keys, and proper access policies for those providers that they wish to utilize.

The client-side interface is available by invoking the FaaSr::faasr() function with a valid payload as argument:

```r
faasr_instance <- FaaSr::faasr("payload.json")
```

With the instance `faasr_instance` returned by the `faasr` function, users can register actions in the workflow to the FaaS provider(s) specified in the workflow JSON configuration. For example: 

```r
faasr_instance$register_workflow()
```

Users can trigger the action in the workflow by using the `invoke_workflow` function. The default action is the first action of the workflow designated in the JSON configuration as `FunctionInvoke`. For example:

```r
faasr_instance$invoke_workflow()
```

Users can also call `set_workflow_timer` to establish a timer event that will automatically invoke the workflow. This is based on the cron [@reznick1993using] specification of time intervals. For example:
 
```r
faasr_instance$set_workflow_timer("*/5 * * * *")
```

The server-side interface allows functions to interact with storage. For example, to download a file from an S3 server to local storage:

```r
faasr_get_file(remote_folder=folder, remote_file=input1, local_file="df0.csv")
```

To upload a file from local storage to an S3 server:

```r
faasr_put_file(local_file="df1.csv", remote_folder=folder, remote_file=output1)
```

To read/write from an S3 bucket with Apache Arrow and Parquet:

```r
s3 <- faasr_arrow_s3_bucket()
```

To write a log message:

```r
faasr_log("Function compute_sum finished")
```

The software also includes a FaaSr-Docker repository [@faasr-docker] with code and actions used to build, configure, and upload container images to the respective container registers for the three platforms currently supported by FaaSr (GitHub’s GCR, AWS’s ECR, and DockerHub). These are used to build the base and default runtime environment for FaaSr (based on Rocker and TidyVerse) as well as for advanced users who may want to build their custom images starting from the base image.

Finally, the software also includes a FaaSr-JSON-Builder repository [@faasr-json-builder] with code for an R-native graphical user interface Shiny app that allows users to create and edit workflows interactively and generate FaaSr schema-compliant JSON files.

# Documentation
The software has been released on The Comprehensive R Archive Network (CRAN) ([https://cran.r-project.org/web/packages/FaaSr/](https://cran.r-project.org/web/packages/FaaSr/)) and the documentation is available on both CRAN and the FaaSr website ([https://faasr.io/documentation](https://faasr.io/documentation))

# Acknowledgements
FaaSr is funded in part by grants from the National Science Foundation (OAC-2311123, OAC-2311124, EF-2318861, EF-2318862). Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.

# References
