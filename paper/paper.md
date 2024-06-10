---
title: 'FaaSr: R package for Function-as-a-Service'
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
    orcid: 0000-0000-0000-0000
    equal-contrib: true
    affiliation: 1
  - name: Nan Mu
    orcid: 0000-0000-0000-0000
    equal-contrib: true
    affiliation: 1
  - name: Vahid Daneshmand
    orcid: 0000-0003-4181-1806
    equal-contrib: true
    affiliation: 1
  - name: R. Quinn Thomas
    orcid: 0000-0003-1282-7825
    equal-contrib: true
    affiliation: 3
  - name: Cayelan C. Carey
    orcid: 0000-0001-8835-4476
    equal-contrib: true
    affiliation: 2
  - name: Renato J. Figueiredo
    orcid: 0000-0001-9841-6060
    equal-contrib: true
    affiliation: 1
affiliations:
 - name: Department of Electrical and Computer Engineering, University of Florida, FL, USA
   index: 1
 - name: Department of Biological Sciences, Virginia Tech, VA, USA
   index: 2
 - name: Department of Forest Resources and Environmental Conservation, Virginia Tech, VA, USA
   index: 3

date: 00 Jun 2024
bibliography: paper.bib

# Optional fields if submitting to a AAS journal too, see this blog post:
# https://blog.joss.theoj.org/2018/12/a-new-collaboration-with-aas-publishing
#aas-doi: 10.3847/xxxxx <- update this with the DOI from AAS once you know it.
#aas-journal: Astrophysical Journal <- The name of the AAS journal.
---

# Summary
The FaaSr software makes it easy for scientists to execute computational workflows developed natively using the R programming language in Function-as-a-Service (FaaS) serverless cloud infrastructures and using S3 cloud object storage[@amazon-s3;@minio]. A key objective of the software is to reduce barriers to entry to cloud computing for scientists in domains such as  environmental sciences, where R is widely used. To this end, FaaSr is designed to hide complexities associated with using cloud Application Programming Interfaces (APIs) for different FaaS and S3 providers, and exposes to the end user a set of simple function interfaces to: 1) register and invoke FaaS functions, 2) compose them to create workflow execution graphs, and 3) access cloud storage at run time. The software supports encapsulation of execution environments in Docker images that can be deployed reproducibly across multiple providers: AWS Lambda[@amazonwebsite-lambda], GitHub Actions[@githubwebsite], and OpenWhisk[@apache-openwhisk], where users are able to leverage a baseline image with the widely-used Rocker/Tidyverse runtime, as well as customize their execution environment if needed. FaaSr is available as a CRAN package to facilitate its installation in R environments. 

# Statement of need
Scientific research increasingly requires extensive data and computing resources to execute complex workflows that are increasingly event-driven. Cloud computing has emerged as a scalable solution to meet these demands. However, traditional Infrastructure-as-a-Service (IaaS) models often prove to be costly and require server management, presenting challenges to many scientists. In particular, this presents barriers to entry for small to medium teams and in domains where users are not accustomed to cloud server deployment and management and/or cluster and high-performance computing environments. Function-as-a-Service serverless computing has the potential to address these concerns by providing a cost-effective alternative where users are not burdened with server management and can simply focus on writing application logic instead. Nevertheless, today’s FaaS platforms still present barriers to entry with respect to usability for scientists, particularly those who heavily rely on the R programming language, because: 1) R is not widely supported by commercial and open-source FaaS platforms as a runtime target, and 2) different FaaS providers use different, non-compatible APIs. While there are systems that enable Python applications to be used in FaaS (such as NumpyWren[@Shankar18], PyWren[@Jonas17] and FuncX[@chard20]), there is a growing need to support R-native applications. This need is addressed by FaaSr through the use of containers that encapsulate an R-based runtime environment supporting the execution of user-provided functions. In addition, while existing systems are tailored to a specific FaaS platform, there is a need to support cross-platform execution to avoid vendor lock-in. This need is addressed by FaaSr by hiding provider-specific APIs behind function interfaces that work consistently across multiple serverless providers, including AWS Lambda, GitHub Actions, and OpenWhisk. Furthermore, there is a need to support complex scientific workflows to express the order of execution of functions, as well as parallelism. This need is addressed by FaaSr in a way that remains serverless in nature, and does not require dedicated/managed workflow engines.

# Description of Software
The FaaSr software is itself written in R. The main GitHub repository, FaaSr-Package, implements the core functionalities to register and invoke functions and to access data at runtime via S3 as well as via Apache Arrow[@arrow] over S3. FaaSr exposes both a client-side interface (intended for end users interactively using R/Rstudio environments) and a server-side interface (intended for runtime invocation once functions are executed on FaaS platforms). These use cURL[@hostetter1997curl] and API based packages httr[@httr] and paws[@paws] for sending requests to three supported FaaS providers: GitHub Actions, OpenWhisk, and AWS Lambda. Users are only required to have accounts, keys and proper access policies for those providers that they wish to utilize. 

The primary client-side functions exposed to users allow them to: 1) register workflows with FaaS providers, 2) invoke workflows as either a one-off, or to set timer schedules for triggering workflows on pre-specified intervals, and 3) copy execution logs from S3 storage to their desktop. The client-side interfaces build on the  `faasr` function, which creates an object instance in memory in the R session for the user, and which can then be subsequently used to register and invoke functions. This function takes as arguments the name of a JSON-formatted[@pezoa2016foundations] workflow configuration file, and (optionally) the name of a file storing FaaS/S3 cloud provider credentials. The JSON schema for this file is also stored in the FaaSr-Package repository. Here’s an example:  

```r
faasr_instance <- FaaSr::faasr(“payload.json”)`
```

With the instance `faasr_instance` returned by `faasr` function, users can register actions in the workflow to the FaaS provider(s) specified in the workflow JSON configuration. For example: 

```r
faasr_instance$register_workflow()
```

Users can trigger the action in the workflow by using the `invoke_workflow` function. The default action is the first action of workflow designated in the json configuration as `FunctionInvoke`. For example:

```r
faasr_instance$invoke_workflow()
```

Users can also call `set_workflow_timer` to establish a timer event that will automatically invoke the workflow. This is based on the cron[@reznick1993using] specification of time intervals. For example:
 
```r
faasr_instance$set_workflow_timer(“*/5 * * * *”)
```

The FaaSr server-side interfaces perform various operations, on behalf of the user, in stubs that are automatically inserted before and after user function invocation. These include: 1) reading the JSON workflow configuration file payload, 2) validating it against the FaaSr schema, 3) checking for reachability of S3 storage, 4) executing the user-provided function, 5) triggering the invocation of downstream function(s) in the workflow, and 6) storing logs. These functions are invoked at runtime by the containers deployed in an event-driven fashion by FaaS providers; the entry point of the container invokes the FaaSr package. Furthermore, some of the server-side interfaces are exposed to users, and implement functions to: 1) use S3 storage to download (get) and upload (put) full objects as files, 2) use Apache Arrow over S3 to efficiently access objects stored in columnar format using Apache Parquet, and 3) storing logs. Examples include:

```r
faasr_get_file(remote_folder=folder, remote_file=input1, local_file="df0.csv")
faasr_put_file(local_file="df1.csv", remote_folder=folder, remote_file=output1)
s3 <- faasr_arrow_s3_bucket()
faasr_log('Function compute_sum finished’)
```

The software also includes a FaaSr-Docker repository with code and actions used to build, configure, and upload container images to the respective container registers for the three platforms currently supported by FaaSr (GitHub’s GCR, AWS’s ECR, and DockerHub). These are used to build the base and default runtime environment for FaaSr (based on Rocker and TidyVerse) as well as for advanced users who may want to build their custom images starting from the base image.

Finally, the software also includes a FaaSr-JSON-Builder repository with code for an R-native graphical user interface Shiny app that allows users to create and edit workflows interactively and generate FaaSr schema-compliant JSON files.

# Documentation
The software has been released on The Comprehensive R Archive Network (CRAN) (https://cran.r-project.org/web/packages/FaaSr/index.html) and the documentation is available on both CRAN and the FaaSr website (https://faasr.io/documentation)

# Acknowledgements
FaaSr is funded in part by grants from the National Science Foundation (OAC-2311123 and OAC-2311124). Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.

# References
