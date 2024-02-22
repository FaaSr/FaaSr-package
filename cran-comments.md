## comments for CRAN submission

* This is a new release.

## Test environments

* local MacOS install, R 4.2.2
* github actions/ windows-latest - R Release
* github actions/ macOS-latest - R Release
* github actions/ ubuntu-latest - R Release
* github actions/ ubuntu-latest - R Devel
* github actions/ ubuntu-latest - R Oldrel-1
* win-builder (devel)

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 2 note ✖

There were no ERRORs or WARNINGs

2 NOTE:

❯ checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Figueiredo Renato <renatof@ufl.edu>'
  
  New submission

❯ Package suggested but not available for checking: ‘arrow’

  * This is because 'arrow' package currently has an error (CRAN build). - All other packages using arrow as a dependency failed R CMD Checks.
  * arrow is not required for operation of FaaSr - which can use S3 for data transfers. We will resubmit a package request once arrow building works again.

## Downstream dependencies

The package has no reverse dependencies.