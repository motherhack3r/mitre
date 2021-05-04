## Test environments
* local win10 install, R 4.0.5
* Ubuntu 16.04.6 LTS (on travis-ci), R 4.0.2

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking sizes of PDF files under 'inst/doc' ... NOTE
  Unable to find GhostScript executable to run checks on size reduction

I added a vignette as PDF to easy view some examples.

## RHUB check results
* Fedora build execution halted with an error: 
  Bioconductor version '3.13' requires R version '4.1'; R version is too new
  Ref: https://github.com/r-hub/rhub/issues/462
  This package does not depend on Bioconductor
