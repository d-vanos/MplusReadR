
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MplusReadR

Mplus is a latent variable modeling program. Model output generated from
Mplus can be imported into R and manipulated using the
[MplusAutomation](https://cran.r-project.org/web/packages/MplusAutomation/index.html)
package. MplusReadR extends on MplusAutomation by making it easier to
compile the output from multiple Mplus output files into tidy datasets,
and to manipulate these to create APA-style html tables.

## Installation

To get started with MplusReadR, download the following:

``` r
# Install and load devtools from CRAN 
install.packages("devtools")
library(devtools)

# Install MplusReadR from Github 
install_github("d-vanos/MplusReadR")
library(MplusReadR)
```

The latest version of the package can be installed by re-running
`install_github("d-vanos/MplusReadR")`.

## Getting Started

There are two components to MplusReadR. The first is a set of functions
which have been custom-made for a project known as Dejonckheere (2019).
These all start with ‘dejon’, and can only be used for this project. The
second is a set of functions which are based off those created for the
‘dejon’ project, but these can be used more widely across different
types of Mplus output. These functions all start with ‘mplus’.

Each set of functions has its own tutorial. Check out the [Dejon
vignette](https://d-vanos.github.io/MplusReadR/articles/Dejon-vignette.html)
and the general [MplusReadR
vignette](https://d-vanos.github.io/MplusReadR/articles/MplusReadR-vignette.html).

## References

Hallquist, M. N. & Wiley, J. F. (2018). MplusAutomation: An R Package
for Facilitating Large-Scale Latent Variable Analyses in Mplus.
Structural Equation Modeling, 25, 621-638. doi:
10.1080/10705511.2017.1402334.
