
<!-- README.md is generated from README.Rmd. Please edit that file -->

# [MplusReadR](https://d-vanos.github.io/MplusReadR/index.html)

Mplus is a latent variable modeling program. Model output generated from
Mplus can be imported into R and manipulated using the
[MplusAutomation](https://cran.r-project.org/web/packages/MplusAutomation/index.html)
package. [MplusReadR](https://d-vanos.github.io/MplusReadR/index.html)
extends on MplusAutomation by making it easier to compile the output
from multiple Mplus output files into tidy datasets, and to manipulate
these to create APA-style html tables.

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

In general it is not necessary to update to the most recent version of
listed packages (if this message is displayed) though you may wish to do
so if there are issues installing the package.

The latest version of MplusReadR can be installed by re-running
`install_github("d-vanos/MplusReadR")`.

## Getting Started

There are two components to MplusReadR. The first is a set of functions
which have been custom-made for a project known as Dejonckheere (2019).
These all start with ‘dejon’, and can only be used for this project. The
second is a set of functions which are based off those created for the
‘dejon’ project, but these can be used more widely across different
types of Mplus output. These functions all start with ‘mplus’.

Each set of functions has its own tutorial. To get started, check out
[this
tutorial](https://d-vanos.github.io/MplusReadR/articles/MplusReadR-vignette.html).

## Troubleshooting

If you are having trouble downloading the package, try the following
options:

#### Update R

MplusReadR was built on R version 4.0.3, and depends on R version 2.10
or above. To update R, follow [these
instructions](https://uvastatlab.github.io/phdplus/installR.html).

#### Install Required Packages

If you get an error similar to this when attempting to install
MplusReadr:  
`Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck =
vI[[i]]) : there is no package called 'reshape2'`

Try installing these packages individually. For example,
`install.packages("reshape2")`.

#### Install RTools

If the suggestions above do not fix the issue, try installing [Rtools
for Windows](https://cran.r-project.org/bin/windows/Rtools/) or [Xcode
for Mac](https://apps.apple.com/au/app/xcode/id497799835?mt=12).

## References

Hallquist, M. N. & Wiley, J. F. (2018). MplusAutomation: An R Package
for Facilitating Large-Scale Latent Variable Analyses in Mplus.
Structural Equation Modeling, 25, 621-638. doi:
10.1080/10705511.2017.1402334.
