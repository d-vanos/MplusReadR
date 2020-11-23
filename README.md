# MplusReadR

MplusReadR formats Mplus output from multiple models into APA-style tables and tidy datasets.

## Installation
```R
# Install and load devtools from CRAN 
install.packages("devtools")
library(devtools)

# Install MplusReadR from Github 
install_github("d-vanos/MplusReadR")
library(MplusReadR)
```
The latest version of the package can be installed by re-running `install_github("d-vanos/MplusReadR")`. 

## Components
There are two components to MplusReadR. The first is a set of functions which have been custom-made for a project known as Dejonckheere (2019). These all start with 'dejon', and can only be used for this project. The second is a set of functions which are based off those created for the 'dejon' project, but these can be used more widely across different types of Mplus output. These functions all start with 'mplus'.  

## References
Hallquist, M. N. & Wiley, J. F. (2018). MplusAutomation: An R Package for Facilitating Large-Scale Latent Variable Analyses in Mplus. Structural Equation Modeling, 25, 621-638. doi: 10.1080/10705511.2017.1402334.
