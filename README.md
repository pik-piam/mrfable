# FABLE project data

R package **mrfable**, version **1.6.3**

[![CRAN status](https://www.r-pkg.org/badges/version/mrfable)](https://cran.r-project.org/package=mrfable)  [![R build status](https://github.com/giannou/mrfable/workflows/check/badge.svg)](https://github.com/giannou/mrfable/actions) [![codecov](https://codecov.io/gh/giannou/mrfable/branch/master/graph/badge.svg)](https://app.codecov.io/gh/giannou/mrfable) [![r-universe](https://pik-piam.r-universe.dev/badges/mrfable)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Tool for easy downloading, cleaning, and sorting foodcrop data for India taken from here: https://eands.dacnet.nic.in/APY_96_To_07.htm .


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrfable")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r
vignette("mrfable") # India foodcrop data preparation with R
```

## Questions / Problems

In case of questions / problems please contact Anastasis Giannousakis <giannou@pik-potsdam.de>.

## Citation

To cite package **mrfable** in publications use:

Giannousakis A (2024). _mrfable: FABLE project data_. R package version 1.6.3.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrfable: FABLE project data},
  author = {Anastasis Giannousakis},
  year = {2024},
  note = {R package version 1.6.3},
}
```
