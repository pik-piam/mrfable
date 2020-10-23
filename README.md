# FABLE project data

R package **mrfable**, version **0.0.4**

  

## Purpose and Functionality

Provides functions to read in FABLE project data.


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

## Questions / Problems

In case of questions / problems please contact Anastasis Giannousakis <giannou@pik-potsdam.de>.

## Citation

To cite package **mrfable** in publications use:

Giannousakis A (2020). _mrfable: FABLE project data_. R package version 0.0.4.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrfable: FABLE project data},
  author = {Anastasis Giannousakis},
  year = {2020},
  note = {R package version 0.0.4},
}
```

