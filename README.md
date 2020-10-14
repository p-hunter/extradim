# extradim
Extra Tidymodels Recipe Steps for Dimensionality Reduction

Contains recipe steps for factor analysis (`step_efa`) using the `psych` and `GPArotation` packages, and multiple correspondence analysis (`step_mca`) using the `MASS` package.

Please note that multiple correspondence analysis should be run <b><i> BEFORE </i></b> using `step_dummy`. 
In addition to this, both functions purposely do not eliminate the pre-existing variables that end up being selected for this step.


# Installation

There is no CRAN version of this package just yet. The user needs to install the package from this GitHub.

```{r}

require(devtools)
install_github("p-hunter/extradim")

```



