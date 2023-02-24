
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sPlotOpenR

<!-- badges: start -->
<!-- badges: end -->

The goal of sPlotOpenR is to make it easier to acquire, process, and
visualize data from the sPlotOpen global vegetation dataset.

## Installation

You can install the development version of sPlotOpenR like so:

``` r
devtools::install_github("andrewsiefert/sPlotOpenR")
```

## Example

``` r
library(sPlotOpenR)

# download sPlotOpen dataset and load into R
data <- get_sPlot(load = T)
```
