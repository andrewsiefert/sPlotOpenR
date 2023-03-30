#' ## Install and load the sPlot package
devtools::install_github("andrewsiefert/sPlotOpenR", force = TRUE)
library(sPlotOpenR)

#' ### Load test data
data("greece")

#' ## Load and prepare additional data (non sPlotOpen) needed for testing 
library(tidyverse)
library(rnaturalearth)
library(sf)
sf_use_s2(use_s2 = FALSE)

countries <- ne_countries(returnclass = "sf")
europe <- countries %>% 
  filter(continent == "Europe") %>% 
  st_union()

#' ## Check sPlotOpen documentation

#' Check for package information
?sPlotOpen

#' There is no general description of the package

??sPlotOpen
#' But there is good documentation for the functions
