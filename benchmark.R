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


#' ## Testing the functions
#' ### get_sPlot()
#'
#' With default options
get_sPlot()

#' It worked nicely, including the progress messages and the creation of the directories.
#' However, all files in the iDiv repository were saved within the directory, in addition to the three table data (DT, header and CWM_CVM)
#' For example, the pdf file Demo.pdf and the folder Schema. 
#' I believe this is occurring because now we can only download all files from the iDiv portal.
#' Also, I think this will be changed by the argument metadata = FALSE, right?


#' #### Changing arguments
#' 
#' 
#' Ask for nonexistent table
get_sPlot(tables = "test")
#' Correct error message
#' 
#' Ask for nonexistent table while also asking for existing table
get_sPlot(tables = c("header", "test"))
#' Correct message about overwriting header, but no message that "test" was ignored.
#'
#'
#' Include existent directory
get_sPlot(dir = "C:/Users/faria/Nextcloud/sPlot")
#' It works nice, but save all files directly in directory, without placing them in a folder.
#' I think an option to handle this is to create a folder named after the version of sPlotOpen being downloaded, as discussed in the Google Docs.
#'

#' Include nonexistent directory
get_sPlot(dir = "C:/Users/faria/Nextcloud/nonexisting")
#' Works nicely, creating the desired directory.