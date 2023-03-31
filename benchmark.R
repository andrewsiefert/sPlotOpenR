#' ---
#' title: "Testing package's functions"
#' output: github_document
#' ---

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
?sPlotOpenR

#' There is no general description of the package.

??sPlotOpenR
#' But there is good documentation for the functions.


#' ## Testing the functions
#' ### get_sPlot()
#'
#' With default options
get_sPlot()

#' It worked nicely, including the progress messages and the creation of the directories.
#' However, all files in the iDiv repository were saved within the directory, in addition to the three table data (DT, header and CWM_CVM)
#' For example, the pdf file Demo.pdf and the folder Schema. 
#' I believe this is occurring because now we can only download all files from the iDiv portal.
#' Also, I think this will be changed when the  argument metadata is implemented, right?
#'
#'
#' #### Changing arguments
#' 
#' 
#' Ask for nonexistent table
get_sPlot(tables = "test")
#' Correct error message
#' 
#' Ask for nonexistent table while also asking for existing table
get_sPlot(tables = c("header", "test"))
#' Correct message about overwriting header, but no message that "test" was ignored. Should we add it?
#'
#'
#' Include existent directory
get_sPlot(dir = "C:/Users/faria/Nextcloud/sPlot")
#' It works nice, but save all files directly in directory, without placing them in a folder.
#' I think an option to handle this is to create a folder named after the version of sPlotOpen being downloaded, as discussed in the Google Docs.
#'
#'
#' Include nonexistent directory
get_sPlot(dir = "C:/Users/faria/Nextcloud/nonexisting")
#' Works nicely, creating the desired directory.
#' 
#' 
#' Only download files, without loading them in R.
get_sPlot(load = FALSE)
#' Works nicely.
#' 
#' Not requesting metadata
get_sPlot(metadata = FALSE)
#' Not implemented yet.
#' 
#' 
#' ### read_sPlot()
#' 
#' With default options
#' 
read_sPlot()
#' Works fine.
#' 
#' Reading from an alternative directory
read_sPlot(dir = "C:/Users/faria/Nextcloud/sPlotOpen/data")
#' Works fine.
#' 
#' Reading it from a wrong directory.
read_sPlot(dir = "~/Nextcloud")
#' It simply does nothing, should we add an error message?
#'
#' Reading only one table
read_sPlot(tables = c("DT"))
#' Works fine.
#' 
#' Reading a nonexistent table
read_sPlot(tables = c("test"))
#' Correct error message
#' 
#' Reading a nonexistent table together with an existent table
read_sPlot(tables = c("DT", "test"))
#' No message about the ignored argument. Should we add any?
#' 
#' 
#' 
#' ### filter_species()
#' 
#' Default options
spp_list <- c("Luzula arcuata", "Bauhinia variegata")
data <- read_sPlot()

filter_species(data, spp_list)
#' Works fine, including the messages.
#' 
#' Setting resolve = TRUE
filter_species(data, spp_list = c("Luzulula arcuata"), resolve = TRUE)
#' **Dependency problem: results in error if TNRS package is not installed.**
#' The matching using TNRS works fine after the package is installed.
#' 
#' 
#' Setting join = TRUE
filter_species(data, spp_list, join = TRUE)
#' Works fine
#' 
#' Using a different object as input for spp_list (as a tibble or a list)
filter_species(data, spp_list = as_tibble(spp_list))
#' Nothing is done and no message is returned. Should we add one?
#' 
#' 
#' Using a wrong input for the data argument
filter_species(spp_list)
#' Not very usefull automatic message, should we add one?
#' 
#' 
#' ### filter_polygon()
#' 
#' Default options
filter_polygon(data, countries %>% filter(continent == "Africa"))
#' Works nicely.
#' 
#' Setting join = TRUE
filter_polygon(data, countries %>% filter(continent == "Africa"), join = TRUE)
#' Works nicely
#' 
#' Testing a polygon with geometry composed by a list of lists
filter_polygon(data, europe)
#' Also works fine
#' 
#' Testing a wrong input for data
filter_polygon(europe)
#' Same as for filter_species(). Should we add a more informative message?
#' 
#' Testing a wrong input for polygon.
filter_polygon(data, countries %>% st_drop_geometry())
#' Uninformative message. Should we add a better one?
#' 
#' ### map_plots()
#' 
#' Default options
map_plots(data)
#' **Dependency problem: results in error if package dggridR is not installed**
#' Works nicely after installed.
#' 
#' Including wrong input for data
map_plots(spp_list)
#' Uninformative message, should we add a better one?
#' 
#' Setting type = "points"
map_plots(data, type = "points")
#' Works fine
#' 
#' Setting type as a different option
map_plots(data, type = "polygon")
#' Nice informative error message.
#' 
#' Setting a different size of grid
map_plots(data, grid_size = 500)
#' Works nicely.
#' 