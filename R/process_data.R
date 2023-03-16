#' Create a Site-by-Species Matrix
#'
#' @param data sPlotOpen species composition data in long format.
#' @param sparse Whether to return a sparse matrix (default is `FALSE`).
#' @param pres_abs Whether to convert species relative cover data to
#'   presence-absence (default is `FALSE`).
#'
#' @return A matrix (sites as rows, species as columns) of species relative
#'   cover or presence-absence.
#' @export
#'
#' @examples
#' data(greece)
#'
#' comp <- greece$DT
#' m <- site_by_species(comp)
site_by_species <- function(data, sparse = FALSE, pres_abs = FALSE) {
  sites <- factor(data$PlotObservationID)
  species <- factor(data$Species)
  if(isTRUE(pres_abs)) {
    x <- 1
  } else {
    x <- data$Relative_cover
  }

  s <- Matrix::sparseMatrix(
    as.numeric(sites),
    as.numeric(species),
    x = x,
    dimnames = list(levels(sites), levels(species)))

  if(isTRUE(sparse)) {
    return(s)
  } else {
    return(as.matrix(s))
  }

}


#' Filter sPlotOpen Data by Species
#'
#' `filter_species()` finds all vegetation plots that contain at least one
#' species in a list you provide.
#'
#' @param data sPlotOpen data, a named list containing `DT` (species composition
#'   data in long format) and `header` (plot-level information).
#' @param spp_list A vector of species names.
#' @param join Whether to join the filtered `DT` and `header` tables.
#'
#' @return sPlotOpen data filtered to include only plots that contain at least
#'   one species in `spp_list`. If `join = F`, a list containing the filtered
#'   `DT` and `header` tables. If `join = T`, a single data from containing the
#'   joined tables.
#'
#' @export
#'
#' @examples
#' data(greece)
#'
#' spp <- c("Silene atropurpurea", "Corylus avellana")
#' greece_filtered <- filter_species(greece, spp, join = FALSE)
filter_species <- function(data, spp_list, join = FALSE) {

  spp_filtered <- dplyr::filter(data$DT, Species %in% spp_list)
  plots_w_spp <- dplyr::distinct(spp_filtered, PlotObservationID)

  header_filtered <- dplyr::inner_join(data$header, plots_w_spp)
  DT_filtered <- dplyr::semi_join(data$DT, header_filtered)

  if(isTRUE(join)) {
    return(dplyr::inner_join(DT_filtered, header_filtered))
  } else {
    return(list(DT = DT_filtered, header = header_filtered))
  }
}


#'Filter sPlotOpen Data by Spatial Polygon
#'
#'@inheritParams filter_species
#'@param x A simple features object, path to a shapefile, or matrix with points
#'  (longitude, latitude) in rows.
#'
#'@return sPlotOpen data filtered to include only plots that intersect with `x`.
#'  If `join = F`, a list containing the filtered `DT` and `header` tables. If
#'  `join = T`, a single data from containing the joined tables.
#'@export
#'
#' @examples
#' data(greece)
#'
#'# create matrix of coordinates defining a polygon
#' points <- matrix(c(24.4, 41.5, 24.8, 41.5, 24.8, 41.3, 24.4, 41.3, 24.4, 41.5),
#'                  ncol = 2, byrow = TRUE)
#'
#' # filter dataset to only include points within the polygon
#' filter_polygon(greece, points, join = FALSE)
filter_polygon <- function(data, x, join = FALSE) {

  plots <- sf::st_as_sf(data$header, coords = c('Longitude', 'Latitude'),
                        crs = sf::st_crs(4326))

  if(is.character(x)) {
    x <- sf::st_read(x)
  }

  if(is.matrix(x)) {
    x <- sf::st_polygon(list(x))
    x <- sf::st_sfc(x, crs = 4326)
  }

  if(sf::st_crs(x) != sf::st_crs(plots)) {
    x <- sf::st_transform(x, sf::st_crs(plots))
  }

  int <- sf::st_intersection(plots, x)
  coords <- sf::st_coordinates(int)

  header_filtered <- sf::st_drop_geometry(int)
  header_filtered$Longitude <- coords[,1]
  header_filtered$Latitude <- coords[,2]

  DT_filtered <- dplyr::semi_join(data$DT, header_filtered)

  if(isTRUE(join)) {
    return(dplyr::inner_join(DT_filtered, header_filtered))
  } else {
    return(list(DT = DT_filtered, header = header_filtered))
  }
}

