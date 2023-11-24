#' Map sPlotOpen Plots
#'
#' `map_plots()` plots the locations of sPlotOpen Vegetation plots on a world
#' map.
#'
#' @param data Either a list of sPlotOpen tables that includes the `header`
#'   table, or the `header` table on its own as a data.frame.
#' @param type If `grid` (the default), plots the number of vegetation plots by
#'   grid cell. If `points`, plots the locations of individual vegetation plots.
#' @param grid_size The approximate spacing between grid cells (in km) if using
#'   `type = grid`.
#' @param extent The extent of the returned map. Can be `world` or `aoi`, if
#'   the map should be zoomed to the area of interest.
#'
#'
#' @return A map showing the locations of plots.
#' @export
#'
#' @examples
#' \dontrun{
#' data(greece)
#' map_plots(greece, grid_size=100, extent="aoi")
#' }
map_plots <- function(data, type = "grid", grid_size = 300, extent="world") {

  if(class(data)[1]=="list") data <- data$header

  plots <- sf::st_as_sf(data,
                        coords = c("Longitude", "Latitude"),
                        crs = 4326) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = "+proj=eck4")

  cpath <- system.file("extdata", "ne_countries.shp", package = "sPlotOpenR")
  countries <- sf::st_read(cpath)

  bpath <- system.file("extdata", "ne_bb.shp", package = "sPlotOpenR")
  bb <- sf::st_read(bpath)


  base <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = bb, col = "grey20", fill = "white") +
    ggplot2::geom_sf(data = countries, fill = "grey90", lwd = 0.3) +
    ggplot2::coord_sf(crs = "+proj=eck4") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   legend.title = ggplot2::element_text(size=12),
                   legend.text = ggplot2::element_text(size=12),
                   legend.background = ggplot2::element_rect(linewidth=0.1, linetype="solid", colour = 1),
                   legend.key.height = ggplot2::unit(1.1, "cm"),
                   legend.key.width = ggplot2::unit(1.1, "cm"))


  if(type == "grid") {

    dggs <- dggridR::dgconstruct(spacing = grid_size, metric = T, resround = 'nearest', show_info = FALSE)

    #Get the corresponding grid cells for each plot
    data$cell <- dggridR::dgGEO_to_SEQNUM(dggs, data$Longitude, data$Latitude)$seqnum

    #Calculate number of plots for each cell
    header_dggs <- data %>%
      dplyr::group_by(cell) %>%
      dplyr::summarize(value = log10(dplyr::n()))

    #Get the grid cell boundaries for cells
    grid <- dggridR::dgcellstogrid(dggs, header_dggs$cell) %>%
      sf::st_as_sf() %>%
      dplyr::mutate(cell = header_dggs$cell) %>%
      dplyr::mutate(value = header_dggs$value) %>%
      sf::st_transform("+proj=eck4") %>%
      sf::st_wrap_dateline(options = c("WRAPDATELINE=YES"))

    brk <- 0:max(ceiling(grid$value))
    lbl <- c("1", "10", "100", "1,000", "10,000")[1:length(brk)]

    ## Prepare plotting
    map_out <- base +
      ggplot2::geom_sf(data = grid, ggplot2::aes(fill = value), color = NA, alpha=0.9)    +
      viridis::scale_fill_viridis(breaks = brk, labels = lbl) +
      ggplot2::labs(fill = "# plots")

    } else if(type == "points") {

    map_out <- base +
      ggplot2::geom_sf(data = plots, size=1, alpha=0.8, color = "forestgreen") +
        ggplot2::theme(legend.position = "none")

    } else stop('type must be one of "grid" or "points"')

  if(extent == "aoi"){
    plots_bbox <- sf::st_bbox(plots)

    ## plotting
    map_out +
      ggplot2::coord_sf(crs = "+proj=eck4",
                        xlim = c(plots_bbox[1] - 1000000,
                                 plots_bbox[3] + 1000000),
                        ylim = c(plots_bbox[2] - 1000000,
                                 plots_bbox[4] + 1000000),
                        expand = FALSE)
  } else if(extent == "world") {
    map_out
  } else stop('extent must be one of "world" or "aoi"')
}



#'Map Species Occurrences
#'
#'`map_species()` plots a world map showing locations of sPlotOpen plots that
#'contain the species you specify.
#'
#'@inheritParams filter_species
#'@inheritParams map_plots
#'@param species Species name.
#' @param extent The extent of the returned map. Can be `world` or `aoi`, if
#'   the map should be zoomed to the area of interest.
#'@param resolve If `TRUE` (not the default), resolves species names using
#'   TNRS.
#'
#'@return A map of speices occurrences.
#'@export
#'
#' @examples
#' data(greece)
#' map_species(greece, species = "Fagus sylvatica", extent = "aoi")
map_species <- function(data, species, extent="world", resolve = FALSE) {
  data <- filter_species(data, species, resolve)
  if(nrow(data$DT) == 0) stop("Species not found")
  if(length(species)==1){
    mytitle <- species
  } else {mytitle <- ("Selected species")}
  map_plots(data, type = "points", extent = extent) +
    ggplot2::labs(title = mytitle, font.face = "italic") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "italic"))
}

