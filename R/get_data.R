#' Download sPlotOpen data and load into R
#'
#' `get_sPlot()` downloads sPlotOpen data from the iDiv Data Repository and
#' saves the downloaded tables to a local directory and/or loads them into R.
#'
#' @param dir Directory where sPlotOpen data will be saved after download. If
#'   `NULL`, data will not be saved on disk and only loaded into the R
#'   environment.
#' @param load If `TRUE` (the default), data will be loaded immediately into R.
#' @param tables A character vector. Names of tables to be downloaded. Options
#'   are (default is to download all):
#'  * `"plots"`: plot-level information.
#'  * `"comp"`: data on species composition of each plot in long format.
#'  * `"traits"`: community-weighted means and variances for 18 traits.
#' @param metadata If `TRUE` (the default), metadata will be downloaded.
#'
#' @return If `load = TRUE`, returns a named list containing the downloaded
#'   tables, each as a [`tibble()`].
#' @export
#'
#' @examples
get_sPlot <- function(dir = "~/sPlotOpen/data",
                      tables = c("plots", "comp", "traits"),
                      metadata = TRUE,
                      load = TRUE) {

  options(timeout = 3600)

  # create directory
  if (!is.null(dir)) {
    if (stringr::str_sub(dir, -1) != "/") {
      dir <- paste(dir, "/", sep = "")
    }
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = T)
      message(paste("Creating directory:", dir))
    }
    message(paste0("Saving to ", dir))
  }

  # give error message if no directory is specified and load = FALSE
  if (is.null(dir) & load == FALSE) {
    stop("Must specify a directory (\"dir\") to save data when \"load = FALSE\".")
  }

  # download zipped sPlotOpen data to temporary file
  temp <- tempfile()
  url <- "https://idata.idiv.de/ddm/Data/DownloadZip/3474?version=5047"
  download.file(url, temp, mode = "wb")

  if (!is.null(dir)) {

    # extract to directory
    unzip(temp, exdir = stringr::str_sub(dir, 1, -2))
    unlink(temp)

    # load data
    if(load) {
      data <- list(plots = readr::read_tsv(paste0(dir, "sPlotOpen_DT(1).txt")),
                   comp = readr::read_tsv(paste0(dir, "sPlotOpen_header(2).txt")),
                   traits = readr::read_tsv(paste0(dir, "sPlotOpen_CWM_CWV(1).txt")))
      return(data)
    }

  } else {

    # unzip to temporary directory
    tempDir <- tempdir()
    unzip(temp, exdir = tempDir)
    unlink(temp)

    # load data
    data <- list(comp = readr::read_tsv(paste0(tempDir, "/sPlotOpen_DT(1).txt")),
                 plots = readr::read_tsv(paste0(tempDir, "/sPlotOpen_header(2).txt")),
                 traits = readr::read_tsv(paste0(tempDir, "/sPlotOpen_CWM_CWV(1).txt")))
    return(data)

    # delete temporary directory
    unlink(tempDir, recursive = T)
  }
}


#' Load sPlotOpen data into R
#'
#' @param dir Directory where sPlotOpen tables are stored.
#'
#' @return List containing sPlotOpen data tables.
#' @export
#'
#' @examples
read_sPlot <- function(dir = "~/sPlotOpen/data") {
  data <- list(comp = readr::read_tsv(paste0(dir, "/sPlotOpen_DT(1).txt")),
               plots = readr::read_tsv(paste0(dir, "/sPlotOpen_header(2).txt")),
               traits = readr::read_tsv(paste0(dir, "/sPlotOpen_CWM_CWV(1).txt")))
  return(data)
}

