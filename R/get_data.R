#' Download sPlotOpen data and load into R
#'
#' @param dir character; Directory where sPlotOpen data will be saved after download. If NULL, data will not be saved on disk and only loaded into the R environment.
#' @param load logical; Should the data be loaded immediately into R?
#'
#' @return If `load = TRUE`, returns a list containing sPlotOpen data tables.
#' @export
#'
#' @examples
get_sPlot <- function(dir = NULL, load = TRUE) {

  options(timeout = 3600)

  # create directory
  if (!is.null(dir)) {
    if (stringr::str_sub(dir, -1) != "/") {
      dir <- paste(dir, "/", sep = "")
    }
    if (!dir.exists(dir)) {
      dir.create(dir)
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
read_sPlot <- function(dir) {
  data <- list(comp = readr::read_tsv(paste0(dir, "/sPlotOpen_DT(1).txt")),
               plots = readr::read_tsv(paste0(dir, "/sPlotOpen_header(2).txt")),
               traits = readr::read_tsv(paste0(dir, "/sPlotOpen_CWM_CWV(1).txt")))
  return(data)
}

