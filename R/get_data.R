get_sPlot <- function(dir = NULL, load = TRUE) {

  options(timeout = 3600)

  # create directory
  if (!is.null(dir)) {
    if (str_sub(dir, -1) != "/") {
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
    unzip(temp, exdir = str_sub(dir, 1, -2))
    unlink(temp)

    # load data
    if(load) {
      data <- list(plots = read.delim(paste0(dir, "sPlotOpen_DT(1).txt")),
                   comp = read.delim(paste0(dir, "sPlotOpen_header(2).txt")),
                   traits = read.delim(paste0(dir, "sPlotOpen_CWM_CWV(1).txt")))
      return(data)
    }

  } else {

    # unzip to temporary directory
    tempDir <- tempdir()
    unzip(temp, exdir = tempDir)
    unlink(temp)

    # load data
    data <- list(plots = read.delim(paste0(tempDir, "/sPlotOpen_DT(1).txt")),
                 comp = read.delim(paste0(tempDir, "/sPlotOpen_header(2).txt")),
                 traits = read.delim(paste0(tempDir, "/sPlotOpen_CWM_CWV(1).txt")))
    return(data)

    # delete temporary directory
    unlink(tempDir, recursive = T)
  }
}


read_sPlot <- function(dir) {
  data <- list(plots = read.delim(paste0(dir, "sPlotOpen_DT(1).txt")),
               comp = read.delim(paste0(dir, "sPlotOpen_header(2).txt")),
               traits = read.delim(paste0(dir, "sPlotOpen_CWM_CWV(1).txt")))
  return(data)
}

