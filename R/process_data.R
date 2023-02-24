#' Create site-by-species matrix
#'
#' @param data A data frame containing site names in the first column, species names in the second column, and, optionally, abundance data in the third column.
#' @param sparse Whether to return a sparse matrix.
#'
#' @return If `sparse = TRUE` a sparse matrix, otherwise a matrix.
#' @export
#'
#' @examples
site_species <- function(data, sparse = TRUE) {
  sites <- factor(data[[1]])
  species <- factor(data[[2]])
  if(ncol(data)==3) {
    x <- data[[3]]
  } else {
    x <- 1
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
