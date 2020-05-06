#' Make Quintile
#'
#' `make_quintile` is a helper function for working with Scottish Index of Multiple Deprivation (SIMD) data. The Scottish Government released data provides domain ranks for each datazone but does not allocate these to quintiles. This function makes it easy to do this.
#'
#' @param rnk A numerical vector of information with values between 1 and 6976 (usually a column in a dataset)
#' @return Returns the value of \code{arg1}
#' @examples
#'
#' myfunction(1) # returns 1
#'
#' @export

make_quintile <- function(rank) {

  if (!(rank %in% 1:6976)) {
    stop("There are values outside the possible number of Scottish datazones")
  }

  if(!(is.numeric(rank)) | !(is.integer(rank)) ~ !(is.double(rank))) {
    stop("The rank variable must be of type double, numerical or integer")
  }

  if (anyDuplicated(rank) == TRUE) {
    warning("There are duplicated values in the ranking, check the contributing values")
  }

  x <- rlang::exprs(
    rank %in% c(1:1395) ~ "1",
    rank %in% c(1396:2790) ~ "2",
    rank %in% c(2791:4185) ~ "3",
    rank %in% c(4186:5580) ~ "4",
    rank %in% c(5581:6976) ~ "5")

  y <- dplyr::case_when(!!! x)

  return(y)

}
