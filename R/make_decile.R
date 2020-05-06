#' Make Decile
#'
#' `make_decile` is a helper function for working with Scottish Index of Multiple Deprivation (SIMD) data. The Scottish Government released data provides domain ranks for each datazone but does not allocate these to deciles. This function makes it easy to do this.
#'
#' @param rnk A numerical vector of information with values between 1 and 6976 (usually a column in a dataset)
#' @return Returns the value of \code{arg1}
#' @examples
#'
#' myfunction(1) # returns 1
#'
#' @export

make_decile <- function(rank) {

  if (!(rank %in% 1:6976)) {
    stop("There are values outside the possible number of Scottish datazones")
  }

  if(!(is.numeric(rank)) | !(is.integer(rank)) ~ !(is.double(rank))) {
    stop("The rank variable must be of type double, numerical or integer")
  }

x <- rlang::exprs(
    rank %in% c(1:697) ~ "1",
    rank %in% c(698:1395) ~ "2",
    rank %in% c(1396:2092) ~ "3",
    rank %in% c(2093:2790) ~ "4",
    rank %in% c(2791:3488) ~ "5",
    rank %in% c(3489:4185) ~ "6",
    rank %in% c(4186:4883) ~ "7",
    rank %in% c(4884:5580) ~ "8",
    rank %in% c(5581:6278) ~ "9",
    rank %in% c(6279:6976) ~ "10")

  y <- dplyr::case_when(!!! x)

  return(y)

}
