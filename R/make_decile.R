#' Make Decile
#'
#' `make_decile` is a helper function for working with Scottish Index of Multiple Deprivation (SIMD) data. The Scottish Government released data provides domain ranks for each datazone but does not allocate these to deciles. This function makes it easy to do this.
#'
#' @param rank A numerical vector of information with values between 1 and 6976 (usually a column in a dataset)
#' @return Returns the value of \code{arg1}
#' @examples
#'
#' myfunction(1) # returns 1
#'
#' @export

make_decile <- function(rank) {

x <- rlang::exprs(
    rank >= 1 & rank <= 697      ~ "1",
    rank >= 698 & rank <= 1395   ~ "2",
    rank >= 1396 & rank <= 2092  ~ "3",
    rank >= 2093 & rank <= 2790  ~ "4",
    rank >= 2791 & rank <= 3488  ~ "5",
    rank >= 3489 & rank <= 4185  ~ "6",
    rank >= 4186 & rank <= 4883  ~ "7",
    rank >= 4884 & rank <= 5580  ~ "8",
    rank >= 5581 & rank <= 6278  ~ "9",
    rank >= 6279 & rank <= 6976  ~ "10")

    y <- dplyr::case_when(!!! x)

    return(y)

}
