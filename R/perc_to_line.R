#' Convert decimal percentage into a money line
#'
#' @param perc Win percentage as a decimal
#'
#' @return Money line
#' @export
#'
#' @examples
perc_to_line <- function(perc) {
  if (perc >= .5) {
    return(
      -100*perc/(1-perc)
    )
  }
  if (perc < .5) {
    return(
      100*(1-perc)/perc
    )
  }
}
