#' Convert money line into a decimal win percentage
#'
#' @param line Money line
#'
#' @return Win percentage as a decimal
#' @export
#'
#' @examples
line_to_perc <- function(line) {
  if (line < 0) {
    return(
      line/(line-100)
    )
  }
  if (line >=0) {
    return(
      100/(line+100)
    )
  }
}
