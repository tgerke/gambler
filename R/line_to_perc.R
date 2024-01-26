#' Convert money line into a decimal win percentage
#'
#' @param line Money line
#'
#' @return Win percentage as a decimal
#' @export
#'
#' @examples
#' lines <- c(100, -100, 150)
#' line_to_perc(lines)
line_to_perc <- function(line) {
  purrr::map_dbl(
    line,
    ~ {
      if (.x < 0) {
        return(
          .x/(.x-100)
        )
      }
      if (.x >=0) {
        return(
          100/(.x+100)
        )
      }
    }
  )
}
