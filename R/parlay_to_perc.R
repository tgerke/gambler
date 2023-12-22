#' Convert parlay to win percentage
#'
#' @param ... Money lines
#'
#' @return A single percentage representing the win percentage
#' @export
#'
#' @examples
parlay_to_perc <- function(...) {
  lines <- list(...)
  Reduce(`*`, purrr::map(lines, line_to_perc))
}
