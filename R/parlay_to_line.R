#' Title
#'
#' @param ... Money lines
#'
#' @return Money line for the combined bets assuming independence
#' @export
#'
#' @examples
parlay_to_line <- function(...) {
  lines <- list(...)
  Reduce(`*`, purrr::map(lines, line_to_perc)) |>  perc_to_line()
}
