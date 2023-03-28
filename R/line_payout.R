#' Calculate payout for a given line with a wager
#'
#' @param line Money line
#' @param wager Wager
#'
#' @return Total payout (wager + winnings)
#' @export
#'
#' @examples
line_payout <- function(line, wager) {
  if (line < 0) {
    return(
      wager*100/-line + wager
    )
  }
  if (line >= 0) {
    return(
      wager*line/100 + wager
    )
  }
}
