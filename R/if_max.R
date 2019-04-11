
#' get_prediction_daily
#' @description get_prediction_daily
#' @param days: days
#' @param n: n
#' @return n or 0
#' @examples
#' if_max(days = 100, n = 10)
#' @import dplyr
#'
if_max <- function(days, n){
  if_else(days >= n & n > 0, n, 0)
}
