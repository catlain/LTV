#' 判断数字大小
#' @description 用于判断循环参数是否属于某个系数段内。
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
