#' get_retain_users_n_days
#' @description get_retain_users_n_days
#' @param retain_users_detail: retain_users_detail
#' @param total_days: total_days
#' @param start_n: start_n
#' @param end_n: end_n
#' @return df
#' @examples
#' # get_retain_users_n_days(retain_users_detail = retain_users_detail, 10, 7, 30)
#' @import purrr
#' @import dplyr
#'
# 取得活跃中各历史新增日留存的占比
get_retain_users_n_days <- function(retain_users_detail, total_days, start_n, end_n) {
  retain_dates_index <- 1:total_days
  map_dbl(retain_dates_index, ~c(if_else(.x-end_n > 0, sum(retain_users_detail[ifelse(.x-end_n > 0, .x-end_n, 0):ifelse(.x-start_n > 0, .x-start_n, 0), .x]), 0)))
}
