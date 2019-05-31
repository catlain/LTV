#' get_retain_users_n_days
#' @description 分析新老各段新增用户的留存贡献：已知每日新增及留存曲线后，按新增日与活跃日的间隔计算距计算日新老程度不同的用户群对计算日DAU的贡献，
#' @param retain_users_detail: 各日新增在每日的留存明细，通过实际新增和每日留存曲线计算得到。
#' @param total_days: 需要计算DAU的总天数，与真实新增>0的天数相同。
#' @param start_n: 活跃日距离计算日的最小天数
#' @param end_n: 活跃日距离计算日的最大天数
#' @return 数字向量
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
