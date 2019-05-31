#' get_life_time
#' @description 计算生命周期：通过新用户环比系数及老用户环比系数计算各自的生命周期。
#' @param retain_users_old_daily_true: 需要计算的老用户数。
#' @param ring_retain_new: 新用户的各段环比参数，可通过get_ring_retain()随机获得。
#' @param ring_retain_old: 老用户的环比参数，可通过get_ring_retain()随机获得。
#' @param prediction_retain_one: 计算生命周期时，预估未来新增的次日留存。
#' @param life_time_year: 计算生命周期的时段(按年统计)。
#' @return 生命周期的列表(life_time)，包含新用户生命周期(new)，老用户生命周期(old)，每日留存(ring_retain_new_rates)
#' @examples
#' get_life_time(10, 7, 30)
#' @export
#' @import purrr
#'
# 根据环比留存及预计的新增次留计算生命周期
get_life_time <- function(retain_users_old_daily_true,
                          ring_retain_new,
                          prediction_retain_one,
                          ring_retain_old,
                          life_time_year){

  # 得到各日相对于首日的环比留存率
  if(is.character(ring_retain_new)) {
    ring_retain_new <- stringr::str_split(ring_retain_new, ",")[[1]]
  }

  # 每个环比参数影响的天数
  rep_days <- c(2, 4, 7, 16, 30, 120, 180, life_time_year*365 - 360)

  # 得到各日相对于首日的环比留存率
  ring_retain_new_rates <- map2(ring_retain_new, rep_days, rep) %>%
    unlist %>%
    cumprod

  # 如果 retain_users_old_daily_true 为 0,则不计算,否则累乘老用户留存
  if (retain_users_old_daily_true == 0) {
    ring_retain_old_rates <- rep(0, life_time_year*365 - 1)
  } else {
    ring_retain_old_rates <- rep(ring_retain_old, life_time_year*365 - 1) %>%
      cumprod
  }

  life_time <- list()
  life_time$new <- sum(ring_retain_new_rates * prediction_retain_one) + prediction_retain_one + 1  # 新用户LT(3日留存起 算 sum,再加首日次日留存)
  life_time$old <- if_else(retain_users_old_daily_true == 0, 0, sum(ring_retain_old_rates) + 1) # 老用户LT
  life_time$ring_retain_new_rates <- c(1, ring_retain_new_rates) * prediction_retain_one  # 预测留存
  return(life_time)
}

# days 计算全部生命周期
# get_life_time <- function(ring_retain_new = c(0.8480, 0.9138, 0.9525, 0.9679, 0.9801, 0.9861, 0.99, 0.99, 0.98), # 新用户环比系数
#                           ring_retain_old = 0.98, # 老用户环比系数
#                           prediction_retain_one = 0.48  # 需要预测的新用户次留(计算总留存天数)
# ){
#   # 每个环比参数影响的天数
#   rep_days <- c(2, 4, 7, 16, 30, 120, 180, days - 360)
#
#   # 得到各日相对于首日的环比留存率
#   ring_retain_new_rates <- map2(ring_retain_new, rep_days, rep) %>%
#     unlist %>%
#     cumprod
#   life_time <- list()
#
#   life_time$new <- sum(ring_retain_new_rates * prediction_retain_one) + prediction_retain_one + 1
#   life_time$old <- sum(ring_retain_old_rates) + 1
#   return(life_time)
#
# }
