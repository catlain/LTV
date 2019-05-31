#' 制作计算所需数据集
#' @description 从csv文件中得到训练/测试数据集，差异天数，差异加权等。
#' @param file_name: 用来拟合的文件，包含真实DAU、DNU、次日留存等。默认 "Data/info.csv"
#' @param train_cr: 全部真实数据中用来做为训练的比例。默认0.8
#' @param diff_days: 用于计算拟合差异的天数，按截止日倒推，对越近期的拟合成绩越看重。默认30
#' @param diff_base: 计算差异时，每增加1天，差异(r)按照diff_base加权(r^diff_base)计为当日差异，最终按全部差异的中位数得到总差异。默认1.02
#' @return 提供给get_prediction_daily()的参数列表(df_list)，包含拟合数据集(train_df)，测试数据集(test_df)，基于真实数据拟合的天数(train_days)，用于计算拟合差异的天数(diff_days)，计算差异的加权(diff_base)
#' @examples
#' make_df()
#' @export
#' @import stringr
#' @import dplyr
#'
make_df <- function(file_name = "Data/info.csv",
                    train_cr = 0.7,
                    diff_days = 30,
                    diff_base = 1.02) {

  if (train_cr < 0 | train_cr > 1) {
    message("训练集的比例(train_cr)应该在[0,1]间")
    return()
  }

  if (diff_days < 1) {
    message("计算差异的日期应该为正整数")
    return()
  }

  diff_days <- as.integer(diff_days)

  df_list <- list()

  info_df <- read.csv(file_name, stringsAsFactors = FALSE) %>%
    unique() %>%
    filter(DNU > 0, !str_detect(retain_rate_1, "-")) %>%
    mutate(retain_rate_1 = as.numeric(retain_rate_1),
           Date = as.Date(Date))

  # 一共有多少天已有数据
  days <- filter(info_df, DAU > 0) %>%
    nrow()

  df_list$train_days <- as.integer(days * train_cr)
  df_list$diff_days <- if_else(diff_days <= df_list$train_days, diff_days, df_list$train_days) #  需要比较差异的近期天数,大于train_days则使用全部差异
  df_list$diff_base <- diff_base # 近期差异加权系数(乘方)
  df_list$train_df <- info_df[1 : df_list$train_days, ]
  df_list$test_df <- info_df

  class(df_list) <- "df_list"
  return(df_list)
}


