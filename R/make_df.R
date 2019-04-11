#' make_df
#' @description make_df
#' @param file_name: file_name
#' @param train_cr: train_cr
#' @param diff_days: diff_days
#' @param diff_base: diff_base
#' @return df_list
#' @examples
#' make_df()
#' @export
#' @import stringr
#' @import dplyr
#'
make_df <- function(file_name = "Data/info.csv", train_cr = 0.7, diff_days = 30, diff_base = 1.02) {

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


