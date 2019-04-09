#' get_prediction_daily
#' @description get_prediction_daily
#' @param file_name: file_name
#' @param train: train
#' @return df_list
#' @examples
#' make_df()
#' @export


make_df <- function(file_name = "Data/info.csv", train_cr = 0.7, diff_cr = 0.25, diff_base = 1.02) {

  if (train_cr < 0 | train_cr > 1) {
    message("训练集的比例(train_cr)应该在[0,1]间")
    return()
  }

  if (diff_cr < 0 | diff_cr > 1) {
    message("计算差异的日期比例(diff_cr)应该在[0,1]间")
    return()
  }


  df_list <- list()

  info_df <- read.csv(file_name, stringsAsFactors = FALSE) %>%
    unique() %>%
    filter(DNU > 0, !stringr::str_detect(retain_rate_1, "-")) %>%
    mutate(retain_rate_1 = as.numeric(retain_rate_1),
           Date = as.Date(Date))

  # 一共有多少天已有数据
  days <- filter(info_df, DAU > 0) %>%
    nrow()

  train_days <- round(days * train_cr)
  df_list$train_days <- train_days
  df_list$diff_days <- round(train_days * diff_cr) #  需要比较的近期天数
  df_list$diff_base <- diff_base # 近期差异加权系数(乘方)
  df_list$train_df <- info_df[1 : train_days, ]
  df_list$test_df <- info_df

  class(df_list) <- "df_list"
  return(df_list)
}


