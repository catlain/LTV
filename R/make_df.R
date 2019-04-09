#' get_prediction_daily
#' @description get_prediction_daily
#' @param file_name: file_name
#' @param train: train
#' @return df_list
#' @examples
#' make_df()
#' @export


make_df <- function(file_name = "Data/info.csv", train_cr = 0.7) {

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
  df_list$train_df <- info_df[1 : train_days, ]
  df_list$test_df <- info_df

  return(df_list)
}


