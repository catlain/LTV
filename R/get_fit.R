#' get_prediction_daily
#' @description get_prediction_daily
#' @param no_cores: no_cores
#' @param file_name: file_name
#' @param n: n
#' @param train_cr: train_cr
#' @param diff_cr: diff_cr
#' @return
#' @examples
#' get_fit()
#' @export

get_fit <- function(no_cores,
                    file_name = "Data/info.csv",
                    n = 50000,
                    train_cr = 0.8,
                    diff_cr = 0.5,
                    start = 0.3,
                    max = 0.99999) {

  df_list <- make_df(file_name = file_name, train_cr = train_cr, diff_cr = diff_cr)

  cl <- makeCluster(no_cores)
  registerDoParallel(cl)

  start_time <- Sys.time()
  res <- foreach(i = seq_len(n),
                 .combine = rbind,
                 .packages = c("purrr",
                               "dplyr",
                               "stringr",
                               "forecast",
                               "LTV"
                 )) %dopar% {
                   res <- get_ring_retain(start = start, max = max)
                   tryCatch({
                     get_prediction_daily(df_list = df_list, # 数据集
                                          type = "train", # 以训练集拟合参数
                                          ring_retain_new = res$ring_retain_new,
                                          ring_retain_old = res$ring_retain_old,
                                          csv = FALSE,
                                          plot = FALSE,
                                          message = FALSE,
                                          smooth = FALSE
                     )
                   }, error = function(e) e)
                 }
  end_time <- Sys.time()
  message("总花费时长: ", round(as.numeric(end_time) - as.numeric(start_time), 3), "秒")

  stopCluster(cl)

  # 从所有尝试中得到差异最小的100组,取中位数
  top_res <- arrange(res, diff) %>%
    head(10) %>%
    separate(ring_retain_new, sep = ",", paste0("retain_new", c(2, 4, 7, 16, 30, 120, 180, 360)), convert = TRUE) %>%
    select(contains("retain_new"), ring_retain_old)

  # par(mfrow = c(3, 3))
  # plot_map <- map(paste0("retain_new", c(2, 4, 7, 16, 30, 120, 180, 360)), ~hist(top_res[[.x]], breaks = 20))

  best_res <- summarise_all(top_res, mean)

  violence_best_retain <- list()
  violence_best_retain$ring_retain_new <- select(best_res, contains("retain_new")) %>%
    `[`(1,) %>%
    paste0(collapse = ",")

  violence_best_retain$ring_retain_old <- best_res$ring_retain_old

  # 使用最好的一组,设定需要预测天数的次留,并画图 -------------------------------------------------

  get_prediction_daily(df_list = df_list,
                       type = "test", # 以训练集拟合参数
                       #####################################################################
                       prediction_retain_one = 0.33,  # 需要预测的新用户次留(计算总留存天数)
                       life_time_year = 1, # 预测生命周期年数
                       #####################################################################
                       ring_retain_new = violence_best_retain$ring_retain_new,
                       ring_retain_old = violence_best_retain$ring_retain_old,
                       csv = FALSE,  # 是否输出 prediction.csv
                       plot = TRUE,  # 是否作图
                       message = TRUE, # 是否打印信息
                       smooth = FALSE
  )
  return(violence_best_retain)
}
