#' 重复随机试验计算最优参数
#' @description 通过重复猜测环比参数，固定测试时间窗口的方式逐步计算测试日期内的预测结果与实际DAU的差异，得到最优的参数组合。
#' @param no_cores: 并行任务时，可用的cpu核数，通过 parallel::detectCores 取得目前总核心数 -1 得到。
#' @param file_name: 用来拟合的文件，包含真实DAU、DNU、次日留存等。默认 "Data/info.csv"
#' @param times: 暴力预测次数，默认50000
#' @param train_cr: 全部真实数据中用来做为训练的比例。默认0.8
#' @param diff_days: 用于计算拟合差异的天数，从训练日最后一日倒推，对越近期的拟合成绩越看重。默认30
#' @param start: 随机获得参数时，新用户留存参数的下限，即第一个参数--第3、4日对次日的环比系数--的下限。默认0.3
#' @param max: 随机获得参数时，参数的上限，包括360日及以上新用户留存和老用户留存。默认0.9999
#' @param smooth: 是否使用时序分析包(forecast)获取趋势，目前只支持排除7日(周)影响。默认FALSE
#' @param csv: 是否输出结果到csv文件，分别为预测结果(prediction_*.csv)和参数(parameter_*.csv)。默认FALSE
#' @return 无返回值
#' @examples
#' get_fit()
#' @export
#' @import parallel
#' @import doParallel
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import forecast
#'
get_fit <- function(no_cores,
                    file_name = "Data/info.csv",
                    times = 50000,
                    train_cr = 0.8,
                    diff_days = 30,
                    start = 0.3,
                    prediction_retain_one = 0.33,
                    max = 0.99999,
                    smooth = FALSE,
                    csv = FALSE
) {
  df_list <- make_df(file_name = file_name, train_cr = train_cr, diff_days = diff_days)

  cl <- makeCluster(no_cores)
  registerDoParallel(cl)

  start_time <- Sys.time()
  res <- foreach(i = seq_len(times),
                 .combine = rbind,
                 .packages = c("purrr",
                               "dplyr",
                               "stringr",
                               "forecast",
                               "magrittr",
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
                                          smooth = smooth
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
                       prediction_retain_one = prediction_retain_one,  # 需要预测的新用户次留(计算总留存天数)
                       life_time_year = 1, # 预测生命周期年数
                       #####################################################################
                       ring_retain_new = violence_best_retain$ring_retain_new,
                       ring_retain_old = violence_best_retain$ring_retain_old,
                       csv = csv,  # 是否输出 prediction.csv
                       plot = TRUE,  # 是否作图
                       message = TRUE, # 是否打印信息
                       smooth = smooth
  )
  return(violence_best_retain)
}
