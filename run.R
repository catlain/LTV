require(LTV)
require(parallel)
require(doParallel)
require(dplyr)
require(purrr)
require(tidyr)
no_cores <- detectCores() - 1

# 有参数的情况下修改参数 --------------------------------------------------------------------

ring_retain_new_parameter <- c(0.922,0.92,0.948,0.9679,0.9801,0.9861,0.99,0.99)
ring_retain_new_old <- 0.97


# 输出修改参数后的结果 --------------------------------------------------------------
df_list <- make_df(file_name = "Data/word_game_info_base_10.csv", train_cr = )

best_retain <- get_prediction_daily(df_list = df_list, # 以训练集拟合参数
                                    type = "test", # 以训练集拟合参数
                                    diff_days = 20, # 需要比较的近期天数
                                    diff_base = 1.02, # 近期差异加权系数（乘方）
                                    ring_retain_new = ring_retain_new_parameter,
                                    ring_retain_old = ring_retain_new_old,
                                    csv = FALSE,
                                    plot = TRUE,
                                    message = FALSE,
                                    smooth = FALSE)
best_retain

# 暴力猜参数 -------------------------------------------------------------------
df_list <- make_df(file_name = "Data/word_game_info_base_10.csv", train_cr = 1, diff_cr = 0.25)

cl <- makeCluster(no_cores)
registerDoParallel(cl)

start_time <- Sys.time()
res <- foreach(i = 1:10000,
        .combine = rbind,
        .packages = c("purrr",
                      "dplyr",
                      "stringr",
                      "forecast",
                      "LTV"
                      )) %dopar% {
  res <- get_ring_retain(start = 0.3, max = 0.99999)
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
  head(100) %>%
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

# # 从所有尝试中得到差异最小1组
# violence_best_retain <- res[res$diff == min(res$diff), ]
# violence_best_retain


# 使用最好的一组,设定需要预测天数的次留,并画图 -------------------------------------------------

# info_df <- read.csv("info.csv")
get_prediction_daily(df_list = df_list,
                     type = "test", # 以训练集拟合参数
                     #####################################################################
                     prediction_retain_one = 0.48,  # 需要预测的新用户次留(计算总留存天数)
                     life_time_year = 1, # 预测生命周期年数
                     #####################################################################
                     ring_retain_new = violence_best_retain$ring_retain_new,
                     ring_retain_old = violence_best_retain$ring_retain_old,
                     csv = FALSE,  # 是否输出 prediction.csv
                     plot = TRUE,  # 是否作图
                     message = TRUE, # 是否打印信息
                     smooth = FALSE
                     )

violence_best_retain$ring_retain_new # 新用户环比参数
violence_best_retain$ring_retain_old # 老用户环比参数
