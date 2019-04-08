#' get_prediction_daily
#' @description get_prediction_daily
#' @param diff_days: diff_days
#' @param diff_base: diff_base
#' @param file_name: file_name
#' @param ring_retain_new: ring_retain_new
#' @param ring_retain_old: ring_retain_old
#' @param prediction_retain_one: prediction_retain_one
#' @param life_time_year: life_time_year
#' @param csv: csv
#' @param plot: plot
#' @param message: message
#' @param smooth: smooth
#' @return df
#' @examples
#' get_prediction_daily()
#' @export

get_prediction_daily <- function(diff_days = 90, #  需要比较的近期天数
                                 diff_base = 1.02, # 近期差异加权系数(乘方)
                                 file_name = "Data/info.csv", # 输入文件名
                                 ring_retain_new = c(0.8480, 0.9138, 0.9525, 0.9679, 0.9801, 0.9861, 0.99, 0.99), # 新用户环比系数
                                 ring_retain_old = 0.98, # 老用户环比系数
                                 prediction_retain_one = 0.48, # 需要预测的新用户次留(计算总留存天数)
                                 life_time_year = 1, # 预测生命周期年数
                                 csv = FALSE, # 是否输出 prediction.csv
                                 plot = TRUE, # 是否作图
                                 message = FALSE, # 是否打印信息
                                 smooth = FALSE # 是否先做时序分析取趋势
) {

  # 读取文件

  info_df <- read.csv(file_name, stringsAsFactors = FALSE) %>%
    unique() %>%
    filter(DNU > 0, !str_detect(retain_rate_1, "-")) %>%
    mutate(retain_rate_1 = as.numeric(retain_rate_1),
           Date = as.Date(Date))

  run_time <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S") # 生成存储文件名所需
  days <- nrow(info_df)  # 文件一共有多少天(包括已有数据日及需预测日)

  # 对波动较大的数据的做平滑
  if (smooth) {
    DAU <- ts(info_df$DAU, start = c(1,1), frequency = 7)
    fit <- stl(DAU, s.window = "period", na.action = na.omit)
    trend <- as_tibble(fit$time.series)$trend
    info_df$DAU <- c(trend, rep(NA, days - length(trend)))
  }

  # 读入实际DAU及新增,次留设定
  retain_users_old_daily_true <- info_df$Old_DAU[1]
  retain_rate_1 <- info_df$retain_rate_1
  new_users_input <- info_df$DNU

  trues <- na.omit(info_df$DAU) %>% length()

  # 每个环比参数影响的天数
  rep_days <- c(if_max(days, 2),
                if_max(days, 4),
                if_max(days, 7),
                if_max(days, 16),
                if_max(days, 30),
                if_max(days, 120),
                if_max(days, 180),
                if_max(days, days - 360))

  # 得到各日相对于首日的环比留存率
  if(is.character(ring_retain_new)) {
    ring_retain_new <- str_split(ring_retain_new, ",")[[1]]
  }

  ring_retain_new_rates <- map2(ring_retain_new, rep_days, rep) %>%
    unlist %>%
    cumprod

  # 老用户留存
  ring_retain_old_rates <- rep(ring_retain_old, days - 1) %>%
    cumprod

  # 不同天留存计算成矩阵, 1为新增当日, .x为次留
  retain_rates <- map(retain_rate_1, ~c(1, .x, .x * ring_retain_new_rates)) %>%
    do.call(rbind, .)

  # 基于第 i 天新增用户的 n 日留存率
  retain_users_detail <- imap(new_users_input, ~c(rep(0, .y - 1), .x * retain_rates[.y, ], rep(0, days - .y + 1))) %>%
    do.call(rbind, .)

  # 每日所有留存加和得到每天历史新增留存的活跃
  retain_users_new_daily <- colSums(retain_users_detail)[1:days] %>%
    round()

  # 老用户留存活跃
  retain_users_old_daily <- c(retain_users_old_daily_true, retain_users_old_daily_true * ring_retain_old_rates) # 首日和缓慢衰减

  # 当日总活跃
  retain_users_daily <- retain_users_new_daily + retain_users_old_daily

  # 活跃中新增
  retain_users_new_0 <- get_retain_users_n_days(retain_users_detail = retain_users_detail,
                                                days = days,
                                                start_n = 0,
                                                end_n = 0)
  # 活跃中1-7日留存
  retain_users_new_7 <- get_retain_users_n_days(retain_users_detail = retain_users_detail,
                                                days = days,
                                                start_n = 1,
                                                end_n = 7)
  # 活跃中8-15日留存
  retain_users_new_15 <- get_retain_users_n_days(retain_users_detail = retain_users_detail,
                                                 days = days,
                                                 start_n = 8,
                                                 end_n = 15)
  # 活跃中16-30日留存
  retain_users_new_30 <- get_retain_users_n_days(retain_users_detail = retain_users_detail,
                                                 days = days,
                                                 start_n = 16,
                                                 end_n = 30)
  # 活跃中31+日留存
  retain_users_new_old <- get_retain_users_n_days(retain_users_detail = retain_users_detail,
                                                  days = days,
                                                  start_n = 31,
                                                  end_n = days) + retain_users_old_daily

  # 将预测每日DAU写进 info_df 计算差异
  info_df$retain_users_daily <- retain_users_daily[1:nrow(info_df)]
  info_df$diff_rate <- info_df$retain_users_daily / info_df$DAU - 1
  info_df$retain_users_new_0 <- retain_users_new_0
  info_df$retain_users_new_7 <- retain_users_new_7
  info_df$retain_users_new_15 <- retain_users_new_15
  info_df$retain_users_new_30 <- retain_users_new_30
  info_df$retain_users_new_old <- retain_users_new_old

  info_df$retain_users_new_0_cr <- round(info_df$retain_users_new_0/info_df$retain_users_daily, 3)
  info_df$retain_users_new_7_cr <- round(info_df$retain_users_new_7/info_df$retain_users_daily, 3)
  info_df$retain_users_new_15_cr <- round(info_df$retain_users_new_15/info_df$retain_users_daily, 3)
  info_df$retain_users_new_30_cr <- round(info_df$retain_users_new_30/info_df$retain_users_daily, 3)
  info_df$retain_users_new_old_cr <- round(info_df$retain_users_new_old/info_df$retain_users_daily, 3)

  # 输出info_df
  if(csv) write.csv(info_df, paste0("prediction_", run_time, ".csv"))

  # 差异中位数
  tail_diff <- info_df$diff_rate %>%
    na.omit() %>%
    tail(diff_days)

  tail_diff <- tail_diff[length(tail_diff):1] %>%
    `*`(diff_base^(1:diff_days)) %>% # 越近期权重越大
    abs() %>%
    median() %>%
    round(5)

  if (message) message(paste0("真实数据共有:", trues, "日\n末", diff_days, "日加权差异绝对值为:", tail_diff*100, "%"))

  # 绘图观察拟合情况

  if (plot) {
    df_fit <- info_df[1:trues, ]
    graph_fit <- ggplot(df_fit) +
      geom_line(aes(x = Date, y = DAU, color = "real")) +
      geom_line(aes(x = Date, y = retain_users_daily, color = "predict")) +
      scale_x_date(breaks = seq(max(df_fit$Date), min(df_fit$Date), length.out = 20)) +
      scale_colour_manual(name = "fit",
                          values = c(real = "red",
                                     predict = "blue"))

    # 绘图观察预测情况
    df_prediction <- info_df
    graph_prediction <- ggplot(df_prediction) +
      geom_line(aes(x = Date, y = DAU, color = "real")) +
      geom_line(aes(x = Date, y = retain_users_daily, color = "predict")) +
      scale_x_date(breaks = seq(max(df_prediction$Date), min(df_prediction$Date), length.out = 20)) +
      scale_colour_manual(name = "predict",
                          values = c(real = "red",
                                     predict = "blue"))

    if(smooth) {

      graph_forecast <- autoplot(fit, range.bars = 0, main = "DAU forecast")

      cl <- lay_new(
        matrix(1:3, nc = 1),
        heights = c(2, 2, 5))

      lay_grid(list(graph_fit, graph_prediction, graph_forecast), cl)

    } else {

      cl <- lay_new(
        matrix(1:2, nc = 1),
        heights = c(2, 2))

      lay_grid(list(graph_fit, graph_prediction), cl)
    }

  }

  life_time <- get_life_time(retain_users_old_daily_true = retain_users_old_daily_true,
                             ring_retain_new = ring_retain_new,
                             prediction_retain_one = prediction_retain_one,
                             ring_retain_old = ring_retain_old,
                             life_time_year = life_time_year)

  res_df <- tibble(
    diff = tail_diff,  # 加权差异
    days = days, # 总天数
    ring_retain_new = paste(ring_retain_new, collapse = ","), # 新用户环比系数
    ring_retain_old = ring_retain_old,  # 老用户环比系数
    life_time_new = life_time$new,
    life_time_old = life_time$old,
    life_time_year = life_time_year
  )

  if(csv) write.csv(res_df, paste0("parameter_", run_time, ".csv"))

  return(res_df)
}

# get_best_ring_retain <-  function(n, precision = 1000, ring_retain = ring_retain_ios, area = 5, message = FALSE) {
#   choice <- seq(ifelse(ring_retain[n]*precision - precision/area >= 0, ring_retain[n]*precision - precision/area, 0),
#                 ifelse(ring_retain[n]*precision + precision/area <= precision, ring_retain[n]*precision + precision/area, precision))
#   temp_diff <- 100
#   new_temp_diff <- 100
#   for(i in choice) {
#     if(new_temp_diff <= temp_diff) {
#       temp_diff <- new_temp_diff
#       if (message) message("temp_diff: ", temp_diff)
#       ring_retain[n] <- i/precision
#       if (message) message(paste0("第 ", n, " 个参数为: ", ring_retain[n]))
#       new_temp_diff <- abs(get_prediction_daily(ring_retain_new = ring_retain, csv = FALSE, plot = FALSE)$diff)
#       if (message) message("new_temp_diff: ", new_temp_diff)
#     }
#   }
#
#   if (message) message("现参数整体为: ", paste(ring_retain, collapse = " "))
#   return(ring_retain)
#
# }




