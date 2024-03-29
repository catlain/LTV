#' 使用已有系数拟合预测DAU并计算差异
#' @description 根据新新用户各段环比参数及实际次日留存计算出每日留存曲线，结合每日实际新增预测各日新用户的每日总留存，再结合老用户按老用户环比参数计算出的老用户每日留存，加和得到每日预测DAU，并与每日实际DAU加权计算得到差异。
#' @param df_list: 函数所需参数列表(df_list)，可由get_prediction_daily()获得。
#' @param type: "train",使用跟训练拟合有关的参数判断，以train_days为训练日期得到拟合结果再以test_df计算差异验证训练结果；"test",直接使用test_df拟合参数及计算差异。默认"train"
#' @param ring_retain_new: 新用户的各段环比参数，可通过get_ring_retain()随机获得。默认 c(0.8480, 0.9138, 0.9525, 0.9679, 0.9801, 0.9861, 0.99, 0.99)
#' @param ring_retain_old: 老用户的环比参数，可通过get_ring_retain()随机获得。默认0.98
#' @param prediction_retain_one: 计算生命周期时，预估未来新增的次日留存。默认0.48
#' @param life_time_year: 计算生命周期的时段(按年统计)。默认1
#' @param csv: 是否输出结果到csv文件，分别为预测结果(prediction_*.csv)和参数(parameter_*.csv)。默认FALSE
#' @param plot: 是否绘图。默认FALSE
#' @param message: 是否输出运行中的信息。默认FALSE
#' @param smooth: 是否使用时序分析包(forecast)获取趋势，目前只支持排除7日(周)影响。默认FALSE
#' @param analysis: 是否分析新老各段新增用户的留存贡献。默认FALSE
#' @param diff_type: 怎么计算差异。默认MSE
#' @return 预测结果(df)
#' @examples
#' get_prediction_daily()
#' @export
#' @import customLayout
#' @import forecast
#' @import purrr
#' @import dplyr
#' @import ggplot2
#' @import stringr
#' @import magrittr
#'
get_prediction_daily <- function(df_list, #  需要计算的数据集
                                 type = "train", # 以训练集拟合参数
                                 params = c(0.8480, 0.9138, 0.9525, 0.9679, 0.9801),
                                 prediction_retain_one = 0.48, # 需要预测的新用户次留(计算总留存天数)
                                 life_time_year = 1, # 预测生命周期年数
                                 csv = FALSE, # 是否输出 prediction.csv
                                 plot = TRUE, # 是否作图
                                 message = FALSE, # 是否打印信息
                                 smooth = FALSE, # 是否先做时序分析取趋势
                                 analysis = FALSE, # 是否分析新老各段新增用户的留存贡献
                                 diff_type = "mse" # 怎么计算差异
) {

  if (class(df_list) != "df_list") {
    message("数据集不合法,请使用make_df()处理的csv数据集!")
    return()
  } else {
    info_df <- switch (type,
                       "train" = df_list$train_df,
                       "test" = df_list$test_df
    )
    train_days <- df_list$train_days
    diff_days <- df_list$diff_days
    diff_base <- df_list$diff_base
  }

  # 生成存储文件名所需
  run_time <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  # 文件一共有多少天(包括已有数据日及需预测日)
  total_days <- nrow(info_df)

  # 对波动较大的数据的做平滑
  if (smooth) {
    DAU <- ts(info_df$DAU, start = c(1,1), frequency = 7)
    fit <- stl(DAU, s.window = "period", na.action = na.omit)
    trend <- as_tibble(fit$time.series)$trend
    info_df$DAU <- c(trend, rep(NA, total_days - length(trend)))
  }

  # 读入实际活跃,新增,次留设定,计算日的老用户活跃等
  retain_users_old_daily_true <- info_df$DAU[1] - info_df$DNU[1]
  retain_rate_1 <- info_df$retain_rate_1
  new_users_input <- info_df$DNU

  true_days <- na.omit(info_df$DAU) %>% length()
  test_days <- true_days - train_days

  # 每个环比参数影响的天数 2,4,7,16,30,120,180,360
  rep_days <- c(if_max(total_days, 2),
                if_max(total_days, 4),
                if_max(total_days, 7),
                if_max(total_days, 16),
                if_max(total_days, 30),
                if_max(total_days, 120),
                if_max(total_days, total_days - 180))

  # 得到各日相对于首日的环比留存率

  params_new <- params[1:7]

  # for (i in 2:7) {
  #   if (params[i] <= params_new[i-1]) {
  #     params_new[i] <- params_new[i-1]
  #   }
  # }

  ring_retain_new <- params_new
  ring_retain_old <- params[8]

  # if(is.character(ring_retain_new)) {
  #   ring_retain_new <- str_split(ring_retain_new, ",")[[1]]
  # }

  # TODO 改成对数函数?
  ring_retain_new_rates <- map2(ring_retain_new, rep_days, rep) %>%
    unlist %>%
    cumprod

  # 老用户留存
  ring_retain_old_rates <- rep(ring_retain_old, total_days - 1) %>%
    cumprod

  # 不同天留存计算成矩阵, 1为新增当日, .x为次留
  retain_rates <- map(retain_rate_1, ~c(1, .x, .x * ring_retain_new_rates)) %>%
    do.call(rbind, .)

  # 基于第 i 天新增用户的 n 日留存率
  retain_users_detail <- imap(new_users_input, ~c(rep(0, .y - 1), .x * retain_rates[.y, ], rep(0, total_days - .y + 1))) %>%
    do.call(rbind, .)

  # 每日所有留存加和得到每天历史新增留存的活跃
  retain_users_new_daily <- colSums(retain_users_detail)[1:total_days] %>%
    round()

  # 老用户留存活跃
  retain_users_old_daily <- c(retain_users_old_daily_true, retain_users_old_daily_true * ring_retain_old_rates) # 首日和缓慢衰减

  # 当日总活跃
  retain_users_daily <- retain_users_new_daily + retain_users_old_daily

  # 将预测每日DAU写进 info_df 计算差异
  info_df$retain_users_daily <- retain_users_daily[1:nrow(info_df)]
  # info_df$diff_rate <- info_df$retain_users_daily / info_df$DAU - 1

  # 分析各时期新增的用户在当天的留存分布
  if (analysis) {

    # 活跃中新增
    info_df$retain_users_new_0 <- get_retain_users_n_days(retain_users_detail = retain_users_detail,
                                                          total_days = total_days,
                                                          start_n = 0,
                                                          end_n = 0)
    # 活跃中1-7日留存
    info_df$retain_users_new_7 <- get_retain_users_n_days(retain_users_detail = retain_users_detail,
                                                          total_days = total_days,
                                                          start_n = 1,
                                                          end_n = 7)
    # 活跃中8-15日留存
    info_df$retain_users_new_15 <- get_retain_users_n_days(retain_users_detail = retain_users_detail,
                                                           total_days = total_days,
                                                           start_n = 8,
                                                           end_n = 15)
    # 活跃中16-30日留存
    info_df$retain_users_new_30 <- get_retain_users_n_days(retain_users_detail = retain_users_detail,
                                                           total_days = total_days,
                                                           start_n = 16,
                                                           end_n = 30)
    # 活跃中31+日留存
    info_df$retain_users_new_old <- get_retain_users_n_days(retain_users_detail = retain_users_detail,
                                                            total_days = total_days,
                                                            start_n = 31,
                                                            end_n = total_days) + retain_users_old_daily

    info_df$retain_users_new_0_cr <- round(info_df$retain_users_new_0/info_df$retain_users_daily, 3)
    info_df$retain_users_new_7_cr <- round(info_df$retain_users_new_7/info_df$retain_users_daily, 3)
    info_df$retain_users_new_15_cr <- round(info_df$retain_users_new_15/info_df$retain_users_daily, 3)
    info_df$retain_users_new_30_cr <- round(info_df$retain_users_new_30/info_df$retain_users_daily, 3)
    info_df$retain_users_new_old_cr <- round(info_df$retain_users_new_old/info_df$retain_users_daily, 3)

  }

  # 输出info_df
  if(csv) write.csv(info_df, paste0("prediction_", run_time, ".csv"))

  # 差异体现在每天的差异的累加上:系数表现优良体现在日间差异减小

  tail_diff <- get_diff(info_df,
                        diff_type = diff_type,
                        diff_days = diff_days)

  if (message) message(paste0("训练数据共有:", true_days, "日\n末", diff_days, "日加权调整R值为:", tail_r2_adj))

  # 绘图观察拟合情况

  if (plot) {

    # cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442") %>%
    #   set_names(c("real", "predict", "test_date", "diff_date"))

    # fit绘图的数据集
    df_fit <- info_df[1:true_days, ]

    # test_date之后的时段作为验证集
    test_date <- df_fit$Date[train_days]
    # diff_date到test_date之间为需要计算加权差异的重点拟合区间
    diff_date <- tail(df_fit$Date,  test_days + diff_days)[1]

    graph_fit <- ggplot(df_fit) +
      geom_line(aes(x = Date, y = DAU, color = "real")) +
      geom_line(aes(x = Date, y = retain_users_daily, color = "predict")) +
      geom_vline(xintercept = test_date, color = "darkgreen", size = 0.6) +
      geom_vline(xintercept = diff_date, color = "black", size = 0.5) +
      scale_x_date(breaks = seq(max(df_fit$Date), min(df_fit$Date), length.out = 20)) +
      scale_colour_manual(name = "fit",
                          values = c("real" = "red",
                                     "predict" = "blue"))

    # 绘图观察预测情况
    df_prediction <- info_df
    graph_prediction <- ggplot(df_prediction) +
      geom_line(aes(x = Date, y = DAU, color = "real")) +
      geom_line(aes(x = Date, y = retain_users_daily, color = "predict")) +
      geom_vline(xintercept = test_date, color = "darkgreen", size = 0.6) +
      geom_vline(xintercept = diff_date, color = "black", size = 0.5) +
      scale_x_date(breaks = seq(max(df_prediction$Date), min(df_prediction$Date), length.out = 20)) +
      scale_colour_manual(name = "predict",
                          values = c("real" = "red",
                                     "predict" = "blue"))

    if(smooth) {

      graph_forecast <- autoplot(fit, range.bars = 0, main = "DAU forecast")

      cl <- lay_new(
        matrix(1:3, nc = 1),
        heights = c(2, 2, 3))

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
    total_days = total_days, # 总天数
    ring_retain_new = paste(ring_retain_new, collapse = ","), # 新用户环比系数
    ring_retain_old = ring_retain_old,  # 老用户环比系数
    life_time_new = life_time$new,
    life_time_old = life_time$old,
    life_time_year = life_time_year
  )

  if(csv) write.csv(res_df, paste0("parameter_", run_time, ".csv"))

  return(res_df)
}


#' @export
#' @import customLayout
#' @import forecast
#' @import purrr
#' @import dplyr
#' @import ggplot2
#' @import stringr
#' @import magrittr
#'

get_prediction_daily_fixed <- function(df_list, #  需要计算的数据集
                                 type = "train", # 以训练集拟合参数
                                 params = c(0.8480, 0.9138, 0.9525, 0.9679, 0.9801),
                                 prediction_retain_one = 0.48, # 需要预测的新用户次留(计算总留存天数)
                                 life_time_year = 1, # 预测生命周期年数
                                 csv = FALSE, # 是否输出 prediction.csv
                                 plot = TRUE, # 是否作图
                                 message = FALSE, # 是否打印信息
                                 smooth = FALSE, # 是否先做时序分析取趋势
                                 analysis = FALSE, # 是否分析新老各段新增用户的留存贡献
                                 diff_type = "mse" # 怎么计算差异
) {

  if (class(df_list) != "df_list") {
    message("数据集不合法,请使用make_df()处理的csv数据集!")
    return()
  } else {
    info_df <- switch (type,
                       "train" = df_list$train_df,
                       "test" = df_list$test_df
    )
    train_days <- df_list$train_days
    diff_days <- df_list$diff_days
    diff_base <- df_list$diff_base
  }

  # 生成存储文件名所需
  run_time <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  # 文件一共有多少天(包括已有数据日及需预测日)
  total_days <- nrow(info_df)

  # 对波动较大的数据的做平滑
  if (smooth) {
    DAU <- ts(info_df$DAU, start = c(1,1), frequency = 7)
    fit <- stl(DAU, s.window = "period", na.action = na.omit)
    trend <- as_tibble(fit$time.series)$trend
    info_df$DAU <- c(trend, rep(NA, total_days - length(trend)))
  }

  # 读入实际活跃,新增,次留设定,计算日的老用户活跃等
  retain_users_old_daily_true <- info_df$DAU[1] - info_df$DNU[1]
  retain_rate_1 <- info_df$retain_rate_1
  new_users_input <- info_df$DNU

  true_days <- na.omit(info_df$DAU) %>% length()
  test_days <- true_days - train_days

  # 每个环比参数影响的天数 2,4,7,16,30,120,180,360
  rep_days <- c(if_max(total_days, 2),
                if_max(total_days, 4),
                if_max(total_days, 7),
                if_max(total_days, 16),
                if_max(total_days, 30),
                if_max(total_days, 120),
                if_max(total_days, total_days - 180))

  # 得到各日相对于首日的环比留存率

  params_new <- params[1:7]

  # for (i in 2:7) {
  #   if (params[i] <= params_new[i-1]) {
  #     params_new[i] <- params_new[i-1]
  #   }
  # }

  ring_retain_new <- params_new
  ring_retain_old <- params[8]

  # if(is.character(ring_retain_new)) {
  #   ring_retain_new <- str_split(ring_retain_new, ",")[[1]]
  # }

  # TODO 改成对数函数?
  ring_retain_new_rates <- map2(ring_retain_new, rep_days, rep) %>%
    unlist %>%
    cumprod

  # 老用户留存
  ring_retain_old_rates <- rep(ring_retain_old, total_days - 1) %>%
    cumprod

  # 不同天留存计算成矩阵, 1为新增当日, .x为次留
  retain_rates <- map(retain_rate_1, ~c(1, .x, .x * ring_retain_new_rates)) %>%
    do.call(rbind, .)

  # 基于第 i 天新增用户的 n 日留存率
  retain_users_detail <- imap(new_users_input, ~c(rep(0, .y - 1), .x * retain_rates[.y, ], rep(0, total_days - .y + 1))) %>%
    do.call(rbind, .)

  # 每日所有留存加和得到每天历史新增留存的活跃
  retain_users_new_daily <- colSums(retain_users_detail)[1:total_days] %>%
    round()

  # 老用户留存活跃
  retain_users_old_daily <- c(retain_users_old_daily_true, retain_users_old_daily_true * ring_retain_old_rates) # 首日和缓慢衰减

  # 当日总活跃
  retain_users_daily <- retain_users_new_daily + retain_users_old_daily

  # 将预测每日DAU写进 info_df 计算差异
  info_df$retain_users_daily <- retain_users_daily[1:nrow(info_df)]
  # info_df$diff_rate <- info_df$retain_users_daily / info_df$DAU - 1

  # 分析各时期新增的用户在当天的留存分布
  if (analysis) {

    # 活跃中新增
    info_df$retain_users_new_0 <- get_retain_users_n_days(retain_users_detail = retain_users_detail,
                                                          total_days = total_days,
                                                          start_n = 0,
                                                          end_n = 0)
    # 活跃中1-7日留存
    info_df$retain_users_new_7 <- get_retain_users_n_days(retain_users_detail = retain_users_detail,
                                                          total_days = total_days,
                                                          start_n = 1,
                                                          end_n = 7)
    # 活跃中8-15日留存
    info_df$retain_users_new_15 <- get_retain_users_n_days(retain_users_detail = retain_users_detail,
                                                           total_days = total_days,
                                                           start_n = 8,
                                                           end_n = 15)
    # 活跃中16-30日留存
    info_df$retain_users_new_30 <- get_retain_users_n_days(retain_users_detail = retain_users_detail,
                                                           total_days = total_days,
                                                           start_n = 16,
                                                           end_n = 30)
    # 活跃中31+日留存
    info_df$retain_users_new_old <- get_retain_users_n_days(retain_users_detail = retain_users_detail,
                                                            total_days = total_days,
                                                            start_n = 31,
                                                            end_n = total_days) + retain_users_old_daily

    info_df$retain_users_new_0_cr <- round(info_df$retain_users_new_0/info_df$retain_users_daily, 3)
    info_df$retain_users_new_7_cr <- round(info_df$retain_users_new_7/info_df$retain_users_daily, 3)
    info_df$retain_users_new_15_cr <- round(info_df$retain_users_new_15/info_df$retain_users_daily, 3)
    info_df$retain_users_new_30_cr <- round(info_df$retain_users_new_30/info_df$retain_users_daily, 3)
    info_df$retain_users_new_old_cr <- round(info_df$retain_users_new_old/info_df$retain_users_daily, 3)

  }

  # 输出info_df
  if(csv) write.csv(info_df, paste0("prediction_", run_time, ".csv"))

  # 差异体现在每天的差异的累加上:系数表现优良体现在日间差异减小

  tail_diff <- get_diff(info_df,
                        diff_type = diff_type,
                        diff_days = diff_days)

  if (message) message(paste0("训练数据共有:", true_days, "日\n末", diff_days, "日加权调整R值为:", tail_r2_adj))

  # 绘图观察拟合情况

  if (plot) {

    # cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442") %>%
    #   set_names(c("real", "predict", "test_date", "diff_date"))

    # fit绘图的数据集
    df_fit <- info_df[1:true_days, ]

    # test_date之后的时段作为验证集
    test_date <- df_fit$Date[train_days]
    # diff_date到test_date之间为需要计算加权差异的重点拟合区间
    diff_date <- tail(df_fit$Date,  test_days + diff_days)[1]

    graph_fit <- ggplot(df_fit) +
      geom_line(aes(x = Date, y = DAU, color = "real")) +
      geom_line(aes(x = Date, y = retain_users_daily, color = "predict")) +
      geom_vline(xintercept = test_date, color = "darkgreen", size = 0.6) +
      geom_vline(xintercept = diff_date, color = "black", size = 0.5) +
      scale_x_date(breaks = seq(max(df_fit$Date), min(df_fit$Date), length.out = 20)) +
      scale_colour_manual(name = "fit",
                          values = c("real" = "red",
                                     "predict" = "blue"))

    # 绘图观察预测情况
    df_prediction <- info_df
    graph_prediction <- ggplot(df_prediction) +
      geom_line(aes(x = Date, y = DAU, color = "real")) +
      geom_line(aes(x = Date, y = retain_users_daily, color = "predict")) +
      geom_vline(xintercept = test_date, color = "darkgreen", size = 0.6) +
      geom_vline(xintercept = diff_date, color = "black", size = 0.5) +
      scale_x_date(breaks = seq(max(df_prediction$Date), min(df_prediction$Date), length.out = 20)) +
      scale_colour_manual(name = "predict",
                          values = c("real" = "red",
                                     "predict" = "blue"))

    if(smooth) {

      graph_forecast <- autoplot(fit, range.bars = 0, main = "DAU forecast")

      cl <- lay_new(
        matrix(1:3, nc = 1),
        heights = c(2, 2, 3))

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
    total_days = total_days, # 总天数
    ring_retain_new = paste(ring_retain_new, collapse = ","), # 新用户环比系数
    ring_retain_old = ring_retain_old,  # 老用户环比系数
    life_time_new = life_time$new,
    life_time_old = life_time$old,
    life_time_year = life_time_year
  )

  if(csv) write.csv(res_df, paste0("parameter_", run_time, ".csv"))

  return(res_df)
}



