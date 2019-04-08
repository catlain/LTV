#' get_ring_retain
#' @description get_ring_retain
#' @param start: start
#' @param n: n
#' @param max: max
#' @return list(ring_retain_new, ring_retain_old)
#' @examples
#' get_ring_retain()
#' @export
#'

# 随机获取环比留存参数
get_ring_retain <- function (start = 0.6, n = 8, max = 0.999) {

  ring_retain_new <- NULL
  i <- 1
  lift <- max/start
  while (i <= n) {
    # 留存环比不断升高,每轮提升幅度降低
    start <- max(start, ring_retain_new[i-1])

    ring_retain_new[i] <- runif(1, start, min(start * lift, max))
    i <- i + 1
    # i > 2之后才有环比系数的增长,新的增长幅度应该小于之前的增长幅度
    if(i > 2) {
      lift <- ring_retain_new[i-1]/ring_retain_new[i-2]
    }
  }

  res <- list()
  res$ring_retain_new <- ring_retain_new
  res$ring_retain_old <- runif(1, ring_retain_new[3], max) # 长期衰减在 14 日留存环比和 max 之间
  return(res)
}
