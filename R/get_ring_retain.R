#' 随机获得新用户的环比系数
#' @description 以[start,max]随机得到环比系数首项，后续各项(i)相比其前一项(i-1)均升高，升高幅度为lift[i]，而其下一项(i+1)相对自身(i)的升高程度(lift[i+1])小于等于lift(i)。以模拟留存随时间逐渐降低，但降低幅度越来越小的常态。
#' @param n: 随机获取环比参数的个性。默认8(对标2,4,7,16,30,120,180,360的时段划分)
#' @param start: 随机获得参数时，新用户留存参数的下限，即第一个参数--第3、4日对次日的环比系数--的下限。默认0.3
#' @param max: 随机获得参数时，参数的上限，包括360日及以上新用户留存和老用户留存。默认0.9999
#' @return 环比留存参数列表(res)，包含新用户环比系数(ring_retain_new)，老用户环比系数(ring_retain_old)。
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
