#' Title calculate 95 confidence interval for mean
#'
#' @param x a sample
#'
#' @return  confidence interval for mean
#' @export
#' @importFrom stats sd  qt
#'
#' @examples
myci <- function(x) {
  # get alpha
  alpha <- 0.05
  # get n
  n <- length(x)
  # get t
  t<-qt(1-alpha/2,n-1)
  # get +- t
  mp<-c(-1,1)
  # calc ci
  ci <- mean(x)+mp*t*sd(x)/sqrt(n)
  return(ci)
}
