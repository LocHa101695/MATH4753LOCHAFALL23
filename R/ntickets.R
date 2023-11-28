#' Title predict tickets to be sold for the flight
#'
#' @param N number of seats in the flight
#' @param gamma the probability a plane will be truly overbooked
#' @param p probability of a "show"
#'
#' @return a named list containing nd, nc, N, p and gamma
#'         where: nd is calculated using the discrete distribution and nc is the same calculated with normal approximation.
#' @importFrom graphics abline layout lines points curve
#' @importFrom stats qbinom pbinom qnorm pnorm uniroot
#' @export
#'
#' @examples
#' ntickets(400,0.02,0.95)
ntickets <- function(N,gamma,p) {
  #set n from 1 to 420
  n <- seq(1,N+20,length=N+20)

  #find nd
  nd <- abs(qbinom(1-gamma, n, p)-N)
  min_nd <- which.min(nd)

  #find nc
  nc <- function(n) {
    mu = n * p
    sigma = sqrt(n * p * (1-p))
    return(qnorm(1-gamma, mu, sigma) - N - 0.5)
  }
  nc_uni<-uniroot(nc,c(1, N+20))
  nc_root<- nc_uni$root

  #create equation to graph discrete
  fnd <- 1-gamma-pbinom(N,n,p)
  fnd1 <- which.min(abs(fnd))

  #create equation to graph continuous
  fnc <- function(n) {
    mu = n * p
    sigma = sqrt(n * p * (1-p))
    return(1-gamma-pnorm(N+0.5, mu, sigma))
  }
  fnc_uni<-uniroot(fnc,c(1, N+20))
  fnc_root<- fnc_uni$root

  #lay out to make two plots
  layout(matrix(c(1:2),nrow=2,ncol=2))

  # plot for discrete
  plot(fnd,
       col='pink3',
       xlim=c(N,N+20),
       xlab = "n",
       ylab = "Objective",
       main = paste("Objective function Vs n to find tickets sold","\n","(",fnd1,")","gamma=",gamma,"N=",N, "discrete"),
       lwd=1,
       cex=0.5,
       type = "b")
  lines(fnd,
        col='darkolivegreen',
        type='b',
        cex=0.1,
        lwd=1)
  points(fnd,
         col="pink3",pch=20,cex=0.5)
  abline(v=fnd1,h=fnd[fnd1],col='red')

  # plot for continuous
  curve(fnc,
       xlim=c(N,N+20),
       xlab = "n",
       ylab = "Objective",
       main = paste("Objective function Vs n to find tickets sold","\n","(",fnc_root,")","gamma=",gamma,"N=",N, "continuous"),
       col='salmon',
       lwd=1,
       type = "l")
  abline(v=fnc_root,h=0,col='forestgreen')

  # print a named list containing nd, nc, N, p, and gamma
  return(list(nd = min_nd, nc = nc_root, N = N, p = p, gamma = gamma))
}

