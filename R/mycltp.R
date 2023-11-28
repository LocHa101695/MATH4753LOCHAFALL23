#' Title lab 8
#'
#' @param n process
#' @param iter iteration
#' @param lambda lambda
#' @param ... table variables
#'
#' @return root
#' @importFrom stats rpois
#' @importFrom graphics hist
#' @export mycltp
#'
#' @examples
#' mycltp(5,10000)
mycltp=function(n,iter,lambda=10,...){

  ## r-random sample from the Poisson
  y=rpois(n*iter,lambda=lambda)
  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w=apply(data,2,mean)
  ## We will make a histogram of the values in w
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param=hist(w,plot=FALSE)
  ## Since the histogram will be a density plot we will find the max density

  ymax=max(param$density)
  ## To be on the safe side we will add 10% more to this
  ymax=1.1*ymax
  x=0:max(y)

  ## Make a suitable layout for graphing
  layout(matrix(c(1,1,2,3),nrow=2,ncol=2, byrow=TRUE))

  ## Now we can make the histogram
  hist(w,freq=FALSE,  ylim=c(0,ymax), col=rainbow(max(w)),
       main=paste("Histogram of sample mean","\n", "sample size= ",n," iter=",iter," lambda=",lambda,sep=""),
       xlab="Sample mean",...)
  ## add a density curve made from the sample distribution
  #lines(density(w),col="Blue",lwd=3) # add a density plot
  ## Add a theoretical normal curve
  curve(dnorm(x,mean=lambda,sd=sqrt(lambda/n)),add=TRUE,col="red4",lty=2,lwd=3) # add a theoretical curve
}
