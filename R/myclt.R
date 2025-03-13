#' Central Limit Theorem function
#'
#' @param n sample size
#' @param iter number of iterations
#'
#' @returns a graph and a vector of the sum of the uniform random variables
#' @export
#'
#' @examples
#' \dontrun{myclt(n=10,iter=10000)}
myclt=function(n,iter){
  y=runif(n*iter,0,5) # A
  data=matrix(y,nr=n,nc=iter,byrow=TRUE) #B
  sm=apply(data,2,sum) #C
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}