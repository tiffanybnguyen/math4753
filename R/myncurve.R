#' @title Curve Visual and Probability Generator
#'
#' @param mu mu of the normal distribution
#' @param sigma sigma of the normal distribution
#' @param a a value for the probability calculation
#'
#' @returns a list of mu, sigma, and probability
#' @export
#'
#' @examples
#' \dontrun{myncurve(2, 3, 5)}
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  list(mu = mu, sigma = sigma)
  xcurve = seq(mu - 3*sigma, a, length=1000)
  ycurve = dnorm(xcurve, mean=mu, sd=sigma)
  polygon(x=c(mu - 3*sigma, xcurve, a), y=c(0, ycurve, 0), col="lightblue")
  probability = round(pnorm(a, mean=mu, sd=sigma), 4)
  probability
  
  return(list(mu = mu, sigma = sigma, probability = probability))
}