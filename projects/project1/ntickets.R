#' Title
#'
#' @param N number of seats on the flight
#' @param gamma probability a plane will be truly overbooked
#' @param p probability of a "show"
#'
#' @returns
#' @export
#'
#' @examples
ntickets=function(N=400, gamma=0.02, p=0.95) {
  # n will be the number of seats on the flight
  # need to find n?
  N=qbinom(1-gamma,n,p)
  N
}

l=qbinom(1-0.02,400:410,0.95)
l

ntickets()
