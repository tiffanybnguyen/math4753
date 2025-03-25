#' Project 1: Flight Overbooking Problem
#'
#' @param N number of seats on the flight
#' @param gamma probability a plane will be truly overbooked
#' @param p probability of a "show"
#'
#' @returns a list with N, gamma, p, the discrete ticket count, and the continuous ticket count
#' @export
#'
#' @examples
#' \dontrun{ntickets(N=400, gamma=0.02, p=0.95)}
ntickets=function(N=400, gamma=0.02, p=0.95) {
  # goal: find good number of tickets to sell (n) to profitmaxx; approximated using discrete and continuous distributions
  
  # choosing bounds (gross)
  variance <- ceiling(N*p*(1-p))
  upperBound <- N+2*variance
  
  # ---------- discrete distribution calculation ----------
  nd <- qbinom(1-gamma,N:upperBound,p)
  
  for (n in nd) { # find the thingy thing
    if (N == qbinom(1-gamma, n, p)) {
      nd=n
      break
    }
  }
  
  # ---------- normal distribution calculation ----------
  root <- function(n){
    return(qnorm(1-gamma, n*p, sqrt((n*p)*(1-p))) - N)
  }
  nc <- uniroot(root, lower = N, upper = upperBound)$root
  
  # ---------- named list ----------
  result <- list(
    nd = nd,
    nc = nc,
    N = N,
    p = p,
    gamma = gamma
  )
  
  n_vals <- seq(N, upperBound)
  
  # ---------- discrete objective function ----------
  obj_discrete <- 1-gamma-pbinom(N, n_vals, p)
  
  # ---------- continuous objective function ----------
  obj_continuous <- 1-gamma-pnorm(N+0.5, mean = n_vals*p, sd = sqrt(n_vals*p*(1-p)))

  par(mfrow = c(2, 1))
  
  # ---------- discrete plot ----------
  plot(n_vals, 
       obj_discrete, 
       type="l", 
       col="black", 
       pch=19, 
       ylim=c(0,1),
       xlab="n", 
       ylab="Objective", 
       main=paste("Objective Vs n to find optimal tickets sold\n(", nd, ") gamma=", gamma, " N=", N, " discrete")
       )
  points(n_vals, obj_discrete, col="blue", pch=19)
  abline(v = nd, col="red", lwd=2)
  abline(h=0, col="red", lwd=2)
  
  # ---------- continuous plot ----------
  plot(n_vals, 
       obj_continuous, 
       type="l", 
       col="black", 
       ylim=c(0,1),
       xlab="n", 
       ylab="Objective", main=paste("Objective Vs n to find optimal tickets sold\n(", nc, ") gamma=", gamma, " N=", N, " continuous")
       )
  abline(v = nc, col="blue", lwd=2)
  abline(h=0, col="blue", lwd=2)
  
  return(result)
  
}

ntickets(N=400, gamma=0.02, p=0.95)
