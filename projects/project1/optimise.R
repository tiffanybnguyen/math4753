f <- function(x){
  x^2-5*x+6
}
windows()
curve(f, 
      xlim = c(1.5,3.5),
      lwd = 4)
op <- optimize(f, interval = c(1,4))
op
abline(v = op$minimum , h = 0)
axis(side = 1,
     labels = TRUE,
     at = 2.5, 
     col.ticks = "Red",
     lwd = 4)
title(main = paste0("Minimum occurs at x = ", op$min))