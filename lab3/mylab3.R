library(Intro2R)

# Task 1
dird="/Users/tiffanynguyen/Documents/College/Sophomore/Spring_2025/Applied_Statistical_Methods/labs/lab3/DATAxls/"
getwd()

# Task 2
myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}

spruce.df=myread("SPRUCE.csv")
head(spruce)

# Task 3
x <- spruce$BHDiameter
y <- spruce$Height

plot(x, y, 
     main="Scatter Plot", 
     xlab="BHDiameter", 
     ylab="Height", 
     pch=21, 
     bg="Blue",
     cex=1.2,
     xlim=c(0, 1.1 * max(x)),
     ylim=c(0, 1.1 * max(y))
)

paste("yes, there does seem to be a straight line relationship")

library(s20x)
layout(matrix(1:4, ncol=2, nrow=2, byrow=TRUE))
trendscatter(x, y, 
             main="Loess Plot (f=0.5)",
             xlab="BHDiameter",
             ylab="Height",
             xlim=c(0, 1.1 * max(x)),
             ylim=c(0, 1.1 * max(y)),
             f=0.5
      )
trendscatter(x, y, 
             main="Loess Plot (f=0.6)",
             xlab="BHDiameter",
             ylab="Height",
             xlim=c(0, 1.1 * max(x)),
             ylim=c(0, 1.1 * max(y)),
             f=0.6
)
trendscatter(x, y, 
             main="Loess Plot (f=0.7)",
             xlab="BHDiameter",
             ylab="Height",
             xlim=c(0, 1.1 * max(x)),
             ylim=c(0, 1.1 * max(y)),
             f=0.7
)

spruce.lm <- lm(spruce$Height~spruce$BHDiameter)

plot(y ~ x,
     main="Spruce Height ~ Diameter",
     xlab="BHDiameter",
     ylab="Height",
     pch=21,
     bg="Blue",
     xlim=c(0, 1.1 * max(x)),
     ylim=c(0, 1.1 * max(y))
         )
abline(spruce.lm)
paste("The straight line seems appropriate for the graph as a majority of the points align with the line.")

# Task 4
layout(matrix(1:4, ncol=2, nrow=2, byrow = TRUE))
layout.show(4)

plot(y ~ x, 
     main="Spruce Height ~ Diameter + Regression",
     xlab="BHDiameter",
     ylab="Height",
     pch=21,
     bg="Blue",
     xlim=c(0, 1.1 * max(x)),
     ylim=c(0, 1.1 * max(y))
)
abline(spruce.lm)

plot(y ~ x, 
     main="Spruce Height ~ Diameter + RSS",
     xlab="BHDiameter",
     ylab="Height",
     pch=21,
     bg="Blue",
     xlim=c(0, 1.1 * max(x)),
     ylim=c(0, 1.1 * max(y))
)
abline(spruce.lm)
fit <- fitted(spruce.lm)
segments(x, y, x, fit)

RSS <- with(spruce, sum((y - fit)^2))
paste("RSS: ", RSS)

plot(y ~ x, 
     main="Spruce Height ~ Diameter + MSS",
     xlab="BHDiameter",
     ylab="Height",
     pch=21,
     bg="Blue",
     xlim=c(0, 1.1 * max(x)),
     ylim=c(0, 1.1 * max(y))
)
with(spruce, abline(h=mean(y)))
abline(spruce.lm)
with(spruce, segments(BHDiameter, mean(Height), BHDiameter, fit, col="Pink"))

MSS <- with(spruce, sum((fit - mean(Height))^2))
paste("MSS: ", MSS)

plot(y ~ x, 
     main="Spruce Height ~ Diameter + TSS",
     xlab="BHDiameter",
     ylab="Height",
     pch=21,
     bg="Blue",
     xlim=c(0, 1.1 * max(x)),
     ylim=c(0, 1.1 * max(y))
)
with(spruce, abline(h=mean(y)))
abline(spruce.lm)
with(spruce, segments(BHDiameter, Height, BHDiameter, mean(Height), col="Pink"))

TSS <- with(spruce, sum((Height - mean(Height))^2))
paste("TSS: ", TSS)

paste("MSS/TSS: ", MSS/TSS)
paste("0.65~ signifies that about 65% of the variance in the dependent variables is explained by the independent variables")

paste("TSS(", TSS, ") = RSS + MSS(", RSS + MSS, ")")
paste("yes, TSS = RSS + MSS")

# Task 5
summary(spruce.lm)
paste("Slope: ", coef(spruce.lm)[2])
paste("Intercept: ", coef(spruce.lm)[1])
paste("Equation: Height = ", coef(spruce.lm)[1], "+", coef(spruce.lm)[2], "* BHDiameter") # do in latex?

predict(spruce.lm, pred=data.frame(BHDiameter=c(15, 18, 20)))

# Task 6
library(ggplot2)
ggplot(spruce, aes(x=BHDiameter, y=Height, colour=BHDiameter)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  geom_line() +
  labs(title="Height vs Diameter", x="BHDiameter", y="Height")

# Task 7 (do not run this, its a shiny app) if ran, use this ```{r slr, echo=FALSE} for header and ``` for footer
library(ggplot2)
# DATA
spruce.df = read.csv("SPRUCE.csv")

# INPUTS
inputPanel(
  
  selectInput("plotg", "Choose plot type",
              list(`points` = c("linear regression", "points alone", "points with segments")
              )
  )
  
)

renderPlot({
  
  g = ggplot(spruce.df, aes(x = BHDiameter, y = Height)) + 
    geom_point() +
    labs(title="Height vs Diameter", 
         x="BHDiameter", 
         y="Height")
  
  linear = g + geom_smooth(method="lm")
  points = g
  points_with_segments = g + geom_line()
  
  if(input$plotg == "linear regression") print(linear)
  
  if(input$plotg == "points alone") print(points)
  
  if(input$plotg == "points with segments") print(points_with_segments)
  
})

        