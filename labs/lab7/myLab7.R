library(MATH4753nguy0850)
library(usethis)

fire <- MATH4753nguy0850::myread("FIREDAM.csv", "/Users/tiffanynguyen/Documents/College/Sophomore/Spring_2025/Applied_Statistical_Methods/MATH4753nguy0850/data/")

usethis::use_data(fire, overwrite = TRUE)
devtools::document()
data("fire")
knitr::kable(head(fire))

fire
