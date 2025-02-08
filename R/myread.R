#' @title CSV File Reader
#'
#' @param csv csv file name (including.csv)
#' @param dird directory where the csv file is located
#'
#' @returns a data frame of the csv file
#' @export
#'
#' @examples
#' \dontrun{spruce.df=myread("SPRUCE.csv", 
#'         "/Users/tiffanynguyen/Documents/College/Sophomore/Spring_2025/Applied_Statistical_Methods/MATH4753nguy0850/labs/lab4/DATAxls/")}
myread=function(csv, dird){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}