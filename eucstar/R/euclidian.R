#' Create Euclidian algorithm
#'
#' Euclidian algoritm to find the greatest common divisor of two integers
#'
#' @param a A Numeric value
#' @param b A Numeric value
#'
#' @return The greatest common divisor
#'
#' @export


euclidian<-function(a,b){
  if(a<b){
    d<-a
    a<-b
    b<-d
  }
  c<-a%%b
  while(c!=0){
    a<-b
    b<-c
    c<-a%%b
  }
  return(b)
}
