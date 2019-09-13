#' Create Euclidean algorithm
#'
#' Euclidean algoritm to find the greatest common divisor of two integers
#'
#' @param a A Numeric value
#' @param b A Numeric value
#'
#' @return The greatest common divisor
#'
#' @export


euclidean<-function(a,b){
 if(a<0){
   a<--a
 }
  if(b<0){
    b<--b
  }
  
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
