#' @title Ratio Estimators for proportion
#' 
#'
#' @description A function that returns the results from calculated estimated values for Ratio Estimators.
#' @import stats
#' @param data A data frame that contains x and y variables.
#' @param c.lev Confidence Level.
#' @param N Population size linked to x variable.
#' @param mu Population sugested mean.
#'
#' @author Marco Aurelio Valles Leal
#'  
#
#'  
#' @examples
#' x=c(100000,50000,75000,200000,150000,175000,150000)
#' y=c(300000,200000,300000,600000,450000,520000,450000)
#' despesas.med=data.frame(area=c(1:7),x=x,y=y)
#' smp.est.razao(data=despesas.med,N=49)
#' 
#' 
#' @export


smp.est.razao.prop = function(data=NULL,c.lev=95,N=NULL,mu=NULL){
  n                 =length(data[,1])
  sample.n          =n
  xpos              =which(tolower(names(data))=="x")
  ypos              =which(tolower(names(data))=="y")
  if(is.null(mu)){
    Xbarra            =mean(data[,xpos])}
  else{
    Xbarra            =mu}
  p.chapeu          =sum(data[,ypos])/sum(data[,xpos])
  p.i               =data[,ypos]/data[,xpos]
  V.P.chapeu <- (N-n)/(N*n*(n-1))*sum((data[,xpos]/Xbarra)^2*(p.i-p.chapeu)^2)
  statistic.val = qnorm(.5+c.lev/200);statistic="Score Statistic (Z)"

  #statistic.val     =qt(p=.5+c.lev/200, df = sample.n-1 );statistic="t Student Statistic"
  #else {}
  
  confidence.interval.pchapeu   =round(p.chapeu+(c(-1,1)*statistic.val*(V.P.chapeu**0.5)),6)
  #confidence.interval.y   =round(yhat+(c(-1,1)*statistic.val*(var.y**0.5)),6)
  #confidence.interval.yt  =round(ythat+(c(-1,1)*statistic.val*(var.yt**0.5)),6)
  
  METHOD = paste("Calculos para o Estimador Razao para Proporcao")
  structure(list(
    "Population Size"                       = N,
    "Sample Size"                           = n,
    "Total X"                               = sum(data[,xpos]),
    "Total Y"                               = sum(data[,ypos]),
    "P.Chapeu"                              = p.chapeu,
    "p.i"                                   = p.i,
    "V(P_Chapeu)"                           = V.P.chapeu,
    "Confidence Interal P Chapeu"           = confidence.interval.pchapeu, 
     method = METHOD),
    class = "power.htest")
}


