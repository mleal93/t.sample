#' @title Ratio Estimators for proportion
#' 
#'
#' @description A function that returns the results from calculated estimated values for Ratio Estimators.
#' @import stats
#' @param data A data frame that contains x and y variables.
#' @param c.lev Confidence Level.
#' @param N Population size linked to x variable.
#' @param Xbarra Population sugested mean.
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


smp.est.regressao = function(data=NULL,c.lev=95,N=NULL,Xbarra=NULL) {
  n                  =length(data[,1])
  sample.n           =n
  xpos               =which(tolower(names(data))=="x")
  ypos               =which(tolower(names(data))=="y")
  xbarra             =mean(data[,xpos])
  ybarra             =mean(data[,ypos])
  sx2                =sd(data[,xpos])**2
  sxy                =cov(data[,xpos],data[,ypos])
  b                  =sxy/sx2
  yhat               =ybarra+(b*(Xbarra-xbarra))
  
  if(is.null(N)){ythat="Parametro N  nao definido"}else{ythat=N*yhat}
  
  cor                =cor(data[,ypos],data[,xpos])
  s2y                =sd(data[,ypos])**2
  var.y              =s2y*(1-cor**2)*((N-n)/(n*N))
  var.yt             =(N**2)*var.y
  if(n<30){statistic.val     =qt(p=.5+c.lev/200, df = sample.n-1 );statistic="t Student Statistic"}
  else {statistic.val = qnorm(.5+c.lev/200);statistic="Score Statistic (Z)"}
  confidence.interval.y      =round(yhat+(c(-1,1)*statistic.val*(var.y**0.5)),6)
  confidence.interval.yt     =round(ythat+(c(-1,1)*statistic.val*(var.yt**0.5)),6)
  
  METHOD = paste("Calculos para o Estimador Regressao")
  structure(list(
    "Population Size"               = N,
    "Sample Size"                   = n,
    "Total X"                       = sum(data[,xpos]),
    "Total Y"                       = sum(data[,ypos]),
    "Calculated Value (sxy)"        = sxy,
    "Calculated Value (sx2)"        = sx2,
    "Calculated Value (b)"          = b,
    "xbarra"                        = xbarra,
    "ybarra"                        = ybarra,
    "Estimated Pontual Value for (Y)"  = yhat,
    "Estimated Total Value of (Y)"     = ythat,
    "Cor XY"                           = cor,
    "Var Y"                            = var.y,
    "Var(Ytotal)"                      =  var.yt,
    "Statistic Used"                   = statistic,
    "Statistic Value"                  = statistic.val,
    " Error of (Y)"                    = statistic.val*(var.y**0.5),
    "Confidence Interval of Y"         = paste0("IC",c.lev,"% : [ ",confidence.interval.y[1]," ; ",confidence.interval.y[2]," ]"),
    "Error (Ytotal)"                   = statistic.val*(var.yt**0.5),
    "Confidence Interval of Total Y"   = paste0("IC",c.lev,"% : [ ",confidence.interval.yt[1]," ; ",confidence.interval.yt[2]," ]"),
    method = METHOD),
    class = "power.htest")
}






