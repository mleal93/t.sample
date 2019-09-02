#' @title Ratio Estimators
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
#' @examples
#' x=c(100000,50000,75000,200000,150000,175000,150000)
#' y=c(300000,200000,300000,600000,450000,520000,450000)
#' despesas.med=data.frame(area=c(1:7),x=x,y=y)
#' smp.est.razao(data=despesas.med,N=49)
#' 
#' 
#' @export


smp.est.razao = function(data=NULL,c.lev=95,N=NULL,mu=NULL){
  n                 =length(data[,1])
  sample.n          =n
  xpos              =which(tolower(names(data))=="x")
  ypos              =which(tolower(names(data))=="y")
  if(is.null(mu)){
  Xbarra            =mean(data[,xpos])}
  else{
  Xbarra            =mu}
  r                 =mean(data[,ypos])/mean(data[,xpos])
  yhat              =Xbarra*r
  if(is.null(N)){ythat="Tamanho da Populacao nao definido"}else{ythat=N*r*Xbarra}
  s2                =(1/(n-1))*sum((data[,ypos]-(r*data[,xpos]))**2)
  var.y             =(1/(n-1))*sum((data[,ypos]-(r*data[,xpos]))**2)*((N-n)/(n*N))
  var.r             =(1-(n/N))*((s2)/(n*mean(data[,xpos])**2))
  var.yt            =(N**2)*(Xbarra**2)*var.r
  if(n<30){statistic.val     =qt(p=.5+c.lev/200, df = sample.n-1 );statistic="t Student Statistic"}
  else {statistic.val = qnorm(.5+c.lev/200);statistic="Score Statistic (Z)"}
  
  confidence.interval.r   =round(r+(c(-1,1)*statistic.val*(var.r**0.5)),6)
  confidence.interval.y   =round(yhat+(c(-1,1)*statistic.val*(var.y**0.5)),6)
  confidence.interval.yt  =round(ythat+(c(-1,1)*statistic.val*(var.yt**0.5)),6)
  
  METHOD = paste("Calculos para o Estimador Razao")
  structure(list(
    "Population Size"                       = N,
    "Sample Size"                           = n,
    "Total X"                               = sum(data[,xpos]),
    "Total Y"                               = sum(data[,ypos]),
    "Estimated Value for (R)"               = r,
    "Estimated Pontual Value for (Y)"       = yhat,
    "Estimated Total Value of (Y)"          = ythat,
    "Var(R)"                                = var.r,
    "S2e"                                   = s2,
    "N2"                                    = N**2,
    "Xbarra"                                = Xbarra,
    "Xbarra2"                               = Xbarra**2,
    "Var Y"                                 = var.y,
    "Var(Ytotal)"                           = var.yt,
    "Statistic Used"                        = statistic,
    "Statistic Value"                       = statistic.val,
    "Confidence Interval of R"              = paste0("IC",c.lev,"% : [ ",confidence.interval.r[1]," ; ",confidence.interval.r[2]," ]"),
    "Confidence Interval of Y"              = paste0("IC",c.lev,"% : [ ",confidence.interval.y[1]," ; ",confidence.interval.y[2]," ]"),
    "Confidence Interval of Total Y"        = paste0("IC",c.lev,"% : [ ",confidence.interval.yt[1]," ; ",confidence.interval.yt[2]," ]"),
    method = METHOD),
    class = "power.htest")
}




