#' @title Stratified Random Sampling Proportion
#' 
#'
#' @description A function that returns the results from calculated estimated values for Stratified Sampling based on Proportions.
#' 
#' @param data A data frame with x and y variables.
#' @param c.lev Confidence Level.
#' @param d The deviation for calculation of sample size. 
#' @import stats
#'
#' @author Marco Aurelio Valles Leal
#' 
#' @export
smp.aae.prop = function(data=NULL,d=NULL,c.lev=95) {#
  
  whpos=which(names(data)=="wh")
  xbarrapos=which(names(data)=="media")
  sh2pos=which(names(data)=="s2h")
  shpos=which(names(data)=="sh")
  Nhpos=which(names(data)=="Nh")
  estpos=which(tolower(names(data))=="estrato")
  chpos=which(names(data)=="ch")
  
  xbarraest=sum(data[,whpos]*data[,xbarrapos])
  statistic.valz = qnorm(.5+c.lev/200);statistic="Score Statistic (Z)"
  #statistic.valt = qt(p=.5+c.lev/200, df = sample.n-1 );statistic="t Student Statistic"
  N=sum(data[,Nhpos])
  v=((d*xbarraest)**2)/(statistic.valz**2)
  n0=((sum(data[,Nhpos]*data[,sh2pos]))/((N)*v))
  n=n0/(1+(n0/N))
  nf<-c()
  for(i in 1:length(data[,estpos])){
   
    nf[i]=n*(data[i,Nhpos]/sum(data[,Nhpos]))
  }
  METHOD = paste("Partilha Otima de Neyman")
  structure(list(
    "Estimated Mean"     = xbarraest,
    
    "Extrato Size"     = length(data[,estpos]),
    "Error (d)" = d,
    "Calculated Sample Size (n0)" = n0,
    "Calculated adjust Sample Size (n)" = round(n,0),
    "Size per Extrato"= rep_len(paste0("n",1:length(data[,estpos])," = ",round(nf,0)),length.out =length(data[,estpos])),
    "Recomended Sample Size"= sum(round(nf,0)),
    method = METHOD),
    class = "power.htest")
}




