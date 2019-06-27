#' @title Stratified Random Sampling
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



smp.aae.unif = function(data=NULL,d=NULL,c.lev=95) {
  
  whpos=which(names(data)=="wh")
  xbarrapos=which(names(data)=="media")
  s2hpos=which(names(data)=="s2h")
  Nhpos=which(names(data)=="Nh")
  estpos=which(tolower(names(data))=="estrato")
  
  xbarraest=sum(data[,whpos]*data[,xbarrapos])
  statistic.valz = qnorm(.5+c.lev/200);statistic="Score Statistic (Z)"
  #statistic.valt = qt(p=.5+c.lev/200, df = sample.n-1 );statistic="t Student Statistic"
  
  v=((d*xbarraest)**2)/(statistic.valz**2)
  n0=sum((data[,whpos]**2)*data[,s2hpos])/v
  N=sum(data[,Nhpos])
  n=n0/(1+(n0/N))
  METHOD = paste("Alocacao Uniforme")
  structure(list(
                 "Estimated Mean"     = xbarraest,
                 
                 "Extrato Size"     = length(data[,estpos]),
                 "Error (d)" = d,
                 "Calculated Sample Size (n0)" = ceiling(n0),
                 "Calculated adjust Sample Size (n)" = round(n,0),
                 "Size per Extrato"= rep_len(paste0("n",1:length(data[,estpos])," = ",round(n,0)),length.out =length(data[,estpos])),
                 "Recomended Sample Size"= round(n,0)*length(data[,estpos]),
                 method = METHOD),
            class = "power.htest")
}


