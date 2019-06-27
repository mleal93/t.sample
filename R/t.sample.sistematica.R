#' @title Sistematic Sampling
#' 
#' @description A function that returns the results from calculated estimated values for Sistematic Sampling.
#'
#' @import stats
#' @param data A data frame.
#' @param c.lev Confidence Level.
#' @param N Size of population.
#' @param type Put "p" for estimate values for proportion.
#' @param c.unit TRUE if want to transpose data frame.
#'
#' @author Marco Aurelio Valles Leal
#'
#' @export


smp.sistematica = function(data=NULL,c.lev=95,N=NULL,type=NULL,c.unit=NULL) {
  
    n=length(data[,1])
    ni=apply(data,2,length)
    k=length((data[1,]))
    x_barra=apply(data,2,mean)
    s=apply(data,2,sd)
    y_barra=mean(x_barra)
    v_ybarra=(1/N)*sum((x_barra-y_barra)^2*ni)
    ep=sqrt(v_ybarra)
    
    if(n<30){statistic.val     =qt(p=.5+c.lev/200, df = n-1 );statistic="t Student Statistic"}
    else {statistic.val = qnorm(.5+c.lev/200);statistic="Score Statistic (Z)"}
    confidence.interval.ybarra   =round(y_barra+(c(-1,1)*statistic.val*(ep)),6)
    
    

    
    METHOD = paste("Results for calculated estimatives for Cluster Sampling (Proportion)")
    structure(list(
      "Statistic                    "    = statistic,
      "Statistic Value              "    = statistic.val,
      "Groups  (k)                  "    = k,
      "Sample Size (n)              "    = n,
      "Population Size (N)          "    = N,
      "Mean                         "    = x_barra,
      "Y_barra                      "    = y_barra,
      "Sd                           "    = s,
      "Var                          "    = s^2,
      "V(y_barra)                   "    = v_ybarra,
      "EP                           "     = ep,
      "Confidence Interval of y_bar2"    = paste0("IC",c.lev,"% : [ ",confidence.interval.ybarra[1]," ; ",confidence.interval.ybarra[2]," ]"),
      
      method=METHOD      
    ),class = "power.htest")
 
  
}


