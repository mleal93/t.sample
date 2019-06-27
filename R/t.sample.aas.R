#' @title Simple Random Sampling 
#' 
#' @description A function that returns the estimated values e confidence values for Proportion.

#' @import stats
#' @param x A vector containing the data of the sample.
#' @param d The deviation for sample size calculation.
#' @param c.lev Confidence Level.
#' @param population Population size, for finity population calculation.
#' @param n Sample size for non existence of sample data.
#' @param mean Mean value for non existence of sample data.
#' @param sd Standard deviation for non existence of sample data.
#' 
#' @author Marco Aurelio Valles Leal
#' @export

smp.aas = function(x=NULL,c.lev, population=NULL,n=NULL,d=NULL,mean=NULL,sd=NULL) {
  
    if(is.null(n)){sample.n=length(x)}else{sample.n=n}
    if(is.null(mean)){sample.mean=mean(x)}else{sample.mean=mean}
    if(is.null(sd)){sample.sd=sd(x)}else{sample.sd=sd}
    if(is.null(population)){pop="Infinity"}
    
    if(FALSE){  statistic.val = qnorm(.5+c.lev/200);statistic="Score Statistic (Z)"
    }else{statistic.val = qt(p=.5+c.lev/200, df = sample.n-1 );statistic="t Student Statistic"}
    
    confidence.interval.mean=round(sample.mean+(c(-1,1)*statistic.val*(sample.sd/sample.n**0.5)),6)
    confidence.interval.total=NULL
    
    if(is.null(d)){d=(statistic.val*sample.sd)/(sample.n**0.5)}
    
    p.ss=((statistic.val*sample.sd)/d)**2
    if(!is.null(population)){
      pop=population
      confidence.interval.total=round(confidence.interval.mean*population,6)
      p.ss = round((p.ss/(1 + ((p.ss-1)/population))), digits=0)
    }
    if(is.null(population)){est.t.mean="Population Not Informed";ic.t.mean="Population Not Informed"
    }else{est.t.mean=sample.mean*population;
          ic.t.mean=paste0("IC",c.lev,"% : [ ",confidence.interval.total[1]," ; ",confidence.interval.total[2]," ]");   }
    
    METHOD = paste("Recommended sample size for a  ",
                   pop, " population  at ", c.lev,
                   "% confidence level", sep = "")
    structure(list(Population = pop,
                   "Statistic Used"= statistic,
                   "Statistic Value"= statistic.val,
                   "Confidence level" = paste0(c.lev,"%"),
                   "Estimated Mean"     = sample.mean,
                   "Estimated Total"= est.t.mean,
                   "Estimated Confidence Interval for Mean" = paste0("IC",c.lev,"% : [ ",confidence.interval.mean[1]," ; ",confidence.interval.mean[2]," ]"),
                   "Estimated Confidence Interval for Total" = ic.t.mean,
                   "Sample SD"       = sample.sd,
                   "Sample Size"     = sample.n,
                   "Error (d)" = d,
                   "Recommended sample size" = ceiling(p.ss),
                   method = METHOD),
              class = "power.htest")
  
  
}

