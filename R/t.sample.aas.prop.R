#' @title Simple Random Sampling for Proportion
#'
#'
#' @description A function that returns the results from calculated estimated values for Stratified Sampling.
#'              
#' @param dprop The deviation for sample size calculation.
#' @param c.lev Confidence Level.
#' @param population Population size.
#' @param n Sample size for non existence of a sample.
#' @param p Estimated proportion.

#' @author Marco Aurelio Valles Leal

#' @export
  smp.aas.prop = function(c.lev, p=.5,dprop=.05, population=NULL,n=NULL) {
  if(!is.null(n)){sample.n=n}
  z.val = qnorm(.5+c.lev/200)
  derror=z.val*((p*(1-p))/sample.n)**0.5
  statistic="Score Statistic (Z)"
  confidence.interval.prop=round(p+(c(-1,1)*z.val*((p*(1-p))/sample.n)**0.5),4)
  pop="Infinity"
  p.ss = ceiling((z.val^2 * p * (1-p))/dprop^2)
  if(!is.null(population)){
    pop=population
    p.ss = ceiling((p.ss/(1 + ((p.ss-1)/population))))}
  METHOD = paste("Recommended sample size for a  ",
                 pop, "population  at ", c.lev,
                 "% confidence level", sep = "")
  structure(list(Population = population,
                 "Confidence level" = c.lev,
                 "Statistic Used"= statistic,
                 "Calculated Error"=derror,
                 "Error (d)" = dprop,
                 "Estimated p" = p,
                 if(!is.null(population)){"Pontual Estimated Total"= dprop*population},
                 "Confidence Interval" = paste0("IC",c.lev,"% : [ ",confidence.interval.prop[1]," ; ",confidence.interval.prop[2]," ]"),
                 "Total Confidence Interval" = paste0("IC",c.lev,"% : [ ",confidence.interval.prop[1]*population," ; ",confidence.interval.prop[2]*population," ]"),
                 "Recommended sample size" = p.ss,
                 method = METHOD),
            class = "power.htest")
}
