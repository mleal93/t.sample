#' @title Cluster Sampling
#'
#' @description A function that returns the results from calculated estimated values for Cluster Sampling.
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


smp.conglomerado = function(data=NULL,c.lev=95,N=NULL,type=NULL,c.unit=NULL,m=NULL) {


  if( tolower(type)=="p"){
    posx=which(tolower(names(data))=="x")
    posn=which(tolower(names(data))=="n")
		if(is.null(m)){
			n=length(data[,1])
			m=length(data[,1])
			proporcoes=data[,posx]/data[,posn]
			f=(n)/N
		}else{

			n=data[,posn][1]
			nup=length(data[,1])
			proporcoes=data[,posx]/data[,posn]
			f=(n*m)/N

		}

    p_barra=sum(proporcoes)/m
    s2c=(sum((proporcoes-p_barra)^2))/(m-1)


    v_pbarra=(1-f)*(s2c/m)
    dp_pbarra=sqrt(v_pbarra)
    if(m<30){statistic.val     =qt(p=.5+c.lev/200, df = m-1 );statistic="t Student Statistic"}
    else {statistic.val = qnorm(.5+c.lev/200);statistic="Score Statistic (Z)"}


  confidence.interval.pbar   =round(p_barra+(c(-1,1)*statistic.val*(dp_pbarra)),6)


  METHOD = paste("Results for calculated estimatives for Cluster Sampling (Proportion)")
  structure(list(
    "Sample Size (n)             "    = n,
    "m                       "    = m,
    "Population Size (N)         "    = N,
    "p_bar                       "    = p_barra,
    "Cluster Variance (S2c)      "    = s2c,
    "Sample Fraction (f)         "    = f,
    "p_bar Variance v(p_bar)     "    = v_pbarra,
    "dp(p_bar)                   "    = dp_pbarra,
    "Statistic                   "    = statistic,
    "Statistic Value             "    = statistic.val,
    "Confidence Interval of p_bar"    = paste0("IC",c.lev,"% : [ ",confidence.interval.pbar[1]," ; ",confidence.interval.pbar[2]," ]"),
    "TOTAL :                     "    = p_barra*N,
    "IC do TOTAL:                "    = paste0("IC",c.lev,"% : [ ",confidence.interval.pbar[1]*N," ; ",confidence.interval.pbar[2]*N," ]"),
    "Proportions                 "    = proporcoes,
    method=METHOD
  ),class = "power.htest")
  }else{
    if(c.unit==T){
      data1=t(data)
      rownames(data1)=1:length(data1[,1])
      colnames(data1)=1:length(data1[1,])
      data=as.data.frame(data1)

    }
    n=length(data[,1])
    m=length(data[1,])
    #data2=data[,]
    yi.=apply(data,1,sum)
    total_yi.=sum(yi.)
    total_yi.2=sum(yi.**2)
   # total_yij=sum(data**2)
    total_yij=sum(data)**2
    y_barra=total_yi./n
    s2b=(total_yi.2-(total_yij/n))/(n-1)
    vchapeu_ybarra=((N-n)/N)*(s2b/n)
    s_ybarra=sqrt(vchapeu_ybarra)

    if(n<30){statistic.val     =qt(p=.5+c.lev/200, df = n-1 );statistic="t Student Statistic"}
    else {statistic.val = qnorm(.5+c.lev/200);statistic="Score Statistic (Z)"}

    confidence.interval.ybar   =round(y_barra+(c(-1,1)*statistic.val*(s_ybarra)),6)

    y_barra2=total_yi./(n*m)
    vchapeu_ybarra2=(1/m^2)*vchapeu_ybarra
    s_ybarra2=sqrt(vchapeu_ybarra2)
    confidence.interval.ybar2   =round(y_barra2+(c(-1,1)*statistic.val*(s_ybarra2)),6)

    y_chapeu=N*y_barra
    vchapeu_ychapeu=N^2*vchapeu_ybarra
    s_ychapeu=sqrt(vchapeu_ychapeu)
    confidence.interval.ychapeu   =round(y_chapeu+(c(-1,1)*statistic.val*(s_ychapeu)),6)


    METHOD = paste("Results for calculated estimatives for Cluster Sampling")
    structure(list(
      "      #####################       "    ="
        Primary Units
      ######################",
      "Statistic                    "    = statistic,
      "Statistic Value              "    = statistic.val,
      "Population Size (N)         "    = N,
      "Tamanho Unidade Primaria (n)"    = n,
      "Tamanho Unidade Secundaria (m)"  = m,
      "yi.                         "    = yi.,
      "Total yi.                   "    = total_yi.,
      "(yi.)^2                     "    = yi.^2,
      "Total (yi.)^2               "    = total_yi.2,
      "Total yij                   "    = total_yij,
      "y_barra                     "    = y_barra,
      "s2b                         "    = s2b,
      "vchapeu_ybarra              "    = vchapeu_ybarra,
      "s_ybarra                    "    = s_ybarra,
      "Confidence Interval of y_bar"    = paste0("IC",c.lev,"% : [ ",confidence.interval.ybar[1]," ; ",confidence.interval.ybar[2]," ]"),
      "      #####################       "    ="
        Secondary Units
      ######################",
      "y_barra2                     "    = y_barra2,
      "vchapeu_ybarra2              "    = vchapeu_ybarra2,
      "s_ybarra2                    "    = s_ybarra2,
      "Confidence Interval of y_bar2"    = paste0("IC",c.lev,"% : [ ",confidence.interval.ybar2[1]," ; ",confidence.interval.ybar2[2]," ]"),
      "      #####################       "    ="
        Estimativas para o Total
      ######################",
      "y_chapeu                     "    = y_chapeu,
      "vchapeu_ychapeu              "    = vchapeu_ychapeu,
      "s_ychapeu                    "    = s_ychapeu,
      "Confidence Interval of y_bar2"    = paste0("IC",c.lev,"% : [ ",confidence.interval.ychapeu[1]," ; ",confidence.interval.ychapeu[2]," ]"),

        method=METHOD

    ),class = "power.htest")
  }

}
