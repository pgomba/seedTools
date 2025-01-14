#' Internal function in charge of creating data points using Bonferronis sigmoidal equation obtained from Hay et al 2008 data
#' @param bf_units Choose between "g/L" (default) or "g/100ml"
#' @noRd
#'

rh_bonferroni<-function(bf_units="g/L"){

  if (bf_units=="g/L") {
    multi<-10
  }else{
    multi<-1
  }

  predict<-data.frame(rh=seq(11,99,.1))%>%
    mutate(gl=(28.6565 + 16.8639 * log((107.7549/(rh - 8.3123)) - 1))*multi)

  predict
}


