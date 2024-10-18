#' Internal function in charge of creating data points using Bonferronis sigmoidal equation obtained from Hay et al 2008 data
#'
#' @noRd
#'

rh_bonferroni<-function(){

  predict<-data.frame(rh=seq(11,99,.1))%>%
    mutate(gl=(28.6565 + 16.8639 * log((107.7549/(rh - 8.3123)) - 1))*10)

  predict
}


