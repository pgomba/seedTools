#' Calculates the result relative humidity of mixing two lithium chloride solutions
#'
#' @param rh1 The relative humidity percentage value of solution number one
#' @param vol1 The volume 'in Litres' of solution rh1
#' @param rh2 The relative humidity percentage value of solution number two
#' @param vol2 The volume 'in Litres' of solution rh2
#' @param verbose Logical. If TRUE, suppresses messages or output during function execution.
#'
#' @return a string with mixed solution relative humidity percentage
#' @export
#'
#' @examples
#' rh_mix(rh1=50,vol1=0.5,rh2=70,vol2=0.25)
#'

rh_mix<-function(rh1,vol1,rh2,vol2,verbose=TRUE){

  install_unit("g/100ml", "0.1*g/L")

  message("Reminder: Ensure volumes are L.")
  volume1<-set_units(vol1,"L")
  volume2<-set_units(vol2,"L")

  lc_conc1<-set_units((28.6565 + 16.8639 * log((107.7549/(rh1 - 8.3123)) - 1)),"g/100ml")
  lc_g1<-lc_conc1*volume1

  lc_conc2<-set_units((28.6565 + 16.8639 * log((107.7549/(rh2 - 8.3123)) - 1)),"g/100ml")
  lc_g2<-lc_conc2*volume2

  new_conc<-(lc_g1+lc_g2)/(volume1+volume2)
  new_conc_g100ml<-as.numeric(new_conc)/10


  new_rh<- (116.0672 - 8.3123) / (1 + exp(((new_conc_g100ml) - 28.6565) / 16.8639)) + 8.3123

  new_rh

}

