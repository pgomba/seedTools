#' Calculates the result relative humidity of mixing two lithium chloride solutions
#'
#' @param rh1 The relative humidity percentage value of solution number one
#' @param vol1 The volume 'in Litres' of solution rh1
#' @param rh2 The relative humidity percentage value of solution number two
#' @param vol2 The volume 'in Litres' of solution rh2
#'
#' @return a string with mixed solution relative humidity percentage
#' @export
#'
#' @examples
#' rh_mix(rh1=50,vol1=0.5,rh2=70,vol2=0.25)
#'

rh_mix<-function(rh1,vol1,rh2,vol2){

  message("Reminder: Ensure volumes are L.")

  lc_conc1<-28.6565 + 16.8639 * log((107.7549/(rh1 - 8.3123)) - 1)
  lc_g1<-lc_conc1*vol1

  lc_conc2<-28.6565 + 16.8639 * log((107.7549/(rh2 - 8.3123)) - 1)
  lc_g2<-lc_conc2*vol2

  new_conc<-(lc_g1+lc_g2)/(vol1+vol2)

  new_rh<- (116.0672 - 8.3123) / (1 + exp((new_conc - 28.6565) / 16.8639)) + 8.3123

  print(paste0("New solution will achieve a relative humidity of ", round(new_rh,2),"%"))

}

