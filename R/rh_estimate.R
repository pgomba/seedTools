#' Given a weight in g. of LiCl and a volume, calculate relative humidity
#'
#' @param licl_g Ammount of LiCl g. in solution
#' @param volumen Solution volume 'in Litres'.
#' @param verbose Logical. If TRUE, suppresses messages or output during function execution.
#' @return a data frame stating lithium chloride and units
#' @export
#' @examples
#' rh_estimate(licl_g=300,volume=.75)


rh_estimate<-function(licl_g,volumen,verbose=TRUE){

  if (verbose){
    message("Ensure LiCl weight is in grams and volume unit is L")
  }

  conc<-(licl_g/volumen)/10

  rh<- 107.7549 / (1 + exp((conc - 28.6565) / 16.8639)) + 8.3123

  rh

}
