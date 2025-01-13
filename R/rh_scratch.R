#' Given a relative humidity and a volume of water, returns the lithium chloride necessary to reach it
#'
#' @param desired_rh The percentage value of the desired relative humidity
#' @param volume The desired volume 'in Litres'.
#' @param verbose Logical. If TRUE, suppresses messages or output during function execution.
#' @param units Choose between "g/L" (default) or "g/100ml"
#' @return a data frame stating lithium chloride and units
#' @export
#' @examples
#' rh_scratch(desired_rh=60,volume=0.5)


rh_scratch<-function(desired_rh,volume,verbose=TRUE,units="g/L"){

  if (verbose){
    message("Ensure volume unit is L")
    message("Final relative humidity might be slighly off due to changes in volume after lithium chloride is added to initial solution")
  }

  if (units=="g/L") {
    multi<-10
  }else{
    multi<-1
  }

  desired_conc<-(28.6565 + 16.8639 * log((107.7549/(desired_rh - 8.3123)) - 1))*multi

  licl_to_add<-set_units(desired_conc*volume,"g")

  licl_to_add
}


