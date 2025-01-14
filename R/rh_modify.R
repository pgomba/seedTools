#' Calculate necessary changes to lithium chloride solutions to achieve target relative humidity
#'
#' @param initial_rh The relative humidity percentage value of the solution to be modified
#' @param desired_rh The percentage value of the desired relative humidity
#' @param initial_volume The volume 'in Litres' of the initial solution
#' @param verbose Logical. If TRUE, suppresses messages or output during function execution.
#' @return a string with instructions to modify the solution
#' @export
#'
#' @examples
#' rh_modify(initial_rh=20,desired_rh=40,initial_volume=0.5)


rh_modify<-function(initial_rh,desired_rh,initial_volume,verbose=TRUE){

  if (verbose){
  message("Reminder: Ensure all values are in g. and L.")
    }

  initial_conc<-set_units((28.6565 + 16.8639 * log((107.7549/(initial_rh - 8.3123)) - 1)),"g/100ml")
  desired_conc<-set_units((28.6565 + 16.8639 * log((107.7549/(desired_rh - 8.3123)) - 1)),"g/100ml")

  starting_volume<-set_units(initial_volume,"L")

  if (initial_conc>desired_conc) {

    final_volume<-set_units((initial_conc*starting_volume)/desired_conc,"L")
    add_w<-final_volume-starting_volume
    add_what<-"water"

    add<-data.frame(Add=add_what,Quantity=add_w)


  }else{


    LiCl_to_add<-(desired_conc-initial_conc)*starting_volume
    add_what<-"LiCl"

    add<-data.frame(Add=add_what,Quantity=LiCl_to_add)

  }

  add
}
