#' Transform Squirrel Temperature Data
#'
#' Reads a CSV file with temperature gradient data from a Squirrel logger, skips the first 17 lines, and reshapes the data from wide to long format,
#' extracting temperature readings from multiple probes.
#'
#' @param file A character string specifying the path to the CSV file.
#'
#' @return A data frame with columns for the original data and two new columns:
#' \code{Date}Data collection timestamp, \code{Probe} indicating the probe identifier and \code{Temperature} containing the corresponding readings.
#'
#'
#' @importFrom readr read_csv
#' @importFrom lubridate mdy_hms
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @export



tgp_squirrel<-function(file){

  data<-read_csv(file,
                 skip = 17)%>%
    pivot_longer(cols = 2:ncol(.), names_to = "Probe",values_to = "Temperature")%>%
    mutate(Date = mdy_hms(Date))

  data

}
