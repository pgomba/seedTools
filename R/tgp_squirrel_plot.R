#' Plot Squirrel Temperature Data
#'
#' Reads a CSV file and plots temperature gradient plate data
#'
#' @param file A character string specifying the path to the CSV file.
#' @param start Select start time of graph threshold
#' @param end Select end time for graph threshold#'
#'
#' @return A plot
#'
#'
#' @importFrom readr read_csv
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @importFrom scales label_number
#' @export



tgp_squirrel_plot<-function(file,start=NULL,end=NULL){

  data<-tgp_squirrel(file)

  if (is.null(start)) {
    start <- min(data$Date, na.rm = TRUE)
  } else {
    start <- as.Date(start)
  }

  if (is.null(end)) {
    end <- max(data$Date, na.rm = TRUE)
  } else {
    end <- as.Date(end)
  }


  data<-data%>%
    filter(Date >= start & Date <= end)

  graph<- ggplot(data,aes(x=Date,y=Temperature,colour=Probe))+
    geom_line(linewidth=2)+
    scale_y_continuous(labels = scales::label_number(suffix =  ("\u00B0C"),limits = c(5,45)))+
    theme_classic()


  graph

}
