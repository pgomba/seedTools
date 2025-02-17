#' Finds mean of stables temperatures for probes
#'
#' Reads and inds mean of stables temperatures for probes
#'
#' @param file A character string specifying the path to the CSV file.
#'
#' @return A data frame
#'
#' @importFrom dplyr bind_rows
#' @importFrom magrittr %>%
#' @importFrom stats quantile
#' @export

tgp_corner_temp<-function(file){

  data<-tgp_squirrel(file)

  probes<-unique(data$Probe)

  temp_summary<-data.frame()

  for (i in probes) {

    ind_probe<-data%>%
      filter(Probe==i)

    percentiles <- quantile(ind_probe$Temperature, probs = c(0.02, 0.98))

    ind_probe_min <-ind_probe%>%
      filter(between(Temperature,as.numeric(percentiles[1]-2),as.numeric(percentiles[1]+2)))

    ind_probe_max <-ind_probe%>%
      filter(between(Temperature,as.numeric(percentiles[2]-2),as.numeric(percentiles[2]+2)))

    min_mean<-mean(ind_probe_min$Temperature)
    max_mean<-mean(ind_probe_max$Temperature)

    temp_df<-data.frame(Probe=i,mean_min_temp=min_mean,mean_max_temp=max_mean)

    temp_summary<-bind_rows(temp_summary,temp_df)

  }

  temp_summary


}
