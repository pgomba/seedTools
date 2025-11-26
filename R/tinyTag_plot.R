#' TinyTag Plot Timeframe
#'
#' This function reads TinyTag CSV data (e.g. from growth chambers),
#' cleans it, and generates a dual-axis ggplot showing temperature and humidity.
#' It can also allow you to select specific date and time frames within which
#' you want to observe your data#
#'
#'
#' @param file_path Object with Path to the CSV file to read.
#' @param title Plot title. Defaults to "TinyTag Data".
#' @param datefrom Object with date and time (format: yyyy-mm-dd or yyyy-mm-dd hh:mm:ss) from which you want to select data to show in the ggplot
#' @param dateto Object with date and time (formatyyyy-mm-dd or yyyy-mm-dd hh:mm:ss) until which you want to select data to show in the ggplot
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' TinyTag_plot("Growth chamber/Large_growth_chamber.csv", title = "Large Growth Chamber")
#' }


tinyTag_plot <- function(file_path, title = "TinyTag Data",datefrom=NULL, dateto=NULL) {
   # Load required packages
  # Read and preprocess data
  # datefrom and dateto format yyyy-dd-dd or yyyy-mm-dd hh:mm:ss, both work equally
  data <- readr::read_csv(
    file_path,
    col_types = cols(
      Temperature = col_number(),
      Humidity = col_number(),
      `Dew Point` = col_skip()
    ),
    skip = 4
  ) %>%
    select(-Property) %>%
    rename(Datetime = 1) %>%
    arrange(Datetime)


if (is.null(datefrom)) {

  message("Message")

  data2<-data

}else{



  data2<-data %>%
   filter(Datetime>= datefrom & Datetime<=dateto)
}

  # Build plot


  p <- ggplot(data2, aes(x = Datetime)) +
    geom_line(aes(y = Temperature, color = "Temperature (\u00B0C)"), linewidth = 1) +
    geom_line(aes(y = (Humidity - 30) / 3 + 22, color = "Humidity (%RH)"),
              linetype = "dashed", linewidth = 1) +
    scale_y_continuous(
      name = "Temperature (\u00B0C)",
      sec.axis = sec_axis(~ (. - 22) * 3 + 30, name = "Humidity (%RH)")
    ) +
    scale_color_manual(
      name = "",
      values = c("Temperature (\u00B0C)" = "red", "Humidity (%RH)" = "blue")
    ) +
    labs(
      title = title,
      x = "Time"
    ) +
    theme_classic()+
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )

  return(p)
}


