#' TinyTag Plot
#'
#' This function reads TinyTag CSV data (e.g. from growth chambers),
#' cleans it, and generates a dual-axis ggplot showing temperature and humidity.
#'
#' @param file_path Object with Path to the CSV file to read.
#' @param title Plot title. Defaults to "TinyTag Data".
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' TinyTag_plot("Growth chamber/Large_growth_chamber.csv", title = "Large Growth Chamber")
#' }

tinyTag_plot <- function(file_path, title = "TinyTag Data") {
  # Load required packages
  # Read and preprocess data
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

  # Build plot
  p <- ggplot(data, aes(x = Datetime)) +
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


