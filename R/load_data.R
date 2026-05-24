#' Load geopolitical dataset
#'
#' Reads a CSV file into a tibble.
#'
#' @param path Path to the CSV file.
#'
#' @return A tibble containing the dataset.
#' @export
#' @importFrom readr read_csv

load_data <- function(){
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-11-05/democracy_data.csv')

}


