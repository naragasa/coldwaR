#' Load geopolitical dataset
#'
#' Reads a CSV file and returns a tibble.
#'
#' @return A tibble containing the dataset.
#'
#' @examples
#' load_data()
#'
#' @importFrom readr read_csv
#' @export
load_data <- function(){
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-11-05/democracy_data.csv')
}
