#' Load geopolitical dataset
#'
#' Reads a CSV file and returns a tibble.
#'
#' @return A tibble containing the dataset.
#'
#' @examples
#' load_data()
#'
#' @importFrom arrow read_parquet
#'
#' @export
load_data <- function(){
  path <- system.file("extdata", "country_meta_data.parquet", package = "coldwaR")
  arrow::read_parquet(file = path)
}
