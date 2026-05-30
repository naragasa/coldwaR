#' Filter government types in country_meta_data.
#'
#' Input a vector of gov_type levels to be selected.
#'
#' @param types Optional character vector of government types to be selected.
#'
#' @return A tibble of the country_meta_data dataset filtered by selected government types.
#'
#' @importFrom dplyr filter

filter_gov_types <- function(types = c("Pure Democracy", "Pure Monarchy", "Democratic Monarchy", "Communist", "Other")){
  if (!all(types %in% c("Pure Democracy", "Pure Monarchy", "Democratic Monarchy", "Communist", "Other"))){
    stop("Selected types must be in: 'Pure Democracy', 'Pure Monarchy', 'Democratic Monarchy', 'Communist', 'Other'")
  }

  filt <- country_meta_data |>
    filter(gov_type %in% types)

  return(filt)
}


