#' Filter regime categories in country_meta_data.
#'
#' Input a vector of regime_category levels to be selected.
#'
#' @param types Optional character vector of regime categories to be selected.
#'
#' @return A tibble of the country_meta_data dataset filtered by selected regime categories.
#'
#' @importFrom dplyr filter

filter_regimes <- function(regimes = c("Parliamentary democracy", "Civilian dictatorship", "Presidential democracy", "Military dictatorship", "Mixed democratic", "Royal dictatorship")){
  if (!all(regimes %in% c("Parliamentary democracy", "Civilian dictatorship", "Presidential democracy", "Military dictatorship", "Mixed democratic", "Royal dictatorship"))){
    stop("Selected types must be in: 'Parliamentary democracy', 'Civilian dictatorship', 'Presidential democracy', 'Military dictatorship', 'Mixed democratic', 'Royal dictatorship'")
  }

  filt <- load_data() |>
    filter(regime_category %in% regimes)

  return(filt)
}
