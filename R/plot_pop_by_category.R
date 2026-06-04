#' Plot between a categorical variable and log(Population)
#'
#' Creates a ridgeline plot between a categorical variable and log(Population).
#'
#' @param cat_var A categorical variable between 'gov_type' and 'regime_category'.
#' @param gov_types Optional character vector of government types to be selected.
#' @param regimes Optional character vector of regime categories to be selected.
#'
#' @return A ridgeline plot.
#'
#'
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes scale_fill_viridis_d scale_x_log10 labs theme_bw theme element_blank element_text
#' @importFrom ggridges geom_density_ridges
#' @importFrom scales comma
#' @export
plot_pop_by_category <- function(cat_var, gov_types = NULL, regimes = NULL) {
  if (missing(cat_var)){
    stop("Error: `cat_var` argument is missing. Provide 'gov_type' or 'regime_category'")
  }

  if (!(is.character(cat_var))){
    stop("Error: `cat_var` must be of type character.")
  }

  if (!(cat_var %in% c('gov_type', 'regime_category'))){
    stop("Error: Selected `cat_var` must be one of: 'gov_type', 'regime_category'")
  }

  dat <- load_data()

  if (!is.null(gov_types)) {
    if (cat_var != "gov_type"){
      stop("Error: `gov_types` argument only works when `cat_var` is set to 'gov_type'.")
    }
    else {
      dat <- filter_gov_types(gov_types)
    }
  }

  if (!is.null(regimes)) {
    if (cat_var != "regime_category"){
      stop("Error: `regimes` argument only works when `cat_var` is set to 'regime_category'.")
    }
    else {
      dat <- filter_regimes(regimes)
    }
  }

  var_name <- c("regime_category" = "Regime Category",
                "gov_type" = "Government Type"
                )[[cat_var]]

  dat |>
    filter(!is.na(.data[[cat_var]]), !is.na(Population)) |>
    ggplot(aes(y = .data[[cat_var]], x = Population, fill = .data[[cat_var]])) +
    geom_density_ridges(alpha = 0.5) +
    scale_x_log10(labels = scales::comma) +
    scale_fill_viridis_d() +
    labs(
      x     = "Population (log scale)",
      y     = "",
      title = paste("Country Population by", var_name),
      subtitle = paste("Comparing the Populations of Countries by", var_name)
    ) +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.minor = element_blank(),
          text = element_text(family = "Helvetica", size = 12))
}
