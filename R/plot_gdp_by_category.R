#' Plot between a categorical variable and log(GDP)
#'
#' Creates a ridgeline plot between a categorical variable and log(GDP).
#'
#' @param cat_var A categorical variable.
#'
#' @return A ridgeline plot.
#'
#'
#' @importFrom dplyr filter recode
#' @importFrom ggplot2 ggplot aes scale_fill_viridis_d scale_x_log10 labs theme_bw theme element_blank element_text
#' @importFrom ggridges geom_density_ridges
#' @importFrom scales comma
#' @export
plot_gdp_by_category <- function(cat_var) {
  # Store variable names
  var_name <- deparse(substitute(cat_var))
  var_name2 <- recode(var_name,
                      "regime_category" = "Regime Category",
                      "gov_type" = "Government Type",
                      .default = var_name)

  load_data() |>
    filter(!is.na({{ cat_var }})) |>
    ggplot(aes(y = {{ cat_var }}, x = GDP, fill = {{ cat_var }})) +
    geom_density_ridges(alpha = 0.5) +
    scale_fill_viridis_d() +
    scale_x_log10(labels = scales::comma) +
    labs(
      x     = "GDP (log scale)",
      y     = "",
      title = paste("Country GDP in USD (in Billions) by", var_name2),
      subtitle = paste("Comparing the GDP of Countries by", var_name2)
    ) +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.minor = element_blank(),
          text = element_text(family = "Helvetica", size = 12))

}
