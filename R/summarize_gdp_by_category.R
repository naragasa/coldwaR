#' Summarizes a categorical variable and log(GDP)
#'
#' Creates a summary table between a categorical variable and log(GDP).
#'
#' @param cat_var A categorical variable between 'gov_type' and 'regime_category'.
#' @param gov_types Optional character vector of government types to be selected.
#' @param regimes Optional character vector of regime categories to be selected.
#'
#' @return A gt summary table.
#'
#'
#' @importFrom dplyr filter group_by summarize arrange desc n recode
#' @importFrom gt gt fmt_currency tab_spanner cols_label data_color tab_header tab_style tab_footnote cell_text cells_title cells_column_labels md
#' @importFrom rlang := sym
#' @importFrom stats sd median
#' @export
summarize_gdp_by_category <- function(cat_var, gov_types = NULL, regimes = NULL) {
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
    filter(!is.na(.data[[cat_var]]),
           !is.na(GDP)
    ) |>
    group_by(.data[[cat_var]]) |>
    summarize(n = n(),
              mean_GDP = mean(GDP, na.rm = TRUE),
              med_GDP = median(GDP, na.rm = TRUE),
              sd_GDP = sd(GDP, na.rm = TRUE),
              se_GDP = sd_GDP / sqrt(n),
              min_gdp = min(GDP, na.rm = TRUE),
              max_gdp = max(GDP, na.rm = TRUE),
              .groups    = "drop"
    ) |>
    arrange(desc(mean_GDP)) |>
    gt() |>
    fmt_currency(columns = 3:8,
                 decimals = 2,
                 currency = "USD",
                 placement = "left",
                 pattern = "{x}B"
    ) |>
    tab_spanner(label = "Category Breakdown",
                columns = 1:2
    ) |>
    tab_spanner(label = "Midpoints",
                columns = 3:4
    ) |>
    tab_spanner(label = "Variation",
                columns = 5:6
    ) |>
    tab_spanner(label = "Range",
                columns = 7:8
    ) |>
    cols_label(!!cat_var := var_name,
               n = "Count",
               mean_GDP = "Mean",
               med_GDP = "Median",
               sd_GDP = "Standard Deviation",
               se_GDP = "Standard Error",
               min_gdp = "Minimum",
               max_gdp = "Maximum"
    ) |>
    data_color(columns = 2:8,
               method = "numeric",
               palette = "Blues"
    ) |>
    tab_header(title = paste("Table of GDP Statistics by", var_name),
               subtitle = "(in USD in Billions)"
    ) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = list(cells_title(groups = "title"),
                               cells_title(groups = "subtitle"),
                               cells_column_labels())
    ) |>
    tab_footnote(md("*Note 1*: Standard error was calculated as the standard deviation divided by the square root of the sample size.")) |>
    tab_footnote(md("*Note 2*: Darker blues represent greater values."))
}
