#' Summarizes a categorical variable and log(Population)
#'
#' Creates a summary table between a categorical variable and log(Population).
#'
#' @param cat_var A categorical variable.
#'
#' @return A summary table.
#'
#'
#' @importFrom dplyr filter group_by summarize arrange desc n recode
#' @importFrom gt gt fmt_number tab_spanner cols_label data_color tab_header tab_style tab_footnote cell_text cells_title cells_column_labels md
#' @importFrom rlang := sym
#' @importFrom stats sd median
#' @export
summarize_pop_by_category <- function(cat_var) {
  var_name <- deparse(substitute(cat_var))
  var_name2 <- recode(var_name,
                      "regime_category" = "Regime Category",
                      "gov_type" = "Government Type",
                      .default = var_name)

  load_data() |>
    filter(!is.na({{ cat_var }}),
           !is.na(Population)
    ) |>
    group_by({{ cat_var }}) |>
    summarize(n = n(),
              mean_pop = mean(Population, na.rm = TRUE),
              med_pop = median(Population, na.rm = TRUE),
              sd_pop = sd(Population, na.rm = TRUE),
              se_pop = sd_pop / sqrt(n),
              min_pop = min(Population, na.rm = TRUE),
              max_pop = max(Population, na.rm = TRUE),
              .groups    = "drop"
    ) |>
    arrange(desc(mean_pop)) |>
    gt() |>
    fmt_number(columns = 3:8,
               decimals = 2
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
    cols_label(!!var_name := var_name2,
               n = "Count",
               mean_pop = "Mean",
               med_pop = "Median",
               sd_pop = "Standard Deviation",
               se_pop = "Standard Error",
               min_pop = "Minimum",
               max_pop = "Maximum"
    ) |>
    data_color(columns = 2:8,
               method = "numeric",
               palette = "Blues"
    ) |>
    tab_header(title = paste("Table of Population Statistics by", var_name2)
    ) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = list(cells_title(groups = "title"),
                               cells_title(groups = "subtitle"),
                               cells_column_labels())
    ) |>
    tab_footnote(md("*Note 1*: Standard error was calculated as the standard deviation divided by the square root of the sample size.")) |>
    tab_footnote(md("*Note 2*: Darker blues represent greater values."))
}
