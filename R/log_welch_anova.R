#' Welch's ANOVA table between a categorical variable and a log-transformed quantitative variable
#'
#' Creates a Welch's ANOVA table between a categorical variable and the logarithm of a quantitative variable.
#'
#' @param quant_var A quantitative variable between 'GDP' and 'Population'
#' @param cat_var A categorical variable between 'gov_type' and 'regime_category'.
#'
#' @return A Welch's ANOVA table.
#'
#'
#' @importFrom stats oneway.test as.formula
#' @importFrom gt gt cols_label tab_header tab_style cell_text cells_title cells_column_labels
#' @export
  log_welch_anova <- function(quant_var, cat_var) {

    dat <- load_data()

    valid_quant <- c("GDP", "Population")
    valid_cat   <- c("gov_type", "regime_category")
    cat_labels  <- c(gov_type       = "Government Type",
                     regime_category = "Regime Category")

    if (missing(quant_var)) stop("Error: `quant_var` argument is missing. Provide 'GDP' or 'Population'")
    if (missing(cat_var))   stop("Error: `cat_var` argument is missing. Provide 'gov_type' or 'regime_category'")

    if (!is.character(quant_var)) stop("Error: `quant_var` must be of type character.")
    if (!is.character(cat_var))   stop("Error: `cat_var` must be of type character.")

    if (!quant_var %in% valid_quant) stop(paste("Error: Selected `quant_var` must be one of:", paste(valid_quant, collapse = ", ")))
    if (!cat_var   %in% valid_cat)   stop(paste("Error: Selected `cat_var` must be one of:",   paste(valid_cat,   collapse = ", ")))

    model <- oneway.test(
      reformulate(cat_var, response = paste0("log(", quant_var, ")")),
      data      = dat,
      var.equal = FALSE
    )

    data.frame(
      f_stat   = unname(model$statistic),
      num_df   = unname(model$parameter[1]),
      denom_df = unname(model$parameter[2]),
      p_value  = model$p.value
    ) |>
      gt() |>
      cols_label(f_stat   = "F-statistic",
                 num_df   = "Numerator Degrees of Freedom",
                 denom_df = "Denominator Degrees of Freedom",
                 p_value  = "P-value") |>
      tab_header(title = paste0("Welch's ANOVA for log(", quant_var, ") and ", cat_labels[cat_var])) |>
      tab_style(style     = cell_text(weight = "bold"),
                locations = list(cells_title(groups = "title"), cells_column_labels()))
  }
