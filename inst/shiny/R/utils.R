# Internal helpers. Not exported.

#' @keywords internal
.welfare_domains <- c("Nutrition", "Environment", "Health",
                      "Behaviour", "Affective state")

#' @keywords internal
.essential_domains <- c("Subjects", "Procedures", "Ethics & compliance")

#' @keywords internal
.palette <- list(
  welfare    = "#3F7A3A",
  essential  = "#2E5F8E",
  text       = "#1f1f1f",
  label      = "#5a5a5a",
  muted      = "#a8a8a8",
  na         = "#7a7a7a",
  rule       = "#cccccc",
  panel_edge = "#d0d0d0"
)

#' Decide how to render a value
#'
#' @return A list with `display`, `colour`, `face` ("plain" or "italic"),
#'   and `status` ("reported", "missing", or "na").
#' @keywords internal
.classify_value <- function(value) {
  if (is.na(value) || identical(value, "")) {
    return(list(display = "Not reported",
                colour  = .palette$muted,
                face    = "italic",
                status  = "missing"))
  }
  if (identical(toupper(value), "NA") || identical(value, "N/A")) {
    return(list(display = "Not applicable",
                colour  = .palette$na,
                face    = "italic",
                status  = "na"))
  }
  list(display = value,
       colour  = .palette$text,
       face    = "plain",
       status  = "reported")
}


#' Compute simple coverage summary
#'
#' @param items_with_status A data frame containing a `status` column with
#'   values in `c("reported", "missing", "na")`.
#' @return A list with counts and a percentage of applicable items reported.
#' @keywords internal
.coverage <- function(items_with_status) {
  n_reported <- sum(items_with_status$status == "reported")
  n_missing  <- sum(items_with_status$status == "missing")
  n_na       <- sum(items_with_status$status == "na")
  applicable <- n_reported + n_missing
  pct <- if (applicable == 0) NA_real_ else 100 * n_reported / applicable
  list(reported = n_reported, missing = n_missing, na = n_na,
       applicable = applicable, percent_reported = pct)
}
