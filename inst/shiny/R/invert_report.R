#' Build an invertebrate welfare reporting figure
#'
#' Produces a standardised, publication-ready summary figure of invertebrate
#' welfare reporting for a given paper, in the spirit of a PRISMA flow
#' diagram: each cell of the figure carries the substantive content for the
#' corresponding item.
#'
#' @param paper A named list of paper metadata. Required: `title`,
#'   `authors`. Optional: `journal`, `version`, `doi`.
#' @param items A data frame with at least `item_id` and `value` columns.
#'   Items with `value = ""` (or `NA`) render as "Not reported". Items with
#'   `value = "NA"` (the character string) render as "Not applicable".
#'   Use [framework_template()] to obtain a ready-to-fill template.
#' @param value_wrap Integer; approximate characters per line for the value
#'   text. Defaults to `75`, which fills the card width at the default
#'   save dimensions; reduce for narrower outputs.
#' @param strict Logical; if `TRUE`, unknown `item_id`s in `items` raise an
#'   error. If `FALSE`, they are warned about and ignored. Defaults to `TRUE`.
#'
#' @return An object of class `invert_report` (a patchwork composition with
#'   added metadata). Can be `print()`ed, `plot()`ted, or saved with
#'   [save_report()] or [ggplot2::ggsave()].
#'
#' @examples
#' tmpl <- framework_template()
#' tmpl$value[tmpl$item_id == "subjects_taxon"] <-
#'   "Bombus terrestris (worker female); morphology + COI"
#' tmpl$value[tmpl$item_id == "proc_anaesthesia"] <- "NA"
#'
#' fig <- invert_report(
#'   paper = list(title = "Demo", authors = "Smith et al. (2026)"),
#'   items = tmpl
#' )
#' \dontrun{ plot(fig) }
#'
#' @export
invert_report <- function(paper, items, value_wrap = 75, strict = TRUE) {
  validate_paper(paper)
  items <- validate_items(items, framework_df = framework, strict = strict)

  # Join items into framework so every framework item has a row
  data <- merge(framework, items[, c("item_id", "value")],
                by = "item_id", all.x = TRUE, sort = FALSE)
  data <- data[order(data$order), , drop = FALSE]

  # Classify each value
  classed <- lapply(data$value, .classify_value)
  data$display_value <- vapply(classed, `[[`, character(1), "display")
  data$colour        <- vapply(classed, `[[`, character(1), "colour")
  data$face          <- vapply(classed, `[[`, character(1), "face")
  data$status        <- vapply(classed, `[[`, character(1), "status")

  fig <- .build_figure(paper, data, value_wrap = value_wrap)

  structure(
    fig,
    class         = c("invert_report", class(fig)),
    paper         = paper,
    items         = items,
    data          = data,
    coverage      = .coverage(data),
    natural_lines = attr(fig, "natural_lines")
  )
}


#' Print method for invert_report
#'
#' Prints a one-line coverage summary and silently returns the figure.
#'
#' @param x An object of class `invert_report`.
#' @param ... Unused.
#' @export
print.invert_report <- function(x, ...) {
  cov <- attr(x, "coverage")
  cat("<invert_report>\n")
  cat(sprintf(
    "  %d of %d applicable items reported (%.0f%%); %d items not applicable.\n",
    cov$reported, cov$applicable, cov$percent_reported, cov$na
  ))
  cat("  Use plot() or save_report() to render.\n")
  invisible(x)
}


#' Plot method for invert_report
#'
#' @param x An object of class `invert_report`.
#' @param ... Unused.
#' @export
plot.invert_report <- function(x, ...) {
  print(structure(x, class = setdiff(class(x), "invert_report")))
  invisible(x)
}


#' Save a report figure to disk
#'
#' Thin wrapper around [ggplot2::ggsave()] with sensible defaults for
#' the figure produced by [invert_report()]. The default height is
#' computed from the figure's natural content size so the saved file is
#' as compact as the content allows, with no large blank regions. Pass
#' an explicit `height` to override.
#'
#' @param report An object of class `invert_report`.
#' @param filename Output file path (extension determines format; .pdf or
#'   .png are recommended).
#' @param width Page width in inches. Defaults to 8.5".
#' @param height Page height in inches. If `NULL` (the default), a
#'   compact height is chosen from the content; the minimum is `6"` and
#'   the maximum is `11"` (US Letter).
#' @param dpi Resolution for raster formats. Defaults to 300.
#' @param ... Additional arguments passed to [ggplot2::ggsave()].
#'
#' @return The filename, invisibly.
#'
#' @examples
#' \dontrun{
#' tmpl <- framework_template()
#' fig  <- invert_report(list(title = "T", authors = "A"), tmpl)
#' save_report(fig, "welfare_reporting.pdf")
#' }
#'
#' @export
save_report <- function(report, filename, width = 8.5, height = NULL,
                        dpi = 300, ...) {
  if (!inherits(report, "invert_report")) {
    stop("`report` must be an object created by invert_report().",
         call. = FALSE)
  }
  if (is.null(height)) {
    # ~0.18" per line at body text size, clamped to a sensible range.
    nat <- attr(report, "natural_lines")
    height <- if (is.null(nat)) 9.5
              else max(6, min(11, nat * 0.18))
  }
  # Strip the invert_report S3 class so ggsave's internal print/draw path
  # doesn't dispatch to print.invert_report (which writes a coverage
  # summary to the console but does not draw the figure). Without this
  # the saved file is blank.
  class(report) <- setdiff(class(report), "invert_report")
  ggplot2::ggsave(filename, plot = report,
                  width = width, height = height, dpi = dpi,
                  bg = "white", ...)
  invisible(filename)
}
