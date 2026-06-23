#' Launch the invertreport web tool
#'
#' Starts a local Shiny app that lets users fill in the 18 framework items
#' interactively, preview the figure, and download the result as PDF or PNG.
#'
#' @param launch.browser Logical. If `TRUE` (the default), opens a browser
#'   window. Passed to [shiny::runApp()].
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Called for its side effect.
#'
#' @examples
#' \dontrun{
#' run_shiny_app()
#' }
#'
#' @export
run_shiny_app <- function(launch.browser = TRUE, ...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Install shiny: install.packages('shiny')", call. = FALSE)
  }
  app_dir <- system.file("shiny", package = "invertreport")
  if (!nzchar(app_dir)) {
    stop("Shiny app directory not found. Reinstall invertreport.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}
