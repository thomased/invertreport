#' Validate user inputs to invert_report()
#'
#' Checks that the items data frame is well-formed and that every
#' `item_id` it references exists in [framework].
#'
#' @param items A data frame with at least `item_id` and `value` columns.
#' @param framework_df The framework to validate against. Defaults to
#'   [framework].
#' @param strict Logical. If `TRUE` (the default), unknown `item_id`s
#'   raise an error. If `FALSE`, they are warned about and dropped.
#'
#' @return Invisibly, the validated items data frame. Errors if validation fails.
#'
#' @examples
#' tmpl <- framework_template()
#' validate_items(tmpl)
#'
#' @export
validate_items <- function(items, framework_df = framework,
                           strict = TRUE) {
  if (!is.data.frame(items)) {
    stop("`items` must be a data frame.", call. = FALSE)
  }
  required <- c("item_id", "value")
  missing_cols <- setdiff(required, names(items))
  if (length(missing_cols) > 0) {
    stop("`items` is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  items$value[is.na(items$value)] <- ""
  unknown <- setdiff(items$item_id, framework_df$item_id)
  if (length(unknown) > 0) {
    msg <- paste0("Unknown item_id(s): ", paste(unknown, collapse = ", "),
                  ". Run `framework$item_id` to see the canonical list.")
    if (strict) stop(msg, call. = FALSE) else {
      warning(msg, call. = FALSE)
      items <- items[items$item_id %in% framework_df$item_id, , drop = FALSE]
    }
  }
  duplicated_ids <- items$item_id[duplicated(items$item_id)]
  if (length(duplicated_ids) > 0) {
    stop("Duplicate item_id(s) in `items`: ",
         paste(unique(duplicated_ids), collapse = ", "), call. = FALSE)
  }
  invisible(as.data.frame(items, stringsAsFactors = FALSE))
}


#' Validate paper metadata
#'
#' @param paper A named list.
#' @return Invisibly, `TRUE` if valid. Errors otherwise.
#' @keywords internal
validate_paper <- function(paper) {
  if (!is.list(paper) || is.null(names(paper))) {
    stop("`paper` must be a named list.", call. = FALSE)
  }
  required <- c("title", "authors")
  missing <- setdiff(required, names(paper))
  if (length(missing) > 0) {
    stop("`paper` must contain at least: ",
         paste(required, collapse = ", "),
         ". Missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}
