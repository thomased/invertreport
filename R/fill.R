# Interactive and CSV-template workflows for filling out the framework.

.HELP_TEXT <- c(
  "Commands at any prompt:",
  "  [enter]      keep current value and move on",
  "  any text     set this as the value",
  "  NA           mark as 'not applicable' to this study",
  "  skip         leave blank (will render as 'Not reported')",
  "  back         go to the previous item",
  "  save [path]  save progress to CSV (uses the last path if omitted)",
  "  show         print current state of all items",
  "  quit         stop and return what you have",
  "  help         show this help"
)

#' Interactively fill in the reporting items at the R console
#'
#' Walks through the 18 framework items in canonical order, prompting for a
#' value for each. Designed for use at the R or RStudio console. You can
#' stop at any point with `quit`, save with `save path/to/file.csv`, and
#' resume later by passing the saved file back in (via [load_items()]) or
#' the returned object directly.
#'
#' Each prompt shows the item's domain, name, and description so you do
#' not need to remember `item_id`s or indexing.
#'
#' @param items A data frame of items to start from. If `NULL`, starts from
#'   a blank [framework_template()]. Pass an existing fill in to resume.
#' @param study_type Passed to [framework_template()] if `items` is `NULL`.
#' @param save_to Optional path to save to on `save` (without an argument)
#'   and on normal exit.
#'
#' @return The (possibly partially) filled items data frame, invisibly.
#'
#' @examples
#' \dontrun{
#' # Start fresh
#' items <- fill_items()
#'
#' # Save partway and resume later
#' items <- fill_items(save_to = "my_study.csv")
#' # ...later...
#' items <- fill_items(load_items("my_study.csv"), save_to = "my_study.csv")
#' }
#'
#' @export
fill_items <- function(items = NULL,
                       study_type = c("both", "lab", "field"),
                       save_to = NULL) {
  if (!interactive()) {
    stop("fill_items() requires an interactive R session.", call. = FALSE)
  }
  if (is.null(items)) {
    study_type <- match.arg(study_type)
    items <- framework_template(study_type)
  } else {
    validate_items(items)
  }

  # Build the working table with description metadata for prompts.
  full <- merge(
    framework[, c("order", "domain", "item_id", "item", "description")],
    items[, c("item_id", "value")],
    by = "item_id", all.x = TRUE, sort = FALSE
  )
  full <- full[order(full$order), , drop = FALSE]
  full$value[is.na(full$value)] <- ""

  .show_help()

  i <- 1
  last_path <- save_to
  repeat {
    if (i > nrow(full)) break

    row <- full[i, ]
    cat(sprintf("\n[%d/%d] %s -- %s\n", i, nrow(full), row$domain, row$item))
    cat(paste(strwrap(row$description, width = 76, prefix = "    "),
              collapse = "\n"), "\n\n", sep = "")
    if (nzchar(row$value)) {
      cat("  current: ", row$value, "\n", sep = "")
    } else {
      cat("  current: (empty)\n")
    }

    inp <- readline("  value > ")
    inp_trim <- trimws(inp)
    cmd <- tolower(inp_trim)

    if (identical(inp, "")) {
      i <- i + 1
      next
    }
    if (cmd == "help" || cmd == "?") { .show_help(); next }
    if (cmd == "show")               { .print_state(full); next }
    if (cmd == "quit" || cmd == "q") { break }
    if (cmd == "back" || cmd == "b") { i <- max(1L, i - 1L); next }
    if (cmd == "skip" || cmd == "s") { full$value[i] <- ""; i <- i + 1L; next }
    if (cmd == "na" || cmd == "n/a") { full$value[i] <- "NA"; i <- i + 1L; next }
    if (startsWith(cmd, "save")) {
      parts <- strsplit(inp_trim, "\\s+", perl = TRUE)[[1]]
      path <- if (length(parts) > 1) parts[2] else last_path
      if (is.null(path) || !nzchar(path)) {
        cat("  ! No save path. Use: save my_study.csv\n")
      } else {
        save_items(full[, c("item_id", "value")], path)
        last_path <- path
        cat(sprintf("  saved -> %s\n", path))
      }
      next
    }

    # Treat anything else as the new value
    full$value[i] <- inp_trim
    i <- i + 1L
  }

  out <- full[, c("item_id", "value")]
  if (!is.null(last_path) && nzchar(last_path)) {
    save_items(out, last_path)
    cat(sprintf("\nSaved to %s\n", last_path))
  }
  .print_summary(out)
  invisible(out)
}


#' Edit a single item
#'
#' Pops up a one-item prompt for the named `item_id`. If `item_id` is
#' `NULL`, prints a numbered menu of all 18 items and asks you to pick one.
#' Useful for tweaking a single field after a full fill.
#'
#' @param items A data frame of items.
#' @param item_id Optional. The canonical `item_id` to edit. If omitted,
#'   you'll be shown a numbered menu.
#'
#' @return The updated items data frame, invisibly.
#'
#' @examples
#' \dontrun{
#' items <- edit_item(items, "subjects_taxon")
#' items <- edit_item(items)   # numbered menu
#' }
#'
#' @export
edit_item <- function(items, item_id = NULL) {
  if (!interactive()) {
    stop("edit_item() requires an interactive R session.", call. = FALSE)
  }
  validate_items(items)

  if (is.null(item_id)) {
    cat("Pick an item to edit:\n")
    last_domain <- ""
    for (i in seq_len(nrow(framework))) {
      if (framework$domain[i] != last_domain) {
        cat(sprintf("\n  %s\n", framework$domain[i]))
        last_domain <- framework$domain[i]
      }
      cat(sprintf("    %2d. %s\n", i, framework$item[i]))
    }
    sel <- readline("\n  number > ")
    n <- suppressWarnings(as.integer(trimws(sel)))
    if (is.na(n) || n < 1 || n > nrow(framework)) {
      cat("  ! Invalid choice. No change.\n")
      return(invisible(items))
    }
    item_id <- framework$item_id[n]
  }
  if (!item_id %in% framework$item_id) {
    stop("Unknown item_id: ", item_id, call. = FALSE)
  }

  meta <- framework[framework$item_id == item_id, ]
  cat(sprintf("\n[%s] %s\n", meta$domain, meta$item))
  cat(paste(strwrap(meta$description, width = 76, prefix = "    "),
            collapse = "\n"), "\n\n", sep = "")

  current <- items$value[items$item_id == item_id]
  if (length(current) == 0L || is.na(current)) current <- ""
  if (nzchar(current)) {
    cat("  current: ", current, "\n", sep = "")
  } else {
    cat("  current: (empty)\n")
  }
  cat("  ([enter] keeps current, 'skip' clears, 'NA' marks not applicable)\n")
  inp <- readline("  new value > ")
  inp_trim <- trimws(inp)

  if (identical(inp, "")) return(invisible(items))
  new_val <- if (tolower(inp_trim) == "skip") {
    ""
  } else if (tolower(inp_trim) %in% c("na", "n/a")) {
    "NA"
  } else {
    inp_trim
  }

  if (item_id %in% items$item_id) {
    items$value[items$item_id == item_id] <- new_val
  } else {
    items <- rbind(
      items[, c("item_id", "value")],
      data.frame(item_id = item_id, value = new_val, stringsAsFactors = FALSE)
    )
  }
  cat("  updated.\n")
  invisible(items)
}


#' Save items to a CSV file
#'
#' Writes a CSV with at minimum `item_id` and `value` columns, suitable for
#' loading later with [load_items()].
#'
#' @param items A data frame of items.
#' @param path Output file path.
#'
#' @return The path, invisibly.
#'
#' @examples
#' \dontrun{
#' save_items(items, "my_study_items.csv")
#' }
#'
#' @export
save_items <- function(items, path) {
  validate_items(items)
  out <- items[, c("item_id", "value"), drop = FALSE]
  utils::write.csv(out, path, row.names = FALSE)
  invisible(path)
}


#' Write a blank CSV template for the framework
#'
#' Writes a CSV file with one row per framework item, with `item_id`,
#' `item`, `domain`, `description`, and an empty `value` column. Hand
#' this to a collaborator, fill it in in Excel or any spreadsheet, then
#' load it back with [load_items()]. Extra columns (`item`, `domain`,
#' `description`) are ignored on load and exist only as in-spreadsheet
#' reminders of what each item asks for.
#'
#' @param path Output file path.
#' @param study_type Optional. One of `"both"`, `"lab"`, or `"field"`.
#'   If supplied, items not applicable in that context have their
#'   `value` pre-set to `"NA"`.
#'
#' @return The path, invisibly.
#'
#' @examples
#' \dontrun{
#' save_template("my_study_template.csv", study_type = "field")
#' # ...fill in the value column in Excel...
#' items <- load_items("my_study_template.csv")
#' }
#'
#' @export
save_template <- function(path, study_type = c("both", "lab", "field")) {
  study_type <- match.arg(study_type)
  tmpl <- framework_template(study_type)
  out <- data.frame(
    item_id     = tmpl$item_id,
    item        = tmpl$item,
    domain      = tmpl$domain,
    description = tmpl$description,
    value       = tmpl$value,
    stringsAsFactors = FALSE
  )
  utils::write.csv(out, path, row.names = FALSE)
  message(sprintf("Wrote template to %s", path))
  invisible(path)
}


#' Pretty-print the state of items, grouped by domain
#'
#' Useful for taking stock partway through a fill, or for printing the
#' current state before deciding which items to edit. Reported items show
#' as `●`, items marked NA as `-`, and empty items as `o`.
#'
#' @param items A data frame of items.
#'
#' @return The items, invisibly.
#'
#' @examples
#' \dontrun{
#' show_items(items)
#' }
#'
#' @export
show_items <- function(items) {
  validate_items(items)
  .print_state(merge_for_state(items))
  invisible(items)
}


# ---------- internal helpers ----------

.show_help <- function() cat(paste(.HELP_TEXT, collapse = "\n"), "\n", sep = "")

merge_for_state <- function(items) {
  full <- merge(
    framework[, c("order", "domain", "item", "item_id")],
    items[, c("item_id", "value")],
    by = "item_id", all.x = TRUE, sort = FALSE
  )
  full <- full[order(full$order), , drop = FALSE]
  full$value[is.na(full$value)] <- ""
  full
}

.print_state <- function(full) {
  last_domain <- ""
  for (i in seq_len(nrow(full))) {
    if (full$domain[i] != last_domain) {
      cat(sprintf("\n  %s\n", full$domain[i]))
      last_domain <- full$domain[i]
    }
    status <- if (full$value[i] == "") "o" else if (full$value[i] == "NA") "-" else "*"
    cat(sprintf("    %s  %s\n", status, full$item[i]))
    if (nzchar(full$value[i]) && full$value[i] != "NA") {
      lines <- strwrap(full$value[i], width = 70, prefix = "         ")
      cat(paste(lines, collapse = "\n"), "\n", sep = "")
    }
  }
  cat("\n")
}

.print_summary <- function(items) {
  n_rep <- sum(items$value != "" & items$value != "NA")
  n_na  <- sum(items$value == "NA")
  n_emp <- sum(items$value == "")
  cat(sprintf("\nDone. %d items filled, %d not applicable, %d blank.\n",
              n_rep, n_na, n_emp))
}
