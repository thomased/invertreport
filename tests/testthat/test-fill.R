test_that("save_items round-trips through load_items", {
  tmpl <- framework_template()
  tmpl$value[tmpl$item_id == "subjects_taxon"] <- "Apis mellifera"
  tmpl$value[tmpl$item_id == "subjects_n"]     <- "n=24"
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  save_items(tmpl, tmp)
  loaded <- load_items(tmp)
  expect_equal(loaded$value[loaded$item_id == "subjects_taxon"],
               "Apis mellifera")
  expect_equal(loaded$value[loaded$item_id == "subjects_n"], "n=24")
})

test_that("save_template writes 18 rows with the expected columns", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  suppressMessages(save_template(tmp))
  df <- read.csv(tmp, stringsAsFactors = FALSE)
  expect_equal(nrow(df), 18L)
  expect_true(all(c("item_id", "item", "domain", "description", "value")
                  %in% names(df)))
  expect_true(all(df$value == "" | is.na(df$value)))
})

test_that("save_template lab/field pre-marks the right rows as NA", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  suppressMessages(save_template(tmp, study_type = "lab"))
  df <- read.csv(tmp, stringsAsFactors = FALSE)
  field_only <- framework$item_id[framework$lab == "-"]
  expect_true(all(df$value[df$item_id %in% field_only] == "NA"))
})

test_that("templates round-trip through invert_report", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  suppressMessages(save_template(tmp))
  items <- load_items(tmp)
  # All blank: should produce an all-"Not reported" report
  fig <- invert_report(
    paper = list(title = "Demo", authors = "A"),
    items = items
  )
  cov <- attr(fig, "coverage")
  expect_equal(cov$reported, 0L)
  expect_equal(cov$missing + cov$na, 18L)
})

test_that("fill_items errors in non-interactive sessions", {
  expect_error(fill_items(), "interactive")
})

test_that("edit_item errors in non-interactive sessions", {
  expect_error(edit_item(framework_template(), "subjects_taxon"),
               "interactive")
})

test_that("show_items prints without error", {
  tmpl <- framework_template()
  tmpl$value[1] <- "filled"
  tmpl$value[2] <- "NA"
  expect_output(show_items(tmpl))
})
