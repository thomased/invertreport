test_that("invert_report builds an object with the expected class", {
  tmpl <- framework_template()
  tmpl$value[tmpl$item_id == "subjects_taxon"] <- "Apis mellifera"
  fig <- invert_report(
    paper = list(title = "Demo", authors = "Author et al."),
    items = tmpl
  )
  expect_s3_class(fig, "invert_report")
  expect_true(!is.null(attr(fig, "coverage")))
})

test_that("invert_report errors on missing paper fields", {
  tmpl <- framework_template()
  expect_error(invert_report(list(authors = "x"), tmpl), "title")
  expect_error(invert_report(list(title = "x"), tmpl), "authors")
})

test_that("coverage attribute counts statuses correctly", {
  tmpl <- framework_template()
  # Fill 10 items; mark 2 as N/A; leave 6 empty
  tmpl$value[1:10] <- "filled"
  tmpl$value[11:12] <- "NA"
  fig <- invert_report(
    paper = list(title = "Demo", authors = "Author et al."),
    items = tmpl
  )
  cov <- attr(fig, "coverage")
  expect_equal(cov$reported, 10L)
  expect_equal(cov$na, 2L)
  expect_equal(cov$missing, 6L)
  expect_equal(cov$applicable, 16L)
  expect_equal(round(cov$percent_reported), 63)
})
