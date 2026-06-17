test_that("validate_items accepts a fresh template", {
  expect_silent(validate_items(framework_template()))
})

test_that("validate_items errors on missing columns", {
  expect_error(validate_items(data.frame(item_id = "x")),
               "missing required column")
  expect_error(validate_items(data.frame(value = "x")),
               "missing required column")
})

test_that("validate_items errors on unknown item_id in strict mode", {
  bad <- data.frame(item_id = "not_a_real_item", value = "x",
                    stringsAsFactors = FALSE)
  expect_error(validate_items(bad), "Unknown item_id")
})

test_that("validate_items warns and drops in non-strict mode", {
  bad <- data.frame(item_id = c("subjects_taxon", "not_a_real_item"),
                    value = c("Apis mellifera", "x"),
                    stringsAsFactors = FALSE)
  expect_warning(out <- validate_items(bad, strict = FALSE),
                 "Unknown item_id")
  expect_equal(nrow(out), 1L)
  expect_equal(out$item_id, "subjects_taxon")
})

test_that("validate_items rejects duplicated item_ids", {
  dupe <- data.frame(item_id = c("subjects_taxon", "subjects_taxon"),
                     value = c("a", "b"), stringsAsFactors = FALSE)
  expect_error(validate_items(dupe), "Duplicate item_id")
})
