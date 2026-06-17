test_that("framework has 18 items in 9 domains", {
  expect_equal(nrow(framework), 18L)
  expect_equal(length(unique(framework$domain)), 9L)
})

test_that("framework item_ids are unique snake_case", {
  expect_equal(length(framework$item_id), length(unique(framework$item_id)))
  expect_true(all(grepl("^[a-z][a-z0-9_]*$", framework$item_id)))
})

test_that("framework_template returns a fillable template", {
  tmpl <- framework_template()
  expect_s3_class(tmpl, "data.frame")
  expect_equal(nrow(tmpl), 18L)
  expect_true(all(c("item_id", "value") %in% names(tmpl)))
  expect_true(all(tmpl$value == ""))
})

test_that("framework_template marks N/A items for lab studies", {
  tmpl <- framework_template("lab")
  field_only_ids <- framework$item_id[framework$lab == "-"]
  expect_true(all(tmpl$value[tmpl$item_id %in% field_only_ids] == "NA"))
})

test_that("framework_template marks N/A items for field studies", {
  tmpl <- framework_template("field")
  lab_only_ids <- framework$item_id[framework$field == "-"]
  expect_true(all(tmpl$value[tmpl$item_id %in% lab_only_ids] == "NA"))
})
