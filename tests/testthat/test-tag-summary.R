testthat::test_that("tag_summary returns an error is SCD isn't present", {
  testthat::expect_error(tag_summary(tidyr::select(reader_ds, -TAG)))
})
