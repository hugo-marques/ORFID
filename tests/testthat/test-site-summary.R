testthat::test_that("site_summary returns an error is SCD isn't present", {
  testthat::expect_error(site_summary(tidyr::select(reader_ds, -SCD)))
})
