testthat::test_that("join_multireader_data returns a warning if SCD is missing", {
  testthat::expect_error(join_multireader_data(x = list(reader_ds, dplyr::select(reader_us, -SCD))))
})