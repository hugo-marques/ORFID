testthat::test_that("No NA values are present in data frame created by import_ORFID", {
  test_data <- import_ORFID(testthat::test_path("testdata/tab_data.txt"), delim = "\t", verbose = FALSE)
  res <- apply(test_data, 2, function(x) sum(is.na(x)) > 0)
  testthat::expect_false(all(res))
})

testthat::test_that("Only S and I data are retained when compiling data using import_ORFID", {
  test_data <- import_ORFID(testthat::test_path("testdata/tab_data.txt"), delim = "\t", verbose = FALSE)
  res <- unique(test_data$DTY)
  testthat::expect_true(all(res %in% c("I", "S")))
})

testthat::test_that("import_ORFID column classes of common columns are correct", {
  test_data <- import_ORFID(testthat::test_path("testdata/tab_data.txt"), delim = "\t", verbose = FALSE)
  test_data_select <- dplyr::select(test_data, DTY, ARR, DUR, TAG, SCD, NCD)
  res <- unname(unlist(lapply(test_data_select, class)))
  testthat::expect_equal(res,
                         c("character", "POSIXct", "POSIXt", "hms", "difftime", "character", "factor", "numeric"))
})

# Errors ----------------------------------------------------------------

testthat::test_that("import_ORFID produces an error with space delimited data", {
  testthat::expect_error(import_ORFID(testthat::test_path("testdata/space_data.txt"), delim = "\t", verbose = FALSE))
})

testthat::test_that("import_ORFID produces a warning when column TAG is not included", {
  testthat::expect_warning(import_ORFID(testthat::test_path("testdata/missing_tag_data.txt"), delim = "\t", verbose = FALSE))
})

testthat::test_that("import_ORFID produces a warning when column SCD is not included", {
  testthat::expect_warning(import_ORFID(testthat::test_path("testdata/missing_scd_data.txt"), delim = "\t", verbose = FALSE))
})