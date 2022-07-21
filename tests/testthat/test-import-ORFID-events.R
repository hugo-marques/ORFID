testthat::test_that("No NA values are present in data frame created by import_ORFID_events", {
  test_data <- import_ORFID_events(testthat::test_path("testdata/tab_data.txt"), delim = "\t", verbose = FALSE)
  res <- apply(test_data, 2, function(x) sum(is.na(x)) > 0)
  testthat::expect_false(all(res))
})

testthat::test_that("Only E data are retained when compiling using import_ORFID_events", {
  test_data <- import_ORFID_events(testthat::test_path("testdata/tab_data.txt"), delim = "\t", verbose = FALSE)
  res <- unique(test_data$DTY)
  testthat::expect_true(all(res == "E"))
})

testthat::test_that("import_ORFID_events column classes of common columns are correct", {
  test_data <- import_ORFID_events(testthat::test_path("testdata/tab_data.txt"), delim = "\t", verbose = FALSE)
  test_data_select <- dplyr::select(test_data, DTY, ARR, message)
  res <- unname(unlist(lapply(test_data_select, class)))
  testthat::expect_equal(res,
                         c("character", "POSIXct", "POSIXt", "character"))
})

# Errors ----------------------------------------------------------------

testthat::test_that("import_ORFID_events produces an error with space delimited data", {
  testthat::expect_error(import_ORFID_events(testthat::test_path("testdata/space_data.txt"), delim = "\t", verbose = FALSE))
})

testthat::test_that("import_ORFID_events produces a warning when column SCD is not included", {
  testthat::expect_warning(import_ORFID_events(testthat::test_path("testdata/missing_scd_data.txt"), delim = "\t", verbose = FALSE))
})