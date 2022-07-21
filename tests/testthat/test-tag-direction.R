testthat::test_that("tag_direction produces an error if incorrect locations are used", {
  test_data <- join_multireader_data(list(reader_ds, reader_us))
  testthat::expect_error(tag_direction(test_data, LOC_vec = c("reader_ds", "reader_us")))
})

testthat::test_that("tag_direction produces a warning if the length of LOC_vector doesn't match the number of locations in the dataframe", {
  test_data <- join_multireader_data(list(reader_ds, reader_us))
  testthat::expect_warning(tag_direction(test_data, LOC_vec = c("downstream_A1")))
})

testthat::test_that("tag_direction produces a data frame with the same number of rows as the input data frame", {
  test_data <- join_multireader_data(list(reader_ds, reader_us))
  testthat::expect_equal(nrow(test_data),
                         nrow(tag_direction(test_data, LOC_vec = c("downstream_A1", "upstream_A1"))))
})

testthat::test_that("tag_direction doesn't produce NA values", {
  test_data <- join_multireader_data(list(reader_ds, reader_us))
  data_out <- tag_direction(test_data, LOC_vec = c("downstream_A1", "upstream_A1"))
  res <- apply(data_out, 2, function(x) sum(is.na(x)) > 0)
  testthat::expect_false(all(res))
})
