# usethis::use_data("DATASET")

# library(tidyverse)

setwd("~/ORFID/data-raw")

list.files()

colClasses <- c("c", "D", "c", "t", "c", "c", "f", "d", "d")

reader_1 <- readr::read_delim("reader_1.txt", delim = "\t", col_types = colClasses)
reader_2 <- readr::read_delim("reader_2.txt", delim = "\t", col_types = colClasses)
reader_3 <- readr::read_delim("reader_3.txt", delim = "\t", col_types = colClasses)

usethis::use_data(reader_1, overwrite = TRUE)
usethis::use_data(reader_2, overwrite = TRUE)
usethis::use_data(reader_3, overwrite = TRUE)

# Upstream/downstream data for directional function testing
reader_us <- readr::read_delim("reader_us.txt", delim = "\t")
reader_ds <- readr::read_delim("reader_ds.txt", delim = "\t")

usethis::use_data(reader_us, overwrite = TRUE)
usethis::use_data(reader_ds, overwrite = TRUE)
