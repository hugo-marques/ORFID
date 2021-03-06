###############################################################################

usethis::use_data("DATASET")

###############################################################################

library(tidyverse)

###############################################################################

setwd("~/ORFID/data-raw")

list.files()

reader_1 <- read_delim("reader_1.txt", delim = "\t")

reader_2 <- read_delim("reader_2.txt", delim = "\t")

reader_3 <- read_delim("reader_3.txt", delim = "\t")


###############################################################################

usethis::use_data(reader_1, overwrite = TRUE)
usethis::use_data(reader_2, overwrite = TRUE)
usethis::use_data(reader_3, overwrite = TRUE)


###############################################################################
