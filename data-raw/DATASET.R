###############################################################################

usethis::use_data("DATASET")

###############################################################################

library(tidyverse)

###############################################################################

import_ORFID <- function(file, delim){
    
    if (!(delim %in% c("\t", ",", ";"))) {
        stop("The column separator must be '\t', ',' or ';'")
    }
    
    raw_data <- read_delim(file, delim = delim, skip = grep("--------- Upload", readLines(file)))
    
    raw_data <- raw_data %>%
        filter(DTY == "S" | DTY == "I") %>%
        mutate(DUR = parse_time(DUR, '%H:%M:%OS')) 
    
    if(("SCD" %in% names(raw_data))){
        raw_data <- raw_data %>%
            mutate(SCD = as.factor(SCD))
    }
    
    if(("TTY" %in% names(raw_data))){
        raw_data <- raw_data %>%
            mutate(TTY = as.factor(TTY))
    }
    
    if(!("TAG" %in% names(raw_data))){
        Sys.sleep(3)
        warning("The Tag number (TAG) is required for further analysis. Consider upload the data from the ORFID reader again making sure the TAG is selected in the field name")
    }
    

}

###############################################################################

setwd("~/ORFID/data-raw")

reader_1 <- import_ORFID("SCD_BRA_t.txt", delim = "\t")

reader_2 <- import_ORFID("SCD_USA_t.txt", delim = "\t")

reader_3 <- import_ORFID("SCD_ENT_c.txt", delim = ",")

###############################################################################

usethis::use_data(reader_1, overwrite = TRUE)
usethis::use_data(reader_2, overwrite = TRUE)
usethis::use_data(reader_3, overwrite = TRUE)

###############################################################################
