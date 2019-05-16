###############################################################################

## Creating a function to import the data
#require(tidyverse)

ORFID_import <- function(file, delim = "\t", skip = 3){
    
    raw_data <- read_delim(file, delim = delim, skip = skip)
    
    PIT_data <<- raw_data %>%
        filter(DTY == "S" | DTY == "I") %>%
        mutate(DUR=parse_time(DUR, '%H:%M:%OS')) %>%
        mutate(TTY=as.factor(TTY)) %>%
        mutate(SCD=as.factor(SCD))
    
    return(glimpse(PIT_data))
    
}


###############################################################################