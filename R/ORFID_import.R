###############################################################################

## Creating a function to import the data
#require(tidyverse)

ORFID_import <- function(file, delim = "\t", skip = NULL){
    
    if (!(delim %in% c("\t", ","))) {
        stop("The column separator must be either tab '\' or ','")
    }
    
    if (is.null(skip)) {
        stop("The number of lines to skip before reading data is mandatory. Count the number of the lines before the data header in the txt file to be imported and assigning this value for the argument 'skip'.")
    }
        
    raw_data <- read_delim(file, delim = delim, skip = skip)
    
    PIT_data <<- raw_data %>%
        filter(DTY == "S" | DTY == "I") %>%
        mutate(DUR=parse_time(DUR, '%H:%M:%OS')) %>%
        mutate(TTY=as.factor(TTY)) %>%
        mutate(SCD=as.factor(SCD))
    
    return(glimpse(PIT_data))
    
}

###############################################################################

# alternative

ORFID_import <- function(file, delim = "\t"){
    
    if (!(delim %in% c("\t", ","))) {
        stop("The column separator must be either tab '\' or ','")
    }
    
    raw_data <- read_delim(file, delim = "\t", skip = grep("--------- Upload", readLines(file)))
    
    PIT_data <<- raw_data %>%
        filter(DTY == "S" | DTY == "I") %>%
        mutate(DUR=parse_time(DUR, '%H:%M:%OS')) %>%
        mutate(TTY=as.factor(TTY)) %>%
        mutate(SCD=as.factor(SCD))
    
    return(glimpse(PIT_data))
    
}

###############################################################################


# validating the function

setwd("C:/Users/hmarq/Desktop")
list.files()

ORFID_import("C:/Users/hmarq/Desktop/SCD_BRA.txt")
