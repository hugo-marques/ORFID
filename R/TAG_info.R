###############################################################################

# Function for getting info about the tags recorded (number of unique tags, tags type and records per tag)  

TAG_info <- function(x){
    
    PIT<<- x %>%
        add_count(TAG) %>%
        distinct(TAG, .keep_all = T) %>%
        select(TAG,TTY, n) %>%
        rename(REC=n)
    
    message("TAG: tag ID
TTY: tag type (A = Animal (ICAR), R = Read-only, W = Writable, P = Phantom)
REC: number of records")
    
    Sys.sleep(1)
    
    return(PIT)
    
}


###############################################################################
