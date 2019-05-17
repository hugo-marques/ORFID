###############################################################################

# Creating a unique database from multiple readers data
#require(tidyverse)

# It is necessary to create a list with all data frames to be joined       


join_multireader_data <- function(list){
    if(class(list) != "list"){
        
        stop("The data frames should be in a list")
        
    } else if(any(names(list[[i]])!="SCD")){
        
        stop("Missing site code (SCD) in at least one data frame")
    } else {
        
        df <- list
        %>% bind_rows()
        
    } 
    
    if(anyNA(df$ANT) == T){
        warning("NA values in ANT were replaced by 1")
    }
    
    PIT_data_array <<- df %>%
        mutate(ANT = replace_na(ANT, 1)) %>%
        mutate(NAM = mutate(paste0(SCD,ANT)))
    
    return(glimpse(PIT_data_array))
}


###############################################################################