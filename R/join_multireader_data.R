#' @title Summarizing tag-specific info of \code{\link{import_ORFID}}.
#'
#' @description Function allows users to combine unique readers into an array, using the data obtained from \code{\link{import_ORFID}} function. It is necessary to create a list with all data frames to be joined. If the data frames have diferent variables,   
#' @param x list: containing data frames created from the \code{\link{import_ORFID}} function to combine.
#' @details Data frame is created in the user environment. The output of \code{\link{join_multireader_data}} will contain a column if that column appears in any of data frames combined.
#' @return Returns a tibble object.
#' @author Hugo Marques
#' @examples
#' 
#' # Create the list containing the imported files:
#' \dontrun{} readers list(data_USA, data_BRA)
#' 
#'  # Combine the files:
#' \dontrun{} data <- join_multireader_data(readers)
#' 
#' @export

join_multireader_data <- function(x){
    
    message("Variable LOC (local) is the combination between SCD (Site Code) and ANT (Antenna)")
    
    if (class(x) != "list") {
        stop("The data frames should be in a list")
    } 
        
    y <- x %>%
        bind_rows()

    if(!("SCD" %in% names(y))){
        stop("Missing site code (SCD) in at least one data frame")
        }
    
    if(!("ANT" %in% names(y))){
        y <- y %>%
            mutate(ANT = 1)
    }
    
    if(anyNA(y$ANT) == T) {
        warning("NA values in ANT were replaced by 1")
    }
    
    PIT_data_array <- y %>%
        mutate(ANT = replace_na(ANT, 1)) %>%
        mutate(ANT = as.factor(ANT)) %>%
        mutate(LOC = paste0(SCD,ANT)) %>%
        mutate(LOC = as.factor(LOC))
    
    Sys.sleep(1)
    
    return(glimpse(PIT_data_array))
        
}


###############################################################################