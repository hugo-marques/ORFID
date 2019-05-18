#' @title Summarizing tag-specific info of \code{\link{import_ORFID}}.
#'
#' @description Function allows users to combine unique readers into an array, using the data obtained from \code{\link{import_ORFID}} function. It is necessary to create a list with all data frames to be joined. If the data frames have diferent variables,   
#' @param x list: A list containing data frames created with the \code{\link{import_ORFID}} function to be combined.
#' @details Data frame is created in the user environment. The output of \code{\link{join_multireader_data}} will contain a column if that column appears in any of data frames combined. A factor class variable called LOC (from locus) is the combination of SCD (Site Code) and ANT (Antenna) variables. It should be used as location in further statistical analysis once it is the individual detection spot.
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

###############################################################################

join_multireader_data <- function(x){
    
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
    
    message("A factor class variable called LOC (from locus) is the combination of SCD (Site Code) and ANT (Antenna) variables. It should be used as location in further statistical analysis once it is the individual detection spot.")
    
    PIT_data_array <- y %>%
        mutate(ANT = replace_na(ANT, 1)) %>%
        mutate(LOC = paste0(SCD,ANT)) %>%
        mutate(SCD = as.factor(SCD)) %>%
        mutate(ANT = as.factor(ANT)) %>%
        mutate(LOC = as.factor(LOC))
    
    Sys.sleep(2)
    
    return(glimpse(PIT_data_array))
        
}

###############################################################################