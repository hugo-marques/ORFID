#' @title Summarizing tag-specific info of \code{\link{import_ORFID}}.
#'
#' @description Function allows users to combine unique readers into an array, using the data obtained from \code{\link{import_ORFID}} function. It is necessary to create a list with all data frames to be joined. If the data frames have diferent variables,   
#' @param x list: containing data frames created from the \code{\link{import_ORFID}} function to combine.
#' @details Data frame is created in the user environment. The output of \code{\link{join_multireader_data}} will contain a column if that column appears in any of data frames combined.
#' @examples
#' #Analyzing tag-specific information in a PIT_data.
#' \dontrun{} TAG_info(PIT_data)
#' @export

join_multireader_data <- function(x) {
    if (class(x) != "list") {
        
        stop("The data frames should be in a list")
        
    } else if (any(names(x[[i]])!="SCD")) {
        
        stop("Missing site code (SCD) in at least one data frame")
    } else {
        
        df <- x %>%
            bind_rows()
    } 
    
    if (anyNA(df$ANT) == T) {
        warning("NA values in ANT were replaced by 1")
    }
    
    PIT_data_array <<- df %>%
        mutate(ANT = replace_na(ANT, 1)) %>%
        mutate(NAM = mutate(paste0(SCD,ANT)))
    
    return(glimpse(PIT_data_array))
}


###############################################################################