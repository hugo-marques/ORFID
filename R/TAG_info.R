#' @title Summarizing tag-specific info of \code{\link{import_ORFID}}.
#'
#' @description Function for getting info about the tags recorded in a PIT_data data frame from \code{\link{import_ORFID}} function (number of unique tags, tags type and records per tag). 
#' @param x PIT_data data frame to analyzed.
#' @details DAta frame is created in the user environment.
#' @examples
#' #Analyzing tag-specific information in a PIT_data.
#' \dontrun{} TAG_info(PIT_data)
#' @export

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
