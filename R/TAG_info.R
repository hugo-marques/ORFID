#' @title Summarizing tag-specific info of \code{\link{import_ORFID}}
#' @description Function for getting info about the tags recorded in a PIT_data data frame from \code{\link{import_ORFID}} function (number of unique tags, tags type and records per tag).
#' @param x PIT_data data frame to analyzed.
#' @details A data frame is created in the user environment with the data grouped by TAG IDs.
#' @return Returns a tibble object.
#' @author Hugo Marques <biohmarques@@gmail.com>
#' @examples
#' 
#' #Analyzing tag-specific information in a PIT_data.
#'  
#' \dontrun{} tags <- TAG_info(PIT_data)
#' 
#' # How many tags were detected? 
#' \dontrun{} lenght(tags$TAG)
#'  
#' @export

###############################################################################

TAG_info <- function(x){
    
    if(!("TAG" %in% names(raw_data))){
        stop("The Tag number (TAG) is required.")
    }
    
    PIT <- x %>%
        group_by(TAG) %>%
        add_count(TAG) %>%
        distinct(TAG, .keep_all = T) %>%
        select(TAG, n) %>%
        rename(REC = n)
    
    if("ARR" %in% names(PIT)){
        PIT <- PIT %>%
            mutate(FIR = first(ARR)) %>%
            mutate(LAS = last(ARR))
    }
    
    if("TTY" %in% names(PIT)){
        PIT <- PIT %>%
            select(TAG, TTY, REC, FIR, LAS)
    }
    
    message("
TAG: tag ID
TTY: tag type (A = Animal (ICAR), R = Read-only, W = Writable, P = Phantom)
REC: number of records
FIR: first record
LAS: last record
")
    
    Sys.sleep(1)
    
    return(PIT)
    
}

###############################################################################