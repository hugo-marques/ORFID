#' @title Summarizing tag-specific info of \code{\link{import_ORFID}}
#' @description Function for getting info about the tags recorded in a PIT_data data frame from \code{\link{import_ORFID}} function (number of unique tags, tags type and records per tag).
#' @param x PIT_data data frame to be analyzed.
#' @details A data frame is created in the user environment with the data grouped by TAG IDs.
#' @return Returns a tibble object.
#' @author Hugo Marques <biohmarques@@gmail.com>
#' @examples
#' 
#' ## Analyzing tag-specific information in a PIT_data.
#'  
#' \run{} tag_info(reader_1)
#' 
#' ## Create the list containing the imported files:
#' \run{} readers list(reader_1, reader_2, reader_3)
#' 
#' ## Combine the files:
#' \run{} array <- join_multireader_data(readers)
#' 
#' ## Summarizing tag info
#' 
#' \run{} tag_info(array)
#'  
#' @export

###############################################################################

tag_info <- function(x){
    
    if(!("TAG" %in% names(x))){
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
        
        if("TTY" %in% names(PIT)){
            PIT <- PIT %>%
                select(TAG, TTY, REC, FIR, LAS)
            message("
TAG: tag ID
TTY: tag type (A = Animal (ICAR), R = Read-only, W = Writable, P = Phantom)
REC: number of records
FIR: first record
LAS: last record
")
            
        } else {
            PIT <- PIT %>%
                select(TAG, REC, FIR, LAS)
            
            message("
TAG: tag ID
REC: number of records
FIR: first record
LAS: last record
")
            
        }
    
                    
    } else {
        if("TTY" %in% names(PIT)){
            PIT <- PIT %>%
                select(TAG, TTY, REC)
            
            message("
TAG: tag ID
TTY: tag type (A = Animal (ICAR), R = Read-only, W = Writable, P = Phantom)
REC: number of records
")
            
        } else {
            PIT <- PIT %>%
                select(TAG, REC)
            
            message("
TAG: tag ID
REC: number of records
")
            
        }
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