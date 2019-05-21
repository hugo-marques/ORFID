#' @title Summarizing tag-specific info of \code{\link{import_ORFID}}
#' @description Function for getting info about the tags recorded in a PIT_data data frame from \code{\link{import_ORFID}} function (number of unique tags, tags type and records per tag).
#' @param x data frame to be analyzed.
#' @param tag variable of x with the tag ID
#' @param time variable of x with the detection time. For ORFID data it can be the either arrival (default = ARR) or departure (default = DEP).
#' @param type variable of x with the tag type (ORFID default = TTY)
#' @details A data frame is created in the user environment with the data grouped by TAG IDs.
#' @return Returns a tibble object.
#' @author Hugo Marques <biohmarques@@gmail.com>
#' @seealso 
#' \code{\link{import_ORFID}} for importing data files from the new generation of Oregon RFID readers
#' \code{\link{join_multireader_data}} for Combining data from ORFID readers into an multi readers array
#' @export
#' @examples
#' 
#' ##  Analyzing tag-specific information in a PIT_data.
#'  
#' tag_info(reader_1)
#' 
#' ##  Create the list containing the imported files:
#' readers list(reader_1, reader_2, reader_3)
#' 
#' ##  Combine the files:
#' array <- join_multireader_data(readers)
#' 
#' ##  Summarizing tag info
#' 
#' tag_info(array)
#' 


###############################################################################

tag_info <- function(x, tag, time, type){
    
    #attach(x)
    
    if(!(tag %in% names(x))){
        stop("The Tag number (TAG) is required.")
    }
    
    PIT <- x %>%
        dplyr::group_by(tag) %>%
        dplyr::add_count(tag) %>%
        dplyr::distinct(tag, .keep_all = T) %>%
        dplyr::select(tag, n) %>%
        dplyr::rename(REC = n) %>%
        dplyr::mutate(FIR = first(time)) %>%
        dplyr::mutate(LAS = last(time))
    
        if(missing(type)){
            
            PIT <- PIT %>%
                dplyr::select(tag, REC, FIR, LAS)
            
            message("
                    TAG: tag ID
                    REC: number of records
                    FIR: first record
                    LAS: last record
                    ")
            
    } else {
        if(type %in% names(PIT)){
            
            PIT <- PIT %>%
                dplyr::select(tag, type, REC, FIR, LAS)
            
            message("
                    TAG: tag ID
                    TTY: tag type (A = Animal (ICAR), R = Read-only, W = Writable, P = Phantom)
                    REC: number of records
                    FIR: first record
                    LAS: last record
                    ")
            
        } else {
            PIT <- PIT %>%
                dplyr::select(tag, REC, FIR, LAS)
            
            message("
                    TAG: tag ID
                    REC: number of records
                    FIR: first record
                    LAS: last record
                    ")
        }

    }
        
    #detach(x)    

    
} # Function ends

###############################################################################