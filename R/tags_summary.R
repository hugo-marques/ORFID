#' @title Summarizing tag-specific info of \code{\link{import_ORFID}}
#' @description Function for getting info about the tags recorded in a PIT_data data frame from \code{\link{import_ORFID}} function (number of unique tags, tags type and records per tag).
#' @param x data frame to be analyzed.
# #' @param tag variable of x with the tag ID
# #' @param time variable of x with the detection time. For ORFID data it can be the either arrival (default = ARR) or departure (default = DEP).
# #' @param type variable of x with the tag type (ORFID default = TTY)
#' @details A data frame is created in the user environment with the data grouped by TAG IDs.
#' @return Returns a tibble object.
#' @author Hugo Marques <biohmarques@@gmail.com>
#' @seealso 
#' \code{\link{import_ORFID}} for importing data files from the new generation of Oregon RFID readers
#' \code{\link{join_multireader_data}} for Combining data from ORFID readers into an multi readers array
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' ##  Analyzing tag-specific information in a PIT_data.
#' tag_info(reader_1)
#' 
#' ##  Create the list containing the imported files:
#' readers list(reader_1, reader_2, reader_3)
#' 
#' ##  Combine the files:
#' array <- join_multireader_data(readers)
#' 
#' ##  Summarizing tag info
#' tag_summary(array)
#' }


###############################################################################

tags_summary <- function(x){
    #, tag = "TAG", time = "ARR", type = "TTY"
    #attach(x)
    
    if(!("TAG" %in% names(x))){
        stop("The Tag number (TAG) is required.")
    }
    
    PIT <- x %>%
        dplyr::group_by(TAG) %>%
        dplyr::add_count(TAG) %>%
        dplyr::distinct(TAG, .keep_all = T) %>%
        #dplyr::select(TAG, n) %>%
        dplyr::rename(REC = n) %>%
        dplyr::mutate(FIR = dplyr::first(ARR)) %>%
        dplyr::mutate(LAS = dplyr::last(ARR))
    
    
    if("TTY" %in% names(PIT)){
        
        PIT <- PIT %>%
            dplyr::select(TAG, TTY, REC, FIR, LAS)
        
        message("
                TAG: tag ID
                TTY: tag type (A = Animal (ICAR), R = Read-only, W = Writable, P = Phantom)
                REC: number of records
                FIR: first record
                LAS: last record
                ")
            
        } else {
            PIT <- PIT %>%
                dplyr::select(TAG, REC, FIR, LAS)
            
            message("
                    TAG: tag ID
                    REC: number of records
                    FIR: first record
                    LAS: last record
                    ")
        }
    
    return(PIT)

} # Function ends

###############################################################################