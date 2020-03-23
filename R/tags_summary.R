#' @title Summarizing tag-specific info of \code{\link{import_ORFID}}
#' @description Function for getting info about the tags recorded in a PIT_data data frame from \code{\link{import_ORFID}} function (number of unique tags, tags type and records per tag).
#' @param x data frame to be analyzed.
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
#' 
#' ##  Create the list containing the imported files:
#' readers <- list(reader_1, reader_2)
#' 
#' ##  Combine the files:
#' PIT_data <- join_multireader_data(readers)
#' 
#' ##  Analyzing tag-specific information in a PIT_data.
#' tags_summary(PIT_data)
#' }


###############################################################################

tags_summary <- function(x){
    
    if(!("TAG" %in% names(x))){
        stop("The Tag number (TAG) is required.")
    }
    
    if("ARR" %in% names(x)){
        PIT_arr <- x %>%
            dplyr::group_by(TAG) %>%
            dplyr::arrange(ARR) %>%
            dplyr::mutate(FIR = dplyr::first(ARR)) %>%
            dplyr::mutate(LAS = dplyr::last(ARR)) %>%
            dplyr::distinct(TAG, .keep_all = T) %>%
            dplyr::select(TAG, FIR, LAS)
    }
    
    if("DUR" %in% names(x)){
        PIT_dur <- x %>%
            dplyr::group_by(TAG) %>%
            dplyr::summarize(mean_DUR = round(mean(DUR), digits = 1))
    }
    
    if("LOC" %in% names(x)){
        PIT_loc <- x %>%
            dplyr::group_by(TAG) %>%
            dplyr::arrange(ARR) %>%
            dplyr::mutate(first_LOC = dplyr::first(LOC)) %>%
            dplyr::mutate(last_LOC = dplyr::last(LOC)) %>%
            dplyr::mutate(intermediate_LOC = dplyr::nth(LOC, ceiling(length(LOC)/2))) %>%
            dplyr::mutate(other_LOC = any(LOC != dplyr::first(LOC))) %>%
            dplyr::distinct(TAG, .keep_all = T) %>%
            dplyr::select(TAG, first_LOC, intermediate_LOC, other_LOC, last_LOC)
    }
    
    PIT_n <- x %>%
        dplyr::group_by(TAG) %>%
        dplyr::add_count(TAG) %>%
        dplyr::distinct(TAG, .keep_all = T) %>%
        dplyr::rename(REC = n)
    
    if("TTY" %in% names(x)){
        PIT <- PIT_n %>%
            dplyr::select(TAG, TTY, REC)
        
    } else {
        PIT <- PIT_n %>%
            dplyr::select(TAG, REC)
    }
    
    if("ARR" %in% names(x)){
        PIT <- PIT %>%
            dplyr::left_join(PIT_arr, by = "TAG") %>%
            dplyr::ungroup()
    }
    
    if("DUR" %in% names(x)){
        PIT <- PIT %>%
            dplyr::left_join(PIT_dur, by = "TAG")
    }
    
    if("LOC" %in% names(x)){
        PIT <- PIT %>%
            dplyr::left_join(PIT_loc, by = "TAG") %>%
            dplyr::ungroup()
    }
    
    message("
        TAG: tag ID
        TTY: tag type (A = Animal (ICAR), R = Read-only, W = Writable, P = Phantom)
        REC: number of records
        FIR: first record
        LAS: last record
        mean_DUR: mean detection duration
        first_LOC: first registered location
        intermediate_LOC: location of the intermediate detection
        other_LOC: if the tag was detected in a location other than the initial one, TRUE, otherwise FALSE
        last_LOC: last registered location
        
        ")
    
    
    return(PIT)
    
    

} # Function ends

###############################################################################