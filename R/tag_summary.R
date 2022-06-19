#' @title Summarize detections for each unique tag detected by Oregon RFID ORMR and ORSR antenna readers
#' @description Summarizes detection information for unique tags within antenna reader data compiled using \code{\link{import_ORFID}} or \code{\link{join_multireader_data}}. 
#' @param x antenna data compiled using \code{\link{import_ORFID}} or \code{\link{join_multireader_data}}.
#' @param verbose If TRUE, a data frame describing output columns is printed to the console.
#' @details Creates a tibble grouped by TAG (one row per unique TAG). A data frame describing the summarized data is printed to the console.
#' @return Returns a tibble object.
#' @author Hugo Marques <biohmarques@@gmail.com>
#' @seealso 
#' \code{\link{import_ORFID}} for importing data files from Oregon RFID ORMR and ORSR antenna readers.
#' 
#' \code{\link{join_multireader_data}} for combining data from Oregon RFID ORMR and ORSR antenna readers into a multi-reader array.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' 
#' # Create a list containing compiled reader data:
#' readers <- list(reader_1, reader_2)
#' 
#' # Join data into a multi-reader array:
#' PIT_data <- join_multireader_data(readers)
#' 
#' # Summarize detection information for each unique tag:
#' tag_summary(PIT_data)
#' }

tag_summary <- function(x, verbose = TRUE) {
    
    if (!("TAG" %in% names(x))) {
        stop("Tag number (TAG) is required.")
    }
    
    if ("ARR" %in% names(x)) {
        PIT_arr <- x %>% 
            dplyr::group_by(TAG) %>%
            dplyr::arrange(ARR) %>%
            dplyr::mutate(FIR = dplyr::first(ARR)) %>%
            dplyr::mutate(LAS = dplyr::last(ARR)) %>%
            dplyr::distinct(TAG, .keep_all = T) %>%
            dplyr::select(TAG, FIR, LAS)
    }
    
    if ("DUR" %in% names(x)) {
        PIT_dur <- x %>%
            dplyr::group_by(TAG) %>%
            dplyr::summarize(mean_DUR = round(mean(DUR), digits = 1))
    }
    
    if ("LOC" %in% names(x)) {
        PIT_loc <- x %>%
            dplyr::arrange(ARR) %>%
            dplyr::group_by(TAG) %>%
            dplyr::summarise(first_LOC = dplyr::first(LOC), 
                             other_LOC = any(!LOC %in% c(dplyr::first(LOC), dplyr::last(LOC))), 
                             last_LOC = dplyr::last(LOC))
    }
    
    PIT_n <- x %>%
        dplyr::group_by(TAG) %>%
        dplyr::add_count(TAG) %>%
        dplyr::distinct(TAG, .keep_all = T) %>%
        dplyr::rename(REC = n)
    
    if ("TTY" %in% names(x)) {
        PIT <- PIT_n %>%
            dplyr::select(TAG, TTY, REC)
        
    } else {
        PIT <- PIT_n %>%
            dplyr::select(TAG, REC)
    }
    
    if ("ARR" %in% names(x)) {
        PIT <- PIT %>%
            dplyr::left_join(PIT_arr, by = "TAG") %>%
            dplyr::ungroup()
    }
    
    if ("DUR" %in% names(x)) {
        PIT <- PIT %>%
            dplyr::left_join(PIT_dur, by = "TAG")
    }
    
    if ("LOC" %in% names(x)) {
        PIT <- PIT %>%
            dplyr::left_join(PIT_loc, by = "TAG") %>%
            dplyr::ungroup()
    }
    
    if (verbose == TRUE) { 
        
        message("
        TAG: tag ID number
        TTY: tag type (A = Animal (ICAR), R = Read-only, W = Writable, P = Phantom)
        REC: number of detection records
        FIR: first detection record
        LAS: last detection record
        mean_DUR: mean detection duration
        first_LOC: location of first detection 
        last_LOC: location of last detection
        other_LOC: was the tag detected at any additional locations?
        ")
        
    }
    
    return(PIT)

} 