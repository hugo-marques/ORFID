#' @title Summarizing site-specific info of \code{\link{import_ORFID}}
#' @description Function for getting info about the tsites in a PIT_data data frame from \code{\link{import_ORFID}} function (number of records, number of tags detected and last tag seen per site).
#' @param x data frame to be analyzed.
# #' @param tag variable of x with the tag ID
# #' @param time variable of x with the detection time. For ORFID data it can be the either arrival (default = ARR) or departure (default = DEP).
# #' @param type variable of x with the tag type (ORFID default = TTY)
#' @details A data frame is created in the user environment with the data grouped by the site code.
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
#' ##  Summarizing site info
#' site_summary(PIT_data)
#' }


###############################################################################

site_summary <- function(x){
    
    site <- x %>%
        dplyr::group_by(LOC) %>%
        dplyr::add_count(LOC) %>%
        dplyr::rename(REC = n) %>%
        dplyr::mutate(TAG_ID = dplyr::n_distinct(TAG)) %>%
        dplyr::distinct(LOC, .keep_all = T) %>%
        dplyr::mutate(LAS = dplyr::last(TAG)) %>%
        dplyr::select(LOC, REC, TAG_ID, LAS)
    
    return(site)
    
}

###############################################################################

###############################################################################
