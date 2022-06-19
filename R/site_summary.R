#' @title Summarize site information from Oregon RFID ORMR and ORSR antenna reader data
#' @description Summarizes detection information for unique antenna sites within antenna reader data compiled using \code{\link{import_ORFID}} or \code{\link{join_multireader_data}}. 
#' @param x antenna data compiled using \code{\link{import_ORFID}} or \code{\link{join_multireader_data}}.
#' @details Creates a tibble grouped by SCD (site code; one row per unique SCD). The data frame contains the site code (SCD), the total number of records detected (REC), the number of unique tags detected (TAG_ID), and the last tag detected (LAS).
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
#' # Summarize detection information for each unique site:
#' site_summary(PIT_data)
#' }

site_summary <- function(x) {
    
    if (("LOC" %in% names(x))) {
        
        site <- x %>%
            dplyr::group_by(LOC) %>%
            dplyr::add_count(LOC) %>%
            dplyr::rename(REC = n) %>%
            dplyr::mutate(TAG_ID = dplyr::n_distinct(TAG)) %>%
            dplyr::distinct(LOC, .keep_all = T) %>%
            dplyr::mutate(LAS = dplyr::last(TAG)) %>%
            dplyr::select(LOC, REC, TAG_ID, LAS)
        
    } else {
        
        site <- x %>%
            dplyr::group_by(SCD) %>%
            dplyr::add_count(SCD) %>%
            dplyr::rename(REC = n) %>%
            dplyr::mutate(TAG_ID = dplyr::n_distinct(TAG)) %>%
            dplyr::distinct(SCD, .keep_all = T) %>%
            dplyr::mutate(LAS = dplyr::last(TAG)) %>%
            dplyr::select(SCD, REC, TAG_ID, LAS)
        
    }
    
    return(site)
    
}