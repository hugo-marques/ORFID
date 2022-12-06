#' @title Summarize site information from Oregon RFID antenna reader data
#' @description Summarizes detection information for unique antenna sites within antenna reader data compiled using \code{\link{import_ORFID}} or \code{\link{join_multireader_data}}. 
#' @param x antenna data compiled using \code{\link{import_ORFID}} or \code{\link{join_multireader_data}}.
#' @details Creates a tibble grouped by \emph{SCD} (site code; one row per unique \emph{SCD}). The data frame contains the site code (\emph{SCD}), the total number of records detected (\emph{REC}), the number of unique tags detected (\emph{TAG_ID}), and the time at which the first (\emph{FIR}) and last (\emph{LAS}) detections occurred on the array. 
#' @return Returns a tibble object.
#' @author Hugo Marques <biohmarques@@gmail.com>
#' @seealso 
#' \code{\link{import_ORFID}} for importing data files from Oregon RFID ORMR and ORSR antenna readers.
#' 
#' \code{\link{join_multireader_data}} for combining data from Oregon RFID ORMR and ORSR antenna readers into a multi-reader array.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' 
#' # Create a list containing compiled reader data:
#' readers <- list(reader_1, reader_2)
#' 
#' # Join data into a multi-reader array:
#' PIT_data <- join_multireader_data(readers)
#' 
#' # Summarize detection information for each unique site:
#' site_summary(PIT_data)

site_summary <- function(x) {
    
    if (!("SCD" %in% names(x))) {
        stop("Site code (SCD) is required.")
    }
    
    if (("LOC" %in% names(x))) {
        
        site <- x %>%
            dplyr::group_by(LOC) %>%
            dplyr::add_count(LOC) %>%
            dplyr::rename(REC = n) %>%
            dplyr::mutate(TAG_ID = dplyr::n_distinct(TAG)) %>%
            dplyr::mutate(FIR = min(ARR)) %>% 
            dplyr::mutate(LAS = max(ARR)) %>%
            dplyr::distinct(LOC, .keep_all = T) %>%
            dplyr::select(LOC, REC, TAG_ID, FIR, LAS)
        
    } else {
        
        site <- x %>%
            dplyr::group_by(SCD) %>%
            dplyr::add_count(SCD) %>%
            dplyr::rename(REC = n) %>%
            dplyr::mutate(TAG_ID = dplyr::n_distinct(TAG)) %>%
            dplyr::mutate(FIR = min(ARR)) %>% 
            dplyr::mutate(LAS = max(ARR)) %>%
            dplyr::distinct(SCD, .keep_all = T) %>%
            dplyr::select(SCD, REC, TAG_ID, FIR, LAS)
        
    }
    
    return(site)
    
}
