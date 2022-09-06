#' @title Summarize marker tag data from \code{\link{import_ORFID}}
#' @description Creates a filtered dataset containing only detection data from a user-specified marker tag (unique tag number).
#' @param x antenna data compiled using \code{\link{import_ORFID}} or \code{\link{join_multireader_data}}.
#' @param tag marker tag identification (character object).
#' @param gap minimum time gap in seconds between detections (optional). 
#' @details A column, GAP, is created that calculates the time gap (in seconds) between subsequent detections. If the gap argument is omitted, all detection from the marker tag are retained. If a minimum time gap is specified, only detections with a larger time gap than the specified minimum are retained. This allows the user to identify periods when marker tags were not being detected as frequently as expected. 
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
#' readers <- list(reader_1, reader_2, reader_3)
#' 
#' # Join data into a multi-reader array:
#' PIT_data <- join_multireader_data(readers)
#' 
#' # Summarize marker tag data
#' marker_tag(PIT_data, "0000_000000004978")
#' 
#' # Summarize marker tag data when the time gap between detections was greater or equal to 10 minutes.
#' marker_tag(PIT_data, "0000_000000004978", gap = 600)
#' }

marker_tag <- function(x, tag, gap) {
    
    if(missing(gap)) {
        marker_tag <- x %>%
            dplyr::filter(TAG == tag) %>%
            dplyr::mutate(arr = ARR) %>%
            dplyr::mutate_at("arr", list(~. - lag(.))) %>%
            dplyr::mutate(GAP = round(arr, digits = 1)) %>%
            dplyr::select(TAG, ARR, DUR, GAP, NCD)
        
    } else {
        
        marker_tag <- x %>%
            dplyr::filter(TAG == tag) %>%
            dplyr::mutate(arr = ARR) %>%
            dplyr::mutate_at("arr", list(~. - lag(.))) %>%
            dplyr::mutate(GAP = round(arr, digits = 1)) %>%
            dplyr::select(TAG, ARR, DUR, GAP, NCD) %>%
            dplyr::filter(GAP >= gap)
        
    }
    
    marker_tag
    
}