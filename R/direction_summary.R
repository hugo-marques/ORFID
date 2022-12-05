#' @title Summarizing time difference between first and last directions in \code{\link{tag_direction}} 
#' @description Function for summarizing the time difference between the first and last movement directions for each unique tag id. Input data are created by \code{\link{tag_direction}}, which determines the direction of movement for each detection event in \code{\link{join_multireader_data}}.
#' @param dir_df output from \code{\link{tag_direction}}
#' @param include_stationary if TRUE, all detections are summarized. If FALSE, only detections with a known movement direction (up or down) are included in the summary.
#' @details \code{direction_summary} isolates the first and last direction of movement from \code{\link{tag_direction}} and determines the time difference in seconds and days. Directions are U for upstream movement, D for downstream movement, and S for no movement, or consecutive detection at the same location.
#' 
#' It is common for a tag to be detected multiple times at the same antenna, which will result in a movement direction of S, or stationary. The user is encouraged to examine direction summaries for `include_stationary = TRUE` and `include_stationary = FALSE` to become familiar with their data. When `include_stationary = FALSE`, there may be fewer tag ids in the direction summary than in the full data set. 
#' @return Returns a tibble object. 
#' @author Annika Putt <annika@@instream.net>
#' @seealso 
#' \code{\link{import_ORFID}} for importing data files from Oregon RFID ORMR and ORSR antenna readers.
#' 
#' \code{\link{join_multireader_data}} for combining data from Oregon RFID ORMR and ORSR antenna readers into a multi-reader array.
#' 
#' \code{\link{tag_direction}} for determining movement direction of detections in systems with a linear migration route.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' 
#' # Create a list containing compiled reader data:
#' readers <- list(reader_us, reader_ds)
#' 
#' # Join data into a multi-reader array:
#' PIT_data <- join_multireader_data(readers)
#' 
#' # List readers:
#' unique(PIT_data$LOC)
#' 
#' # Determine tag direction for animals moving from downstream to upstream:
#' dir <- tag_direction(PIT_data, c("downstream_A1", "upstream_A1"))
#'  
#' # Determine the time difference between first and last detections:
#' dir_summary <- direction_summary(dir)
#' }
#' 
#' @importFrom rlang .data

direction_summary <- function(dir_df, 
                              include_stationary = FALSE) {
    
    if(!"DIR" %in% colnames(dir_df)) {
        stop("Input data must be output from tag_direction()")
    }
    
    if (include_stationary == TRUE) {
        
        x <- dir_df %>% 
            dplyr::group_by(.data$TAG) %>% 
            dplyr::filter(ARR == min(.data$ARR) | ARR == max(.data$ARR)) %>% 
            dplyr::arrange(ARR) %>% 
            dplyr::mutate(
                first_DET = min(.data$ARR),
                first_LOC = dplyr::first(.data$LOC),
                first_DIR = dplyr::first(.data$DIR),
                last_DET = max(.data$ARR),
                last_LOC = dplyr::last(.data$LOC),
                last_DIR = dplyr::last(.data$DIR),
                tdiff_sec = difftime(.data$last_DET, .data$first_DET),
                tdiff_day = round(as.numeric(.data$tdiff_sec, units = "days"), 1)) %>% 
            dplyr::ungroup() %>% 
            # Select only pertinent columns, which results in a df with 2 identical rows
            dplyr::select(.data$TAG, .data$first_DET, .data$first_LOC, .data$first_DIR, .data$last_DET, .data$last_LOC, .data$last_DIR, .data$tdiff_sec, .data$tdiff_day) %>% 
            dplyr::distinct() # Get rid of duplicate rows
        
    } else {
        
        x <- dir_df %>% 
            dplyr::filter(.data$DIR != "S") %>%             
            dplyr::group_by(.data$TAG) %>% 
            dplyr::filter(ARR == min(.data$ARR) | ARR == max(.data$ARR)) %>% 
            dplyr::arrange(ARR) %>% 
            dplyr::mutate(
                first_DET = min(.data$ARR),
                first_LOC = dplyr::first(.data$LOC),
                first_DIR = dplyr::first(.data$DIR),
                last_DET = max(.data$ARR),
                last_LOC = dplyr::last(.data$LOC),
                last_DIR = dplyr::last(.data$DIR),
                tdiff_sec = difftime(.data$last_DET, .data$first_DET),
                tdiff_day = round(as.numeric(.data$tdiff_sec, units = "days"), 1)) %>% 
            dplyr::ungroup() %>% 
            # Select only pertinent columns, which results in a df with 2 identical rows
            dplyr::select(.data$TAG, .data$first_DET, .data$first_LOC, .data$first_DIR, .data$last_DET, .data$last_LOC, .data$last_DIR, .data$tdiff_sec, .data$tdiff_day) %>% 
            dplyr::distinct() # Get rid of duplicate rows
        
    }
    
    return(x)
    
}
