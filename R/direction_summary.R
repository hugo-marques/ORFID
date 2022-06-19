#' @title Summarizing time difference between first and last directions in \code{\link{tag_direction}} 
#' @description Function for summarizing the time between the first and last movement directions for each unique tag id. Input data are created by \code{\link{tag_direction}}, which determines the direction of movement for each detection event in \code{\link{join_multireader_data}}.
#' @param dir_df output from \code{\link{tag_direction}}
#' @details \code{direction_summary} isolates the first and last direction of movement from \code{\link{tag_direction}} and determines the time difference in seconds and days. 
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
#' readers <- list(reader_1, reader_2)
#' 
#' # Join data into a multi-reader array:
#' PIT_data <- join_multireader_data(readers)
#' 
#' # Determine tag direction:
#' dir <- tag_direction(PIT_data, c("BRA", "USA"))
#' 
#' # Determine the time difference between first and last detections:
#' dir_summary <- direction_summary(dir)
#' }

direction_summary <- function(dir_df) {
  
  if(!"DIR" %in% colnames(dir_df)) {
    warning("Input data must be output from tag_direction()")
    stop()
  }
  
  x <- dir_df %>% 
    filter(DIR != "S") %>% 
    group_by(TAG) %>% 
    filter(ARR == min(ARR) | ARR == max(ARR)) %>% 
    arrange(ARR) %>% 
    mutate(first_DET = min(ARR),
           first_LOC = first(LOC),
           first_DIR = first(DIR),
           last_DET = max(ARR),
           last_LOC = last(LOC),
           last_DIR = last(DIR),
           tdiff_sec = difftime(last_DET, first_DET),
           tdiff_day = round(as.numeric(tdiff_sec, units = "days"), 1)) %>% 
    ungroup() %>% 
    # Select only pertinent columns, which results in a df with 2 identical rows
    select(TAG, first_DET, first_LOC, first_DIR, last_DET, last_LOC, last_DIR, tdiff_sec, tdiff_day) %>% 
    distinct() # Get rid of duplicate rows
  
  return(x)
  
}
