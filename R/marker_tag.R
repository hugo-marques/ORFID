#' @title Summarizing tag-specific info of \code{\link{import_ORFID}}
#' @description Function for getting info about the tags recorded in a PIT_data data frame from \code{\link{import_ORFID}} function (number of unique tags, tags type and records per tag).
#' @param x data frame to be analyzed.
#' @param tag data frame to be analyzed.
#' @param gap data frame to be analyzed.
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
#' tags_summary(reader_1)
#' 
#' ##  Create the list containing the imported files:
#' readers <- list(reader_1, reader_2)
#' 
#' ##  Combine the files:
#' array <- join_multireader_data(readers)
#' 
#' ##  Summarizing tag info
#' tags_summary(array)
#' }


###############################################################################

marker_tag <- function(x, tag, gap){
    
    if(missing(gap)){
        marker_tag <- x %>%
            filter(TAG == tag) %>%
            mutate(arr = ARR) %>%
            mutate_at("arr", list(~. - lag(.))) %>%
            mutate(GAP = round(arr, digits = 1)) %>%
            select(TAG, ARR, DUR, GAP, NCD)
        
    } else {
        
        marker_tag <- x %>%
            filter(TAG == tag) %>%
            mutate(arr = ARR) %>%
            mutate_at("arr", list(~. - lag(.))) %>%
            mutate(GAP = round(arr, digits = 1)) %>%
            select(TAG, ARR, DUR, GAP, NCD) %>%
            filter(GAP > gap)
        
    }
    
    marker_tag
    
}

###############################################################################

###############################################################################

