#' @title Summarizing tag-specific info of \code{\link{import_ORFID}}
#' @description Function for getting info about the tags recorded in a PIT_data data frame from \code{\link{import_ORFID}} function (number of unique tags, tags type and records per tag).
#' @param x data frame to be analyzed.
#' @param tag marker tag number. Must be between "".
#' @param gap Value in seconds, if you want to filter only values greater than or equal to "gap".
#' @details A data frame is created with the data from the marker tag.
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
#' PIT_data <- join_multireader_data(readers)
#' 
#' ##  Summarizing marker tag data
#' mrker_tag(PIT_data)
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
            filter(GAP >= gap)
        
    }
    
    marker_tag
    
}

###############################################################################

###############################################################################

