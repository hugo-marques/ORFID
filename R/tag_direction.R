#' @title Movement direction for Oregon RFID antenna data  
#' @description Determines direction of tag movement in systems where multiple antennas are used along a linear migration route. 
#' @param x data frame generated using \code{\link{join_multireader_data}}.
#' @param LOC_vec vector of antenna locations from first encountered to last encountered.
#' @details \code{tag_direction} determines the direction of movement for individual detection events in \emph{x}. Direction is determined based on the order of locations from first encountered to last encountered, as specified in \emph{LOC_vec}. Note that direction cannot be determined until the tag has been detected at multiple locations. Use \code{\link{site_summary}} to identify all locations present in the multi-reader data, which must be included in \emph{LOC_vec}. 
#' @return Returns a tibble object. The column \emph{DIR} displays direction, where \emph{U} is upstream movement, \emph{D} is downstream movement, and \emph{S} is no movement, or a consecutive detection at the previous location. 
#' @author Annika Putt <annika@@instream.net>
#' @seealso 
#' \code{\link{import_ORFID}} for importing data files from Oregon RFID ORMR and ORSR antenna readers.
#' 
#' \code{\link{join_multireader_data}} for combining data from Oregon RFID ORMR and ORSR antenna readers into a multi-reader array.
#' 
#' \code{\link{site_summary}} for identifying all locations present in a multi reader array
#' @importFrom magrittr %>%
#' @export
#' @examples
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
#' tag_direction(PIT_data, c("downstream_A1", "upstream_A1"))
#' 
#' @importFrom rlang .data

tag_direction <- function(x, LOC_vec) {
    
    if (length(unique(x$LOC)) != length(LOC_vec)) {
        warning("Dataframe LOC values do not match LOC_vec values")
    }
    
    if (all(unique(x$LOC) %in% LOC_vec != TRUE)) {
        stop("Dataframe LOC values do not match LOC_vec values")
    }
    
    if (("LOC" %in% names(x))) {
        
        x2 <- data.frame(LOC = LOC_vec) %>% 
            dplyr::mutate(LOC_NUM = as.numeric(row.names(.))) %>% 
            dplyr::right_join(., x, by = "LOC") %>% 
            dplyr::group_by(TAG) %>% 
            dplyr::arrange(ARR) %>% 
            dplyr::mutate(DIR = ifelse(c(0, diff(.data$LOC_NUM)) > 0, "U",
                                       ifelse(c(0, diff(.data$LOC_NUM)) < 0, "D",
                                              "S"))) %>% 
            dplyr::ungroup() %>% 
            dplyr::arrange(ARR, TAG)
        
    }
    
    if(!("LOC" %in% names(x))) {
        
        stop("LOC is required. Input data must be a product of join_multireader_data()")
        
    }
    
    return(x2)
    
}
