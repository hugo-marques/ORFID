#' @title Summarizing direction of movement for \code{\link{join_multireader_data}} 
#' @description Function for determining direction of tag movement in systems where multiple antennas are used along a linear migration route. 
#' @param x data frame to be analyzed.
#' @param LOC_vec vector of antenna locations from downstream to upstream.
#' @details \code{tag_direction} determines the direction of movement for each individual tag detection in \emph{x}. Direction is determined based on the order of locations from downstream to upstream, as specified in \emph{LOC_vec}. Note that direction cannot be determined until the tag has been detected at multiple locations. Use \code{\link{site_summary}} to identify all locations present in the multi reader data, which must be included in \emph{LOC_vec}. 
#' @return Returns a tibble object. The column \emph{DIR} displays direction, where \emph{U} is upstream movement, \emph{D} is downstream movement, and \emph{S} is no movement, or a consecutive detection at the previous location. 
#' @author Annika Putt
#' @seealso 
#' \code{\link{import_ORFID}} for importing data files from the new generation of Oregon RFID readers
#' \code{\link{join_multireader_data}} for combining data from ORFID readers into a multi reader array
#' \code{\link{site_summary}} for identifying all locations present in a multi reader array
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
#' ##  Example of format for determining tag direction.
#' tag_direction(PIT_data, c("BRA", "USA"))
#' }

tag_direction <- function(x, LOC_vec) {
  
  if(("LOC" %in% names(x))) {
    
    x2 <- data.frame(LOC = LOC_vec) %>% 
      dplyr::mutate(LOC_NUM = as.numeric(row.names(.))) %>% 
      dplyr::right_join(., x, by = "LOC") %>% 
      dplyr::group_by(TAG) %>% 
      dplyr::arrange(ARR) %>% 
      dplyr::mutate(DIR = ifelse(c(0, diff(LOC_NUM)) > 0, "U",
                                 ifelse(c(0, diff(LOC_NUM)) < 0, "D",
                                        "S"))) %>% 
      dplyr::ungroup()
    
  }
  
  if(!("LOC" %in% names(x))) {
    
    stop("LOC is required. Input data must be a product of join_multireader_data()")
    
  }
  
  return(x2)
  
}
