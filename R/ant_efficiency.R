#' @title Detection efficiency for directional Oregon RFID antenna data 
#' @description Determines detection efficiency for each antenna in systems where multiple antennas are used along a linear migration route. 
#' @param x data frame generated using \code{\link{join_multireader_data}}.
#' @param LOC_vec vector of antenna locations from first encountered to last encountered.
#' @details \code{ant_efficiency} determines the detection efficiency of each antenna in a linear migration route. Direction is determined based on the order of locations from first encountered to last encountered, as specified in \emph{LOC_vec}. Use \code{\link{site_summary}} to identify all locations present in the multi reader data, which must be included in \emph{LOC_vec}. 
#' 
#' Antenna efficiency is determined by identifying which tags were detected at antenna x and which tags were detected anywhere after/above antenna x. The efficiency of antenna x is then the number of shared tag detections divided by the total number of detections after x. Note that efficiency and shared detections cannot be determined for the final antenna as there are no subsequent detections. Reversing the order of \emph{LOC_vec} can inform efficiency in systems with movement in multiple directions.
#' @return Returns a tibble object. 
#' @author Annika Putt <annika@@instream.net>
#' @seealso 
#' \code{\link{import_ORFID}} for importing data files from Oregon RFID ORMR and ORSR antenna readers.
#' 
#' \code{\link{join_multireader_data}} for combining data from Oregon RFID ORMR and ORSR antenna readers into a multi-reader array.
#' 
#' \code{\link{site_summary}} for identifying all locations present in a multi-reader array.
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
#' # Determine antenna efficiency for animals moving from downstream to upstream:
#' ant_efficiency(PIT_data, c("downstream_A1", "upstream_A1"))
#' 
#' # Determine antenna efficiency for animals moving from upstream to downstream:
#' ant_efficiency(PIT_data, c("upstream_A1", "downstream_A1"))
#'  
#' @importFrom rlang .data

ant_efficiency <- function(x, LOC_vec) {
  
    if (length(unique(x$LOC)) != length(LOC_vec)) {
        warning("Dataframe LOC values do not match LOC_vec values")
    }
    
    if (all(unique(x$LOC) %in% LOC_vec != TRUE)) {
        stop("Dataframe LOC values do not match LOC_vec values")
    }
  
  if(("LOC" %in% names(x))) {
    
    x2 <- data.frame(LOC = LOC_vec) %>% 
      dplyr::mutate(LOC_NUM = as.numeric(row.names(.))) %>% 
      dplyr::right_join(., x, by = "LOC") 
    
    out <- list()
    
    for (i in 1:length(unique(x2$LOC_NUM))) {
      
      # Which tags are at antenna x?
      tags_at_ant <- x2 %>% 
        dplyr::filter(.data$LOC_NUM == i) %>% 
        dplyr::distinct(TAG)
      
      # Which tags are above antenna x?
      tags_above <- x2 %>% 
        dplyr::filter(.data$LOC_NUM > i) %>% 
        dplyr::distinct(TAG)
      
      shared <- sum(tags_at_ant$TAG %in% tags_above$TAG)
      
      out[[i]] <- data.frame(LOC = LOC_vec[i],
                             LOC_NUM = unique(x2$LOC_NUM)[i],
                             shared_tags = shared,
                             tags_above = nrow(tags_above),
                             det_eff = round(shared/nrow(tags_above), 2))
      
    }
    
  }
  
  if(!("LOC" %in% names(x))) {
    stop("LOC is required. Input data must be a product of join_multireader_data()")
  }
  
  return(dplyr::bind_rows(out))
  
}
