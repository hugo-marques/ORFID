#' @title Ploting marker tag detections of \code{\link{import_ORFID}}
#' @description Function for getting info about the tags recorded in a PIT_data data frame from \code{\link{import_ORFID}} function (number of unique tags, tags type and records per tag).
#' @param x data frame to be analyzed.
#' @param tag marker tag number. Must be between "".
#' @param gap Value in seconds, if you want to filter only values greater than or equal to "gap".
#' @details A plot is created with the data from the marker tag.
#' @return Returns a plot from the marker tag data.
#' @author Hugo Marques <biohmarques@@gmail.com>
#' @seealso 
#' \code{\link{import_ORFID}} for importing data files from the new generation of Oregon RFID readers
#' \code{\link{join_multireader_data}} for Combining data from ORFID readers into an multi readers array
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' 
#' ##  Create the list containing the imported files:
#' readers <- list(reader_1, reader_2, reader_3)
#' 
#' ##  Combine the files:
#' PIT_data <- join_multireader_data(readers)
#' 
#' ##  Ploting marker tag data
#' mrker_tag_plot(PIT_data, "0000_000000004978")
#' mrker_tag_plot(PIT_data, "0000_000000004978", 360)
#' }


###############################################################################

marker_tag_plot <- function(x, tag, gap){
    
    if(missing(gap)){
        marker_tag <- x %>%
            dplyr::filter(TAG == tag) %>%
            dplyr::mutate(arr = ARR) %>%
            dplyr::mutate_at("arr", list(~. - lag(.))) %>%
            dplyr::mutate(GAP = round(arr, digits = 1)) %>%
            dplyr::select(TAG, ARR, DUR, GAP, NCD)
        
        plot <- ggplot2::ggplot() +
            ggplot2::geom_point(data = marker_tag, ggplot2::aes(x = ARR, y = TAG), size = .01) +
            ggplot2::xlab("Detection time")+
            ggplot2::ylab(unique(marker_tag$TAG))+
            ggplot2::scale_x_datetime(date_breaks = "1 hour", date_labels = "%m/%d %H:%M") +
            ggplot2::theme_bw() +
            ggplot2::theme(panel.background = ggplot2::element_blank(), 
                  panel.grid.major = ggplot2::element_blank(),
                  panel.grid.minor = ggplot2::element_blank(),
                  plot.background = ggplot2::element_blank(),
                  axis.text.y= ggplot2::element_blank(),
                  axis.ticks.y= ggplot2::element_blank(),
                  axis.text.x = ggplot2::element_text(angle=90, vjust = 0.5, hjust = 1.0))
        
    } else {
        
        marker_tag <- x %>%
            dplyr::filter(TAG == tag) %>%
            dplyr::mutate(arr = ARR) %>%
            dplyr::mutate_at("arr", list(~. - lag(.))) %>%
            dplyr::mutate(GAP = round(arr, digits = 1)) %>%
            dplyr::select(TAG, ARR, DUR, GAP, NCD)
        
        marker_tag_gap <- x %>%
            dplyr::filter(TAG == tag) %>%
            dplyr::mutate(arr = ARR) %>%
            dplyr::mutate_at("arr", list(~. - lag(.))) %>%
            dplyr::mutate(GAP = round(arr, digits = 1)) %>%
            dplyr::mutate(gap_start = ARR - GAP) %>%
            dplyr::mutate(gap_end = ARR) %>%
            dplyr::select(TAG, gap_start, gap_end, GAP) %>%
            dplyr::filter(GAP >= gap)
        
        plot <- ggplot2::ggplot() +
            ggplot2::geom_point(data = marker_tag, ggplot2::aes(x = ARR, y = TAG), size = .01) +
            ggplot2::xlab("Detection time")+
            ggplot2::ylab(unique(marker_tag$TAG))+
            ggplot2::scale_x_datetime(date_breaks = "1 hour", date_labels = "%m/%d %H:%M") +
            ggplot2::geom_linerange(data = marker_tag_gap, ggplot2::aes(xmin = gap_start, xmax = gap_end, y = TAG), colour = "red", size = 500, alpha = .3) +
            ggplot2::theme_bw() +
            ggplot2::theme(panel.background = ggplot2::element_blank(), 
                  panel.grid.major = ggplot2::element_blank(),
                  panel.grid.minor = ggplot2::element_blank(),
                  plot.background = ggplot2::element_blank(),
                  axis.text.y= ggplot2::element_blank(),
                  axis.ticks.y= ggplot2::element_blank(),
                  axis.text.x = ggplot2::element_text(angle=90, vjust = 0.5, hjust = 1.0))
        
    }
    
    plot
    
}

###############################################################################

###############################################################################
