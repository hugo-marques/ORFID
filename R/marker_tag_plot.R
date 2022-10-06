#' @title Plot detection from marker tags used with Oregon RFID ORMR and ORSR antenna readers
#' @description Creates a time series plot containing only detection data from a user-specified marker tag (unique tag number).
#' @param x antenna data compiled using \code{\link{import_ORFID}} or \code{\link{join_multireader_data}}.
#' @param tag marker tag identification (character object).
#' @param gap minimum time gap in seconds between detections (optional). 
#' @details Creates a plot displaying marker tag detections. If a minimum time gap is specified, time gaps greater than the minimum specified are highlighted in red. This allows the user to identify periods when marker tags were not being detected as frequently as expected. 
#' @return Returns a plot object.
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
#' # Plot marker tag data and highlight gaps greater than 10 minutes.
#' marker_tag(PIT_data, "0000_000000004978", gap = 600)
#' }

marker_tag_plot <- function(x, tag, gap) {
    
    if (missing(gap)) {
        
        marker_tag <- x %>%
            dplyr::filter(TAG == tag) %>%
            dplyr::mutate(arr = ARR) %>%
            dplyr::mutate_at("arr", list(~. - lag(.))) %>%
            dplyr::mutate(GAP = round(arr, digits = 1)) %>%
            dplyr::select(TAG, ARR, DUR, GAP, NCD)
        
        plot <- ggplot2::ggplot() +
            ggplot2::geom_point(data = marker_tag, 
                                ggplot2::aes(x = ARR, y = TAG), 
                                size = .01) +
            ggplot2::xlab("Detection date and time (ARR)")+
            ggplot2::ylab(unique(marker_tag$TAG))+
            ggplot2::theme_bw()
        
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
            ggplot2::geom_point(data = marker_tag, 
                                ggplot2::aes(x = ARR, y = TAG), 
                                size = .01) +
            ggplot2::xlab("Detection date and time (ARR)")+
            ggplot2::ylab(unique(marker_tag$TAG)) +
            ggplot2::geom_linerange(data = marker_tag_gap, 
                                    ggplot2::aes(xmin = gap_start, 
                                                 xmax = gap_end, 
                                                 y = TAG), 
                                    colour = "red", 
                                    size = 500, 
                                    alpha = .3) +
            ggplot2::theme_bw()
        
    }
    
    custom_theme <- function() {
        ggplot2::theme(panel.background = ggplot2::element_blank(), 
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       plot.background = ggplot2::element_blank(),
                       axis.text.y= ggplot2::element_blank(),
                       axis.ticks.y= ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(angle = 90, 
                                                           vjust = 0.5, 
                                                           hjust = 1.0))
    }
    
    # Hourly axis breaks if scale is 2 days or less, otherwise daily breaks
    if (difftime(max(marker_tag$ARR), min(marker_tag$ARR), units = "hours") > 48) {
        
        plot_out <- plot +
            ggplot2::scale_x_datetime(date_breaks = "1 day", 
                                      date_labels = "%m/%d %H:%M") +
            custom_theme()
        
    } else {
        
        plot_out <- plot +
            ggplot2::scale_x_datetime(date_breaks = "1 hour", 
                                      date_labels = "%m/%d %H:%M") +
            custom_theme()
        
    }
    
    plot_out
    
}
