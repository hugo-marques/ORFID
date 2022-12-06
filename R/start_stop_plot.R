#' @title Plot reader start and stop times from Oregon RFID antenna readers
#' @description Creates a time series plot containing start and stop times from Oregon RFID (radio-frequency identification) ORMR (Oregon RFID multi-reader) and ORSR (Oregon RFID single reader) antenna readers.
#' @param x event data compiled using \code{\link{import_ORFID_events}} 
#' @details Creates a plot displaying reader start and stop times. Note that start and stop times can be very close together and difficult to distinguish depending on the period plotted. Filter event data to improve resolution.
#' @return Returns a plot object.
#' @author Annika Putt <annika@@instream.net>
#' @seealso 
#' \code{\link{import_ORFID_events}} for importing event data from Oregon RFID ORMR and ORSR antenna readers.
#' 
#' \code{\link{join_multireader_data}} for combining data from Oregon RFID ORMR and ORSR antenna readers into a multi-reader array.
#' @importFrom magrittr %>%

start_stop_plot <- function(x) {
  
  starts <- x %>% 
    dplyr::filter(grepl("started", message)) %>% 
    dplyr::mutate(pwr = "Start")
  
  stops <- x %>% 
    dplyr::filter(grepl("stopped", message)) %>% 
    dplyr::mutate(pwr = "Stop")
  
  pwr_data <- dplyr::bind_rows(starts, stops) %>% 
    dplyr::arrange(ARR)

  plot <- ggplot2::ggplot() +
    ggplot2::geom_point(data = pwr_data,
                        ggplot2::aes(x = ARR, y = -1, color = pwr),
                        size = 3,
                        shape = 15) +
    ggplot2::geom_vline(xintercept = dplyr::filter(pwr_data, pwr == "Start")$ARR,
                        color = "green") +
    ggplot2::geom_vline(xintercept = dplyr::filter(pwr_data, pwr == "Stop")$ARR,
                        color = "red") +
    ggplot2::scale_color_manual(values = c("green", "red"),
                                name = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::xlab("Date and time (ARR)") +
    ggplot2::ylab("") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.background = ggplot2::element_blank(), 
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.background = ggplot2::element_blank(),
                   axis.text.y= ggplot2::element_blank(),
                   axis.ticks.y= ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 90, 
                                                       vjust = 0.5, 
                                                       hjust = 1.0))
  
  # Hourly axis breaks if scale is 2 days or less, otherwise daily breaks
  if (difftime(max(pwr_data$ARR), min(pwr_data$ARR), units = "hours") > 48) {
    
    plot_out <- plot +
      ggplot2::scale_x_datetime(date_breaks = "1 day", 
                                date_labels = "%m/%d %H:%M") 
    
  } else {
    
    plot_out <- plot +
      ggplot2::scale_x_datetime(date_breaks = "1 hour", 
                                date_labels = "%m/%d %H:%M") 
    
  }
    
  plot_out
  
}
  