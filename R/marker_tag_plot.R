

###############################################################################

###############################################################################

marker_tag_plot <- function(x, tag, gap){
    
    if(missing(gap)){
        marker_tag <- x %>%
            dplyr::filter(TAG == tag) %>%
            dplyr::mutate(arr = ARR) %>%
            dplyr::mutate_at("arr", list(~. - lag(.))) %>%
            dplyr::mutate(GAP = round(arr, digits = 1)) %>%
            dplyr::select(TAG, ARR, DUR, GAP, NCD)
        
        plot <- ggplot() +
            geom_point(data = marker_tag, aes(x = ARR, y = TAG), size = .01) +
            xlab("Detection time")+
            ylab(unique(marker_tag$TAG))+
            scale_x_datetime(date_breaks = "1 hour", date_labels = "%m/%d %H:%M") +
            theme_bw() +
            theme(panel.background = element_blank(), 
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 1.0))
        
    } else {
        
        marker_tag <- x %>%
            filter(TAG == tag) %>%
            mutate(arr = ARR) %>%
            mutate_at("arr", list(~. - lag(.))) %>%
            mutate(GAP = round(arr, digits = 1)) %>%
            select(TAG, ARR, DUR, GAP, NCD)
        
        marker_tag_gap <- x %>%
            dplyr::filter(TAG == tag) %>%
            dplyr::mutate(arr = ARR) %>%
            dplyr::mutate_at("arr", list(~. - lag(.))) %>%
            dplyr::mutate(GAP = round(arr, digits = 1)) %>%
            dplyr::mutate(gap_start = ARR - GAP) %>%
            dplyr::mutate(gap_end = ARR) %>%
            dplyr::select(TAG, gap_start, gap_end, GAP) %>%
            dplyr::filter(GAP >= gap)
        
        plot <- ggplot() +
            geom_point(data = marker_tag, aes(x = ARR, y = TAG), size = .01) +
            xlab("Detection time")+
            ylab(unique(marker_tag$TAG))+
            scale_x_datetime(date_breaks = "1 hour", date_labels = "%m/%d %H:%M") +
            geom_linerange(data = marker_tag_gap, aes(xmin = gap_start, xmax = gap_end, y = TAG), colour = "red", size = 500, alpha = .3) +
            theme_bw() +
            theme(panel.background = element_blank(), 
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 1.0))
        
    }
    
    plot
    
}

###############################################################################

###############################################################################
