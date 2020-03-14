bla <- function(x, tag){
    
    if(missing(tag)){
        result <- x %>%
            group_by(TAG) %>%
            summarise(mean_DUR = round(mean(DUR), digits = 2), 
                      mean_NCD = mean(NCD), 
                      max_GAP = max(GAP, na.rm = T), 
                      median_gap = median(GAP, na.rm = T), 
                      mean_gap = mean(GAP, na.rm = T))
        
    } else {
        
        result <- x %>%
            filter(TAG == tag) %>%
            summarise(mean_DUR = round(mean(DUR), digits = 2), 
                      mean_NCD = mean(NCD), 
                      max_GAP = max(GAP, na.rm = T), 
                      median_gap = median(GAP, na.rm = T), 
                      mean_gap = mean(GAP, na.rm = T))
        
    }
    
    result
    
}

bla(teste, "0000_000000004978")

bla(teste)

###############################################################################

###############################################################################
