#' @title Combine data from ORFID readers into an multi readers array
#' @description Function allows users to combine unique readers into an array, using the data obtained from \code{\link{import_ORFID}} function. It is necessary to create a list with all data frames to be joined. If the data frames have diferent variables,
#' @param x list: A list containing data frames created with the \code{\link{import_ORFID}} function to be combined.
#' @details Data frame is created in the user environment. The output of \code{\link{join_multireader_data}} will contain a column if that column appears in any of data frames combined. A factor class variable called LOC (from locus) is the combination of SCD (Site Code) and ANT (Antenna) variables. It should be used as location in further statistical analysis once it is the individual detection spot.
#' @return Returns a tibble object.
#' @author Hugo Marques <biohmarques@@gmail.com>
#' @seealso \code{\link{import_ORFID}} for importing data files from the new generation of Oregon RFID readers
#' @importFrom magrittr %>%
#' @export
#' @examples
#' 
#' ##  Create the list containing the imported files:
#' readers <- list(reader_1, reader_2)
#' 
#' ##  Combine the files:
#' array <- join_multireader_data(readers)


###############################################################################

join_multireader_data <- function(x){
    
    if (class(x) != "list") {
        stop("The data frames should be in a list")
    } 
        
    y <- x %>%
        dplyr::bind_rows()

    if(!("SCD" %in% names(y))){
        stop("Missing site code (SCD). Combining data from multi readers requires a SCD variables in all reader's data")
        }
    
    if(anyNA(y$SCD) == T) {
        stop("Missing the site code (SCD) in at least one reader data. Combining data from multi readers requires a SCD variables in all reader's data")
    }
    
    if(!("ANT" %in% names(y))){
        y <- y %>%
            dplyr::mutate(ANT = 1)
    }
    
    if(anyNA(y$ANT) == T) {
        warning("NA values in ANT were replaced by 1")
    }
    
    message("A factor class variable called LOC (from locus) is the combination of SCD (Site Code) and ANT (Antenna) variables. It should be used as location in further graphical statistical analysis once it is the individual detection spot.")
    
    PIT_data_array <- y %>%
        dplyr::mutate(ANT = tidyr::replace_na(ANT, 1)) %>%
        dplyr::mutate(LOC = paste0(SCD,ANT)) %>%
        dplyr::mutate(SCD = as.factor(SCD)) %>%
        dplyr::mutate(ANT = as.factor(ANT)) %>%
        dplyr::mutate(LOC = as.factor(LOC))
    
    Sys.sleep(2)
    
    return(dplyr::glimpse(PIT_data_array))
        
}

###############################################################################