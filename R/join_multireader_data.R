#' @title Combine data from Oregon RFID ORMR and ORSR antenna readers
#' @description Function to combine data from unique readers into an array, using reader data compiled using \code{\link{import_ORFID}}. 
#' @param x A list object containing data frames compiled using \code{\link{import_ORFID}}.
#' @param verbose If TRUE, a description of the compiled data is printed to the console.
#' @details As in \code{\link{bind_rows}}, the output of \code{\link{join_multireader_data}} will contain a column if that column appears in any of the data inputs. The function creates a unique factor variable, LOC (location), which is a combination of SCD (site code) and ANT (antenna). 
#' @return Returns a tibble object of distinct detections from multiple antenna readers.
#' @author Hugo Marques <biohmarques@@gmail.com>
#' @seealso 
#' \code{\link{import_ORFID}} for importing data files from Oregon RFID ORMR and ORSR antenna readers.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' 
#' # Create a list containing compiled reader data:
#' readers <- list(reader_1, reader_2)
#' 
#' # Join data into a multi-reader array:
#' PIT_data <- join_multireader_data(readers)
#' 
#' }

join_multireader_data <- function(x, verbose = TRUE) {
    
    if (class(x) != "list") {
        stop("Input must be in list format")
    } 
    
    y <- x %>%
        dplyr::bind_rows()
    
    if (!("SCD" %in% names(y))) {
        stop("Missing site code (SCD). All input data must include SCD.")
    }
    
    if (anyNA(y$SCD) == T) {
        stop("Missing site code (SCD) in at least one input data frame. All input data must include SCD.")
    }
    
    if (!("ANT" %in% names(y))) {
        y <- y %>%
            dplyr::mutate(ANT = "A1")
    }
    
    if (anyNA(y$ANT) == T) {
        warning("NA values in ANT were replaced by A1")
    }
    
    PIT_data_array <- y %>%
        dplyr::mutate(ANT = tidyr::replace_na(ANT, "A1")) %>%
        dplyr::mutate(LOC = paste(SCD, ANT, sep = "_")) %>%
        dplyr::mutate(SCD = as.factor(SCD)) %>%
        dplyr::mutate(ANT = as.factor(ANT)) %>%
        dplyr::mutate(LOC = as.factor(LOC)) %>%
        dplyr::arrange(ARR) %>% 
        dplyr::distinct()
    
    
    message("A unique variable, LOC (location), was created by combining SCD (site code) and ANT (antenna).")
    
    if (verbose == TRUE) {
        return(dplyr::glimpse(PIT_data_array))
    } else {
        return(PIT_data_array)
    }
}
