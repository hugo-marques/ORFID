#' @title Export data compiled from Oregon RFID ORMR and ORSR antenna readers 
#' @description Data compiled using \code{\link{import_ORFID}} or \code{\link{join_multireader_data}} are exported to the working directory as either a .csv or .xlsx file.
#' @param x data frame to be exported.
#' @param name file name. 
#' @param extension file extension (".csv" or ".xlsx").
#' @details Any data frame created using functions in the ORFID package can be exported using this function.
#' @return A file is saved in the working directory. 
#' @author Hugo Marques <biohmarques@@gmail.com>
#' @seealso 
#' \code{\link{import_ORFID}} for importing data files from Oregon RFID ORMR and ORSR antenna readers.
#' 
#' \code{\link{join_multireader_data}} for combining data from Oregon RFID ORMR and ORSR antenna readers into a multi-reader array.
#' @export
#' @examples
#' \dontrun{
#' 
#' # Import a single data file from an ORFID reader
#' reader_1 <- import_ORFID("~/reader_1.txt", delim = "\t")
#' 
#' # Export compiled data as a .xlsx file:
#' export_ORFID(x = reader_1, name = "reader_1_compiled", extension = ".xlsx")
#' 
#' }

export_ORFID <- function(x, name, extension) {
    
    if (!(extension %in% c(".csv", ".xlsx"))) {
        stop("The extension must be '.csv' or '.xlsx'")
    }
    
    file <- paste0(name, extension)
    
    
    if (extension == ".csv") {
        readr::write_csv(x, file)
    }
    
    if (extension == ".xlsx") {
        openxlsx::write.xlsx(x, file)
    }
    
}