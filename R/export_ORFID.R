#' @title Export the data frames created with the \code{\link{import_ORFID}} as a xlsx file
#' @description Function to export PIT_data data frames as a xlsx file to user working directory.
#' @param x PIT_data data frame to be exported.
#' @param name a name for the exported file. 
#' @param extension extension to determine the file type. Should be ".txt", ".csv" or ".xlsx".
#' @details The file will be saved in the user-specified working directory.
#' @return Returns a ".txt", ".csv" or ".xlsx" file depending on the user option.
#' @author Hugo Marques <biohmarques@@gmail.com>
#' @seealso 
#' \code{\link{import_ORFID}} for importing data files from the new generation of Oregon RFID readers
#' \code{\link{join_multireader_data}} for Combining data from ORFID readers into an multi readers array
#' @export
#' @examples
#' \dontrun{
#' 
#' ##  Exporting a PIT_data data frame to the working directory.
#' 
#' ##  Create the list containing the imported files:  
#' readers <- list(reader_1, reader_2)
#' 
#' ##  Combine the files:
#' array <- join_multireader_data(readers)
#' 
#' ##  Export the data a .xlsx file:
#' export_ORFID(x = array, name = "PIT_data", extension = ".xlsx")
#' 
#' ##  Export the data as a .txt file:
#' export_ORFID(x = array, name = "PIT_data", extension = ".txt")
#' 
#' ##  Summarizing tag info
#' tags <- tags_summary(array) 
#' 
#' ##  Export the tag info as a .xlsx file:
#' export_ORFID(x = tags, name = "tags_info", extension = ".xlsx")
#' }

###############################################################################


export_ORFID <- function(x, name, extension){
    
    if (!(extension %in% c(".csv", ".xlsx"))) {
        stop("The extension must be '.csv' or '.xlsx'")
    }
    
    file <- paste0(name,extension)
    

    if(extension == ".csv") {
        readr::write_csv(x, file)
    }
    
    if(extension == ".xlsx") {
        openxlsx::write.xlsx(x, file)
    }
    
}

###############################################################################