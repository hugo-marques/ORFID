#' @title Export the data frames created with the \code{\link{import_ORFID}} as a xlsx file
#' @description Function to export PIT_data data frames as a xlsx file to user working directory 
#' @param x PIT_data data frame to be exported.
#' @details The file will be saved in the user-specified working directory.
#' @return Returns an .xlsx file.
#' @author Hugo Marques <biohmarques@@gmail.com>
#' @seealso 
#' \code{\link{import_ORFID}} for importing data files from the new generation of Oregon RFID readers
#' \code{\link{join_multireader_data}} for Combining data from ORFID readers into an multi readers array
#' @examples
#' #  Exporting a PIT_data data frame to the working directory.
#' 
#' #  Import the ORFID reader data to the user working directory
#' \dontrun{} data_BRA <- import_ORFID("~/Desktop/SCD_BRA.txt")
#' 
#' #  Create the list containing the imported files:
#' \dontrun{} readers list(data_USA, data_BRA)
#' 
#' #  Combine the files:
#' \dontrun{} data <- join_multireader_data(readers)
#' 
#' #  Export the multi reader data as a .xlsx file:
#' \dontrun{} export_excel_ORFID(PIT_data)
#' 
#' @export

###############################################################################


export_ORFID <- function(x, name, extension){
    
    if (!(extension %in% c(".txt", ".csv", ".xlsx"))) {
        stop("The extension must be '.txt', '.csv' or '.xlsx'")
    }
    
    file <- paste0(name,extension)
    
    if(extension == ".txt") {
        
        write_delim(x, file)
    }
    
    if(extension == ".csv") {
        write_csv(x, file)
    }
    
    if(extension == ".xlsx") {
        write.xlsx(x, file)
    }
    
}

###############################################################################