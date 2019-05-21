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

export_excel_ORFID <- function(x){
    
    #file <- paste0(getwd(),"/ORFID_data_", sample(1:100, 1, replace=FALSE), ".xlsx")
    
    file <- paste0(getwd(),"/ORFID_data_", str_replace(Sys.time(), pattern = " ", replacement = "_"), ".xlsx")
    
    openxlsx::write.xlsx(x, file = file)
    
}

###############################################################################