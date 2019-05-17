###############################################################################

# Creating a function to export the data frame to excel
# require(openxlsx)

#' @title Export the data frames created with the \code{\link{mport_ORFID}} as a xlsx file
#'
#' @description Function to export PIT_data data frames as a xlsx file to user working directory  
#' @param x PIT_data data frame to be exported.
#' @details File is saved in the user-specified working directory.
#' @examples
#' #export a PIT_data data frame to the working directory.
#' \dontrun{} export_excel_ORFID(PIT_data)
#' @export


export_excel_ORFID <- function(x){
    
    file <- paste0(getwd(),"PIT_data_", sample(1:100,1, replace=FALSE), ".xlsx")
    
    write.xlsx(x, file = file)
    
}


###############################################################################