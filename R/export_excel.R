###############################################################################

# Creating a function to export the data frame to excel
# require(openxlsx)

export_excel <- function(x){
    
    file <- paste0(getwd(),"PIT_data_", format(Sys.time(), "%F_%H-%M-%S"), ".xlsx")
    
    write.xlsx(x, file = file)
    
}


###############################################################################