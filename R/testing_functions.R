#Testing_functions

setwd("C:/Users/hmarq/Desktop")
list.files()

#  Importing thw data:

data_USA <- import_ORFID("C:/Users/hmarq/Desktop/SCD_USA_t.txt", delim = "\t")

data_BRA <- import_ORFID("C:/Users/hmarq/Desktop/SCD_BRA_t.txt", delim = "\t")

# Combining the data:

readers <- list(data_USA, data_BRA)

bla<- join_multireader_data(readers)

readers2 <- list(bla, data_BRA)

blabla <- join_multireader_data(readers2)


TAG_info(bla)

TAG_info(blabla)

export_excel_ORFID(bla)

roxygen2::roxygenise()
