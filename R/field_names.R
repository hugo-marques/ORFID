#' @title Returns the field names from ORSR and ORMR readers
#'
#' @description  Function to return the field names from ORSR and ORMR readers
#' @details Returns the field names and its details from ORSR and ORMR readers data
#' @return A data frame with two variables is returned (Name and Details)
#' @author Hugo Marques <biohmarques@@gmail.com>
#' @examples
#' 
#' ## Creating a data frame containing the field names from ORSR and ORMR readers.
#' 
#' (FM <- field_names())
#'  
#' @export

###############################################################################

field_names <- function(){
    
    fn <- data.frame(Name = c("DTY", "TCH", "TTY", "TAG", "ANT", "ARR", "TRF", "DEP", "SSN", "ESN", "NCD", "EMP", "LAT", "LON", "ALT", "SIV", "HDP", "TSS", "CPA", "LSA", "EFA", "CPT", "LST", "ANV", "ANA", "NOI", "DUR", "CLS", "SPC", "SCD"),
                     Details = c("Detection type, S = summary, I = individual, E = event", "Tag technology HDX, FDX, HF", "Tag type A = Animal, R = Read only, W = Writeable, P = Phantom", "Tag ID number", "Antenna number", "Arrival date and time", "Time reference G=GNSS, N=network, U=unreferenced", "Departure date and time", "Starting scan number (since midnight)", "Ending scan number", "Number of consecutive detections", "Number of empty scans preceding detection", "Latitude", "Longitude", "Altitude meters", "Satellites in view", "Location horizontal accuracy (m)", "Tag signal strength", "Charge pulse amps", "Listen amps", "Effective amps", "Charge pulse time", "Listen time", "Antenna voltage", "Antenna amperage", "Noise", "Duration", "Tag class", "Output one space character", "Site code"))
    
    #print(fn, justify = "r")
    return(format(fn, justify = "left"))
}

###############################################################################
