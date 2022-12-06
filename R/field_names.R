#' @title View field names from Oregon RFID (radio-frequency identification) antenna readers
#'
#' @description Function to return field names and descriptions from Oregon RFID single readers (ORSR) and multi-readers (ORMR).
#' @details Returns the field/column names and its details from ORSR and ORMR data, which can be used to determine which field names should be included in data downloads.
#' @return A data frame with two variables: name and details
#' @author Hugo Marques <biohmarques@@gmail.com>
#' @export
#' @examples
#' 
#' field_names()
#' 

field_names <- function() {
    
    fn <- data.frame(Name = c("DTY", "TCH", "TTY", "TAG", "ANT", "ARR", "TRF", 
                              "DEP", "SSN", "ESN", "NCD", "EMP", "LAT", "LON", 
                              "ALT", "SIV", "HDP", "TSS", "CPA", "LSA", "EFA", 
                              "CPT", "LST", "ANV", "ANA", "NOI", "DUR", "CLS", 
                              "SPC", "SCD"),
                     Details = c("Detection type, S = summary, I = individual, E = event", 
                                 "Tag technology HDX, FDX, HF", 
                                 "Tag type A = Animal, R = Read only, W = Writeable, P = Phantom",
                                 "Tag ID number", 
                                 "Antenna number", 
                                 "Arrival date and time", 
                                 "Time reference G = GNSS, N = network, U = unreferenced", 
                                 "Departure date and time", 
                                 "Starting scan number (since midnight)", 
                                 "Ending scan number", 
                                 "Number of consecutive detections", 
                                 "Number of empty scans preceding detection", 
                                 "Latitude", 
                                 "Longitude", 
                                 "Altitude meters", 
                                 "Satellites in view", 
                                 "Location horizontal accuracy (m)", 
                                 "Tag signal strength", 
                                 "Charge pulse amps", 
                                 "Listen amps", 
                                 "Effective amps", 
                                 "Charge pulse time", 
                                 "Listen time", 
                                 "Antenna voltage", 
                                 "Antenna amperage", 
                                 "Noise", "Duration", 
                                 "Tag class", 
                                 "Output one space character", 
                                 "Site code"))
    
    return(format(fn, justify = "left"))
}