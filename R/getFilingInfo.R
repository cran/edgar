#' Retrieves filing information of company.
#'
#' \code{getFilingInfo} retrieves filing information of company based on company name.
#'
#' getFilingInfo function takes firm name and filing year as input parameters from user,
#' and gives filing information for that firm. Master index file must be present 
#' for given year before applying this function. It can be downloaded using 
#' \link[edgar]{getMasterIndex} function.  
#' 
#' @usage getFilingInfo(firm.name, filing.year)
#' 
#' @param firm.name Desired full name or partial name of company.
#' @param filing.year Filing year in integer.
#' 
#' @return Function returns data frame with filing information.
#'   
#' @examples
#' \dontrun{
#' 
#' info <- getFilingInfo('United Technologies', 1994) 
#'}

getFilingInfo <- function(firm.name, filing.year) {
  
  filepath <- paste0("Master Index/", filing.year, "master.Rda")
  
  if (file.exists(filepath)) {
    
    load(filepath)
    
    firm.name <- toupper(firm.name)
    
    # Extract specific company information
    year.master <- year.master[grep(firm.name, year.master$COMPANY_NAME), ]
    
    year.master$EDGAR_LINK <- NULL
    rownames(year.master) <- 1:nrow(year.master)
    
    if (nrow(year.master) == 0) {
      cat("No information found for given company name. Please correct the company name.")
      return()
    }
    
  } else {
    msg1 <- paste0("Current directory does not contains master index file for ", 
      filing.year, " in 'Master Index' directory. Please download master index using 'getMasterIndex' function.")
    cat(msg1)
    return()
  }
  return(year.master)
}
