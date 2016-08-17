#' Retrieves daily master index.
#'
#' \code{getDailyMaster} retrieves daily master index from US SEC site.
#'
#' getDailyMaster function takes date as an input parameter from user,  
#' download master index for that particular date from ftp://ftp.sec.gov/edgar/daily-index
#' site. It strips headers and convert this daily filing information in data frame format.
#' Function creates new directory 'Daily Index' into working directory 
#' to save these downloaded daily master index files.
#' 
#' @usage getDailyMaster(input.date)
#' 
#' @param input.date in character format 'mm/dd/YYYY'.
#' 
#' @return Function returns filing information for the input date.
#'   
#' @examples
#' \dontrun{
#' 
#' daily.filing.info <- getDailyMaster('08/09/2016')
#'} 


getDailyMaster <- function(input.date) {
  
  
  dir.create("Daily Index")
  
  input.date <- as.Date(input.date, "%m/%d/%Y")
  
  
  if (Sys.Date() < input.date) {
    msg <- "Select the appropriate date."
    cat(msg)
    return()
  }
  
  year <- format(input.date, "%Y")
  month <- format(input.date, "%m")
  day <- format(input.date, "%d")
  
  options(warn = -1)  # remove warnings
  
  # function for downloading daily Index
  GetDailyInfo <- function(day, month, year) {
    
    # function to download file and return FALSE if download
    # error
    DownloadFile <- function(link, dfile) {
      tryCatch({
        utils::download.file(link, dfile, quiet = TRUE)
        return(TRUE)
      }, error = function(e) {
        return(FALSE)
      })
    }
    
    date <- paste0(year, month, day)
    filename <- paste0("Daily Index/daily_idx_", date)
    link1 <- paste0("ftp://ftp.sec.gov/edgar/daily-index/master.", 
      date, ".idx")
    link2 <- paste0("ftp://ftp.sec.gov/edgar/daily-index/", 
      year, "/QTR", ceiling(as.integer(month)/3), "/master.", 
      date, ".idx.gz")
    link3 <- paste0("ftp://ftp.sec.gov/edgar/daily-index/", 
      year, "/QTR", ceiling(as.integer(month)/3), "/master.", 
      substr(as.character(year), 3, 4), month, day, ".idx")
    link4 <- paste0("ftp://ftp.sec.gov/edgar/daily-index/", 
      year, "/QTR", ceiling(as.integer(month)/3), "/master.", 
      date, ".idx")
    down.success = FALSE
    
    if (year < 1999) {
      fun.return3 <- DownloadFile(link3, filename)
      if (fun.return3 == 1) {
        down.success = TRUE
      }
    }
    
    if (year > 1998 && year < 2012) {
      fun.return4 <- DownloadFile(link4, filename)
      if (fun.return4 == 1) {
        down.success = TRUE
      }
    }
    
    if (year > 2011) {
      fun.return1 <- DownloadFile(link1, filename)
      if (fun.return1) {
        down.success = TRUE
      } else {
        fun.return2 <- DownloadFile(link2, filename)
        if (fun.return2) {
          down.success = TRUE
        }
      }
    }
    
    if (down.success) {
      
      # Removing ''' so that scan with '|' not fail due to
      # occurrence of ''' in company name
      temp.data <- gsub("'", "", readLines(filename))
      # writting back to storage
      writeLines(temp.data, filename)
      
      d <- scan(filename, what = list("", "", "", "", ""), 
        flush = F, skip = 7, sep = "|", quiet = T)
      data <- data.frame(CIK = d[[1]], COMPANY_NAME = d[[2]], 
        FORM_TYPE = d[[3]], DATE_FILED = d[[4]], EDGAR_LINK = d[[5]])
      data$DATE_FILED <- NULL
      return(data)
    }
  }
  
  ## Call above GetDailyInfo function
  return(GetDailyInfo(day, month, year))
}
