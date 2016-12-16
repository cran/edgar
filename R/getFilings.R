#' Retrieves EDGAR filings from SEC site.
#'
#' \code{getFilings} retrieves EDGAR filings for the specified CIK, form type,
#' and year mentioned in function parameters.
#'
#' getFilings function takes year, form type, and CIK as an input.
#' Working directory should contain 'Master Index' 
#' directory which contains master Rda files for specific years downloaded using 
#' \link[edgar]{getMasterIndex} function. Function creates new directory 'Edgar filings'  
#' to store all downloaded filings. Please note, for all other functions in this 
#' package needs to locate the same working directory.  
#' 
#' @usage getFilings(year, cik.no, form.type)
#'
#' @param year an integer specifies year for which EDGAR filings are to be downloaded.
#' @param cik.no an integer containing specific CIK number for which EDGAR filings are 
#' to be downloaded. cik.no = 'ALL' if required to download for all CIK's.
#' @param form.type character string containing specific Form type to be downloaded. 
#' form.type = 'ALL' if required to download all form-types. 
#' 
#' @return Function downloads EDGAR filings and returns download status data frame.
#'   
#' @examples
#' \dontrun{
#' 
#' report <- getFilings(1994, 100030, 'ALL') 
#' ## download all filings filed by the firm with CIK=100030 in the year 1994. 
#' ## Generates download report in data frame.
#' 
#' report <- getFilings(2006, 1000180, '10-K')
#' ## download '10-K' filings filed by the firm with CIK=1000180 in the year 2006. 
#' ## Generates download report in data frame.
#' }
#' 

getFilings <- function(year, cik.no, form.type) {
  
  # function to download file and return FALSE if download
  # error
  DownloadFile <- function(link, filename) {
    tryCatch({
      utils::download.file(link, filename, quiet = TRUE)
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  options(warn = -1)  # remove warnings
  
  yr.master <- paste0(year, "master.Rda")  # Create specific year .Rda filename.
  
  stat.filing2 <- data.frame()  # Create empty data frame to store download status result.
  
  if (file.exists(paste0("Master Index/", yr.master))) {
    load(paste0("Master Index/", yr.master))
    if (nrow(year.master) > 0) {
      # check if user want to download all cik or specific cik
      if (!cik.no == "ALL") {
        year.master <- year.master[year.master$CIK == cik.no, ]
      }
      
      # if cik.no not found in the master file then show msg and
      # exit
      if (nrow(year.master) == 0) {
        msg1 <- paste0("CIK No:", cik.no, " not found in file: ", 
          yr.master)
        cat(msg1)
        return()
      }
      
      # check if user want to download all the files or specific
      # type
      if (!form.type == "ALL") {
        year.master <- year.master[year.master$FORM_TYPE == form.type, ]
      }
      
      # if form.type not found in the master file then show msg and
      # exit
      if (nrow(year.master) == 0) {
        msg2 <- paste0("Form Type: ", form.type, " not Filed by ", cik.no)
        cat(msg2)
        return()
      }
      
      # downloading files
      total.files <- nrow(year.master)
      msg3 <- paste0("Total number of filings to be downloaded=", 
        total.files, ". Do you want to download (yes/no)? ")
      choice <- readline(prompt = msg3)
      
      if (as.character(choice) == "yes") {
        
        dir.create("Edgar filings")
        
        # Create progress bar object
        progress.bar <- txtProgressBar(min = 0, max = total.files, style = 3)
        
        f.type <- gsub("/", "", form.type)
        new.dir <- paste0("Edgar filings/", cik.no, "_", 
          f.type, "_", year)
        dir.create(new.dir)
        
        for (i in 1:total.files) {
          LINK <- paste0("https://www.sec.gov/Archives/", 
          year.master$EDGAR_LINK[i])
          f.type <- gsub("/", "", year.master$FORM_TYPE[i])
          dest.filename <- paste0(new.dir, "/", year.master$CIK[i], 
          "_", f.type, "_", year.master$DATE_FILED[i], 
          ".txt")
          
          res <- DownloadFile(LINK, dest.filename)
          if (res) {
          temp.status <- data.frame(Link = LINK, Status = "Download success")
          } else {
          temp.status <- data.frame(Link = LINK, Status = "Server Error")
          }
          
          stat.filing2 <- rbind(stat.filing2, temp.status)
          
          # Update progress bar
          setTxtProgressBar(progress.bar, i)
        }
        
        # Close progress bar
        close(progress.bar)
        
        return(stat.filing2)
      }
    } else {
      msg4 <- "Rda file is corrupted. Please re-download the master index file for the selected year using 'getMasterIndex' function."
      cat(msg4)
      return()
    }
  } else {
    msg5 <- paste0("Current directory does not contains ", 
      yr.master, " file in 'Master Index' directory. Please download master index using 'getMasterIndex' function.")
    cat(msg5)
    return()
  }
}
