#' Retrieve the quarterly Master Index files from SEC.org site.
#'
#' \code{DownloadMaster} retrieve the quarterly Master Index files.
#'
#' DownloadMaster function takes years as a parameter to download quarterly Master Index 
#' files and stores into Rda yearly format. It ask the user to locate the 
#' working directory. New directory 'Master Files' will be created and all the Rda 
#' master files get stored into it. Please note, for all other functions in this 
#' package needs to locate the same working directory to access these Rda index files.  
#'  
#' @param year.array year in integer or integer array containing years for which Master 
#' Index are to be downloaded.
#' 
#' @return Function retrieve the quarterly Master Index files 
#' from \url{ftp://ftp.sec.gov/edgar} site and returns download status dataframe.
#'   
#' @examples
#' \dontrun{
#' 
#' report <- DownloadMaster(1995) 
#' ## Download quarterly Master Index files for the year 1990 and stores into yearly  
#' ## 1995master.Rda file. It returns the download report in dataframe format.
#' 
#' report <- DownloadMaster(c(1994, 1995, 2006)) 
#' ## Download quarterly Master Index files for the years 1995, 1996, 1998 and stores into 
#' ## different {year}master.Rda files. It returns the download report in dataframe format.
#'}

DownloadMaster <- function(year.array) {
    if (!is.numeric(year.array)) {
        msg <- "Please enter valid year"
        err <- tcltk::tkmessageBox(message = msg, icon = "error")
        stop(msg)
    }
	
    # function to download file and return FALSE if download error
	DownloadFile <- function(link, dfile) {
		tryCatch({
			utils::download.file(link, dfile)
			return(TRUE)
			}, error = function(e) {
			return(FALSE)
		})
	}
	
    options(warn = -1)
    cat("Select working directory to store master files in pop up menu........")
    setwd(jchoose.dir(default = getwd(), caption = "Choose working directory"))
    dir.create("Master Files")
    
    status.array <- data.frame()
    for (i in 1:length(year.array)) {
        year <- year.array[i]
        year.master <- data.frame()
        quarterloop <- 4
        if (year == format(Sys.Date(), "%Y")) {
            quarterloop <- ceiling(as.integer(format(Sys.Date(), "%m"))/3)
        }
        
        for (quarter in 1:quarterloop) {
            # save downloaded file as specific name
            dfile <- paste0("Master Files/", year, "QTR", quarter, "master.gz")
            file <- paste0("Master Files/", year, "QTR", quarter, "master")
            
            # form a link to download master file
            link <- paste0("ftp://ftp.sec.gov/edgar/full-index/", year, "/QTR", quarter, "/master.gz")
            
            res <- DownloadFile(link, dfile)
            if (res) {
                # Unzip gz file
                R.utils::gunzip(dfile, destname = file, temporary = FALSE, 
								skip = FALSE, overwrite = TRUE, remove = TRUE)
                cat("Successfully downloaded Quarter Master File for the year:", year, "and quarter:", quarter, "...")
                d <- scan(file, what = list("", "", "", "", ""), flush = F, skip = 10, sep = "|")
                data <- data.frame(CIK = d[[1]], COMPANY_NAME = d[[2]], FORM_TYPE = d[[3]], 
								   DATE_FILED = d[[4]], EDGAR_LINK = d[[5]], QUARTER = quarter)
                year.master <- rbind(year.master, data)
                file.remove(file)
                status.array <- rbind(status.array, data.frame(Filename = paste0(year, ": quarter-", quarter), 
				                      status = "Download success"))
                
            } else {
                status.array <- rbind(status.array, data.frame(Filename = paste0(year, ": quarter-", quarter), 
				                      status = "Server Error"))
            }
        }
        assign(paste0(year, "master"), year.master)
        save(year.master, file = paste0("Master Files/", year, "master.Rda"))
    }
    return(status.array)
}

 
