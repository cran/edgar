#' Retrieve the EDGAR fillings from SEC.org site.
#'
#' \code{DownloadFillings} retrieve the EDGAR fillings for the specified CIK, form-type,
#' and year mentioned in the function parameters.
#'
#' DownloadFillings function takes year, form_type, and CIK as an input. It ask the
#' user to locate the working directory. Working directory should contain 'Master Files' 
#' directory containing master Rda files for specific years downloaded using 
#' \link[edgar]{DownloadMaster} function. New directory 'EDGAR Fillings' will be created   
#' which stores all downloaded fillings. Please note, for all other functions in this 
#' package needs to locate the same working directory.  
#' 
#' @param year An integer specifies year for which EDGAR filings are to be downloaded.
#' @param cik.no An integer containing specific CIK number for which EDGAR filings are 
#' to be downloaded. cik.no = 'ALL' if required to download for all CIK's.
#' @param form.type character string containing specific Form-type to be downloaded. 
#' form.type = 'ALL' if required to download all form-types. 
#' 
#' @return Function download the EDGAR filings from \url{ftp://ftp.sec.gov/edgar} site 
#' and return download status dataframe.
#'   
#' @examples
#' \dontrun{
#' 
#' rep <- DownloadFillings(1994, 100030, 'ALL') 
#' ## download all fillings filed by the firm with CIK=100030 in the year 1994. Generate the report 
#' ## in dataframe.
#' 
#' rep <- DownloadFillings(2006, 1000180, '10-K')
#' ## download '10-K' fillings filed by the firm with CIK=1000180 in the year 2006. 
#' ## Generate the report in dataframe.
#' }
#' 

DownloadFillings <- function(year, cik.no, form.type) {

    # function to download file and return FALSE if download error
	DownloadFile <- function(link, filename) {
		tryCatch({
		  utils::download.file(link, filename)
			return(TRUE)
		}, error = function(e) {
			return(FALSE)
		})
	} 
    options(warn = -1)  # remove warnings
    yr.master <- paste0(year, "master.Rda")
    cat("Locate the current directory in the pop-up console........\n")
    wd <- jchoose.dir(default = getwd(), caption = "Choose working directory")
    setwd(wd)
    stat.filling2 <- data.frame()
    
    if (file.exists(paste0("Master Files/", yr.master))) {
        load(paste0("Master Files/", yr.master))
        if(nrow(year.master)>0){
			# check if user want to download all cik or specific cik
			if (!cik.no == "ALL") {
			  year.master <- year.master[year.master$CIK == cik.no, ]
			}
			
			# if cik.no not found in the master file then show msg and exit
			if (nrow(year.master) == 0) {
			  msg3 <- paste0("CIK No:", cik.no, " not found in file: ", yr.master)
			  err <- tcltk::tkmessageBox(message = msg3, icon = "error")
			  stop(msg3)
			}
			# check if user want to download all the files or specific type
			if (!form.type == "ALL") {
				year.master <- year.master[year.master$FORM_TYPE == form.type, ]
			}
			
			# if form.type not found in the master file then show msg and exit
			if (nrow(year.master) == 0) {
				msg2 <- paste0("Form Type: ", form.type, " not filled by ", cik.no)
				err <- tcltk::tkmessageBox(message = msg2, icon = "error")
				stop(msg2)
			}
			
			# downloading files
			total.files <- nrow(year.master)
			msg3 <- paste0("Total EDGARs to be downloaded:", total.files, "\nDo you want to download the EDGAR files")
			choice <- tcltk::tkmessageBox(message = msg3, type = "yesno", default = "no")
			
			if (as.character(choice) == "yes") {
				# progress bar
				dir.create("EDGAR Fillings")
				progress.bar <- tcltk::tkProgressBar(title = "Progress Bar", min = 0, max = total.files, width = 400)
				f.type<-gsub("/", "", form.type)
				new.dir <- paste0("EDGAR Fillings/", cik.no, "_", f.type, "_", year)
				dir.create(new.dir)
				
				for (i in 1:total.files) {
					LINK <- paste0("http://edgar.sec.gov/Archives/", year.master$EDGAR_LINK[i])
					f.type<-gsub("/", "", year.master$FORM_TYPE[i])
					dest.filename <- paste0(new.dir, "/", year.master$CIK[i], "_", f.type, "_", year.master$DATE_FILED[i], ".txt")
					
					cat("Downloading file:", LINK, "\n")
					res <- DownloadFile(LINK, dest.filename)
					
					if (res) {
					  temp.status <- data.frame(Link = LINK, Status = "Download success")
					} else {
					  temp.status <- data.frame(Link = LINK, Status = "Server Error")
					}
					
					stat.filling2 <- rbind(stat.filling2, temp.status)
					prg <- paste0(ceiling(i/total.files * 100), "% done")
					tcltk::setTkProgressBar(progress.bar, i, label = prg)
				}
				close(progress.bar)
				return(stat.filling2)
			}
    } 	else {
			msg3 <- "Rda file is corrupted. Please re-download the master Index file for the selected year using 'Get Master Index' tab."
			err <- tcltk::tkmessageBox(message = msg3, icon = "error") 
			return(stat.filling2)
		}
    } else {
        errmsg <- paste0("Current directory does not contains ", yr.master, " file in 'Master Files' directory. Please download Master files using DownloadMaster function.")
        err <- tcltk::tkmessageBox(message = errmsg, icon = "error")
        return(stat.filling2)
    }
}


