#' Retrieves quarterly master index
#'
#' \code{getMasterIndex} retrieves the quarterly master indexes from the U.S. SEC EDGAR server.
#'
#' getMasterIndex function takes filing year as an input parameter from a user,  
#' downloads quarterly master indexes from the US SEC server.
#' \url{https://www.sec.gov/Archives/edgar/full-index/}. It then strips headers from the 
#' master index files, converts them into dataframe, and 
#' merges such quarterly dataframes into yearly dataframe, and stores them 
#' in Rda format. It has ability to download master indexes for multiple years 
#' based on the user input. This function creates a new directory 'edgar_MasterIndex' 
#' into current working directory to save these Rda Master Index. Please note, for 
#' all other functions in this package need to locate the same working 
#' directory to access these Rda master index files. 
#' User must follow the US SEC's fair access policy, i.e. download only what you 
#' need and limit your request rates, see \url{https://www.sec.gov/os/accessing-edgar-data}.
#'     
#' @usage getMasterIndex(filing.year)
#'
#' @param filing.year vector of integer containing filing years.
#'  
#' @return Function downloads quarterly master index files and stores them 
#' into the mentioned directory.
#'   
#' @examples
#' \dontrun{
#'  
#' getMasterIndex(2006) 
#' ## Downloads quarterly master index files for 2006 and 
#' stores into yearly 2006master.Rda file.
#' 
#' getMasterIndex(c(2006, 2008)) 
#' ## Downloads quarterly master index files for 2006 and 2008, and 
#' stores into 2006master.Rda and 2008master.Rda files.
#'}

getMasterIndex <- function(filing.year) {

    options(warn = -1)
    
    # Check year validity
    if (any(is.na(as.numeric(filing.year)))) {
        cat("Please provide valid year.")
        return()
    }
    
  # Check the download compatibility based on OS
  getdownCompat <- function() {
    
    if (nzchar(Sys.which("libcurl"))) {
      dmethod <- "libcurl"
    } else if (nzchar(Sys.which("wget"))) {
      dmethod <- "wget"
    } else if (nzchar(Sys.which("curl"))) {
      dmethod <- "curl"
    } else if (nzchar(Sys.which("lynx"))) {
      dmethod <- "lynx"
    } else if (nzchar(Sys.which("wininet"))) {
      dmethod <- "wininet"
    } else {
      dmethod <- "auto"
    }
    
    return(dmethod)
  }
    
  UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0"
  
  # function to download file and return FALSE if download error
  DownloadSECFile <- function(link, dfile, dmethod, UA) {
    
    tryCatch({
      ## method 1 
      # utils::download.file(link, dfile, method = dmethod, quiet = TRUE,
      #                      headers = c("User-Agent" = useragent,
      #                                  "Accept-Encoding"= "deflate, gzip",
      #                                  "Host"= "www.sec.gov"))
      
      ## method 2 
      
      r <- httr::GET(link, 
                httr::add_headers(`Connection` = "keep-alive", `User-Agent` = UA),
                httr::write_disk(dfile, overwrite=TRUE)
              )
      
      if(httr::status_code(r)==200){
        return(TRUE)
      }else{
        return(FALSE)
      }
     
    }, error = function(e) {
      return(FALSE)
    })
    
  }
  
    ## Check the download compatibility based on OS
    dmethod <- getdownCompat() 
    
    dir.create("edgar_MasterIndex")
    
    status.array <- data.frame()
    
    for (i in 1:length(filing.year)) {
        
        year <- filing.year[i]
        
        year.master <- data.frame()
        
        quarterloop <- 4
        
        # Find the number of quarters completed in input year
        if (year == format(Sys.Date(), "%Y")) {
            quarterloop <- ceiling(as.integer(format(Sys.Date(), "%m"))/3)
        }
        
        cat("Downloading Master Indexes from SEC server for",year,"...\n")

        for (quarter in 1:quarterloop) {
            
            # save downloaded file as specific name
            dfile <- paste0("edgar_MasterIndex/", year, "QTR", quarter, "master.gz")
            file <- paste0("edgar_MasterIndex/", year, "QTR", quarter, "master")
            
            # form a link to download master file
            link <- paste0("https://www.sec.gov/Archives/edgar/full-index/", year, "/QTR", quarter, "/master.gz")
            
            ### Go inside a loop to download
            i = 1
            
            while(TRUE){

              res <- DownloadSECFile(link, dfile, dmethod, UA)
            
              if (res){
                
                if (file.info(dfile)$size < 6000){
                  
                  aa <- readLines(dfile)
                  
                  if(any(grepl("For security purposes, and to ensure that the public service remains available to users, this government computer system", aa)) == FALSE){
                  
                      break
                  }
                  
                }else{
                  Sys.sleep(1)
                  break
                }
              }
              
              ### If waiting for more than 10*15 seconds, put as server error
              if(i == 16){
                
                status.array <- rbind(status.array, data.frame(Filename = paste0(year, ": quarter-", quarter),
                                                               status = "Server Error"))
                break
              }
              
              i = i + 1 
              Sys.sleep(3) ## Wait for multiple of 3 seconds to ease request load on SEC server. 
            }
            
            
            # Unzip gz file
            R.utils::gunzip(dfile, destname = file, temporary = FALSE, skip = FALSE, overwrite = TRUE, remove = FALSE)
            
            # Removing ''' so that scan with '|' not fail due to occurrence of ''' in company name
            raw.data <- readLines(file)
            raw.data <- iconv(raw.data, "latin1", "ASCII", sub = "")
            raw.data <- gsub("'", "", raw.data)
            
            # Find line number where header description ends
            header.end <- grep("--------------------------------------------------------", raw.data)
            
            # writing back to storage
            writeLines(raw.data, file)
            
            scraped.data <- scan(file, what = list("", "", "", "", ""), flush = F, skip = header.end, sep = "|",
                                 quiet = T)
            
            # Remove punctuation characters from company names
            company.name <- gsub("[[:punct:]]", " ", scraped.data[[2]], perl = T)
            
            final.data <- data.frame(cik = scraped.data[[1]], company.name = company.name, form.type = scraped.data[[3]],
                                     date.filed = scraped.data[[4]], edgar.link = scraped.data[[5]], quarter = quarter)
            
            year.master <- rbind(year.master, final.data)
            
            file.remove(file)
            
            status.array <- rbind(status.array, data.frame(Filename = paste0(year, ": quarter-", quarter),
                                                           status = "Download success"))
            
            cat("Master Index for quarter", quarter,"\n")
        }
        
  
        assign(paste0(year, "master"), year.master)
        
        save(year.master, file = paste0("edgar_MasterIndex/", year, "master.Rda"))
      
    }
    
}
