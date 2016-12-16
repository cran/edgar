#' Connect sentiment count of multiple cik's 10-K filing with Compustat data.
#'
#' \code{mergeCompustat} merge sentiment count of 10-K filing with Compustat data.
#'
#' mergeCompustat function takes cik number, filing year, sentiment dictionary, and  
#' Compustat data. It download and parse 10-K filing for desired cik and filing year  
#' present in input Compustat dataset. It count sentiment words present in defined 10-K 
#' and append to Compustat data frame. Working directory must contain 'Master Index' 
#' directory which contains master Rda files for specified filing year. This master index
#' can be downloaded using \link[edgar]{getMasterIndex} function. Please note that,
#' Compustat data must contains 'cik' column, and  'datadate' column in 'mm/dd/yyyy' format.
#'  
#' @usage mergeCompustat (cik.no, filing.yr, words.list, compustat.data)
#'
#' @param cik.no cik number.
#' @param filing.yr 10-K filing year.
#' @param words.list sentiment dictionary in list format.
#' @param compustat.data Compustat data frame.
#'
#' @return Compustat data with sentiment.count column for desired cik and filing year.
#'   
#' @examples
#' \dontrun{
#' 
#' ## User needs to input Compustat data in data frame format.
#' compustat.data <- read.csv('compustat_data.csv')
#' 
#' ## User can apply any desired user defined dictionary 
#' ## other than default dictionaries from this package.
#' words.list <- scan(system.file('data/negwords.txt', package = 'edgar') 
#'                    , what='character')
#' 
#' ## For single cik
#' res <- mergeCompustat( 2098, 2014, words.list, compustat.data)
#' 
#' ## User can provide list of different CIK's.
#' cik.no <- c(1750,6201,2098)
#' res <- mergeCompustat( cik.no, 2014, words.list, compustat.data)
#'  
#' }
#' 

mergeCompustat <- function(cik.no, filing.yr, words.list, compustat.data) {
  
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
  
  # Convert cik to number to have consistancy in data and
  # filter data frame.
  compustat.data$cik <- as.numeric(compustat.data$cik)
  compustat.data <- compustat.data[compustat.data$cik %in% 
    cik.no, ]
  compustat.data <- compustat.data[!is.na(compustat.data$cik), 
    ]
  
  if (nrow(compustat.data) == 0) {
    msg <- paste0("cik list not found in Compustat data.")
    cat(msg)
    return()
  }
  
  # filter data frame by filing year
  compustat.data$datadate <- as.character(compustat.data$datadate)
  compustat.data$filing.yr <- sub(".*(\\d{4}).*", "\\1", compustat.data$datadate)
  compustat.data <- compustat.data[compustat.data$filing.yr == 
    filing.yr, ]
  
  if (nrow(compustat.data) == 0) {
    msg <- paste0("cik list does not have filing entry for year ", 
      filing.yr, " in Compustat data.")
    cat(msg)
    return()
  }
  
  compustat.data$sentiment.count <- NULL
  
  yr.master <- paste0(filing.yr, "master.Rda")  # Create specific year .Rda filename.
  
  
  if (!file.exists(paste0("Master Index/", yr.master))) {
    msg <- paste0("Current directory does not contains ", 
      yr.master, " file in 'Master Index' directory. Please download master index using 'getMasterIndex' function.")
    cat(msg)
    return()
  }
  
  load(paste0("Master Index/", yr.master))
  
  # filter master index with cik
  year.master <- year.master[year.master$CIK %in% cik.no, ]
  
  if (nrow(year.master) == 0) {
    msg <- paste0("CIK No:", cik.no, " not found in file: ", 
      yr.master, ".\n")
    cat(msg)
    return()
  }
  
  form.type <- "10-K"
  
  # filter master index with '10-K'
  year.master <- year.master[year.master$FORM_TYPE == form.type, 
    ]
  
  if (nrow(year.master) == 0) {
    msg <- paste0("Form Type: ", form.type, " not Filed by CIK:", 
      cik.no, " in year ", filing.yr, ".\n")
    cat(msg)
    return()
  }
  
  f.type <- gsub("/", "", form.type)
  
  
  temp.data <- data.frame(cik = year.master$CIK, edgar.link = year.master$EDGAR_LINK)
  temp.data$sentiment.count <- NULL
  
  # Create new directory to store 10-K filing
  dir.create("Edgar Filings/Temp filings")
  dest.filename <- "Edgar Filings/Temp filings/temp.txt"
  
  for (i in 1:nrow(temp.data)) {
    # Create link to downlaod 10-K filing
    LINK <- paste0("https://www.sec.gov/Archives/", temp.data$edgar.link[i])
    res <- DownloadFile(LINK, dest.filename)
    
    if (res == FALSE) {
      temp.data$sentiment.count[i] <- NA
      next
    }
    
    word.frq <- getWordfrquency(dest.filename)  # Create word frequency data frame for 10-K filing
    
    senti.words <- getSentimentCount(word.frq, words.list)  #  get sentiment words count
    
    senti.count <- sum(senti.words$FREQUENCY)  # Count total number of sentiment words 
    
    temp.data$sentiment.count[i] <- senti.count
  }
  
  # Merge Compustat data and sentiment count based on cik.
  compustat.data = merge(compustat.data, temp.data, by = "cik", 
    all.x = T)
  
  rownames(compustat.data) <- NULL
  compustat.data$filing.yr <- NULL
  compustat.data$edgar.link <- NULL
  
  return(compustat.data)
}



