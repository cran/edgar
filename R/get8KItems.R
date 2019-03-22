#' Retrieves Form 8-K event information
#'
#' \code{get8KItems} retrieves Form 8-K event information of firms based on CIK numbers
#' and filing year.
#'
#' get8KItems function takes firm CIK number(s) and filing year(s) as input parameters from 
#' a user and provides information on the Form 8-K triggering events along with the firm 
#' filing information. The function searches and imports existing downloaded 
#' 8-K filings in the current directory; otherwise it downloads them using 
#' \link[edgar]{getFilings} function. It then reads the 8-K filings and parse the 
#' contents to get events information.
#' 
#' @usage get8KItems(cik.no, filing.year)
#' 
#' @param cik.no vector of CIK numbers in integer format. Suppress leading 
#' zeroes from CIKs.
#' 
#' @param filing.year vector of four digit numeric year
#' 
#' @return Function returns dataframe with Form 8-K events information along with CIK
#'  number, company name, date of filing, and accession number
#'   
#' @examples
#' \dontrun{
#' 
#' output <- get8KItems(cik.no = 38079, filing.year = 2005)
#' ## Returns 8-K events information for CIK '38079' filed in year 2005.
#' 
#' output <- get8KItems(cik.no = c(1000180,38079), 
#'                      filing.year = c(2005, 2006)) 
#'}

get8KItems <- function(cik.no, filing.year) {
    
    f.type <- "8-K"
    
    output <- getFilings(cik.no = cik.no, form.type = "8-K", filing.year, quarter = c(1, 2, 3, 4), 
	                     downl.permit = "y")
    
    if(is.null(output)){
      return()
    }
    
    cat("Scraping 8-K filings...\n")
    
    progress.bar <- txtProgressBar(min = 0, max = nrow(output), style = 3)
    
    ## Make new dataframe to store events info
    output.8K.df <- data.frame()
    
    for (i in 1:nrow(output)) {
        
        dest.filename <- paste0("Edgar filings_full text/Form ", f.type, 
                              "/", output$cik[i], "/", output$cik[i], "_", f.type, "_", 
                              output$date.filed[i], "_", output$accession.number[i], ".txt")
        
        filing.text <- readLines(dest.filename)

        # Capture ITEM INFORMATION
        item.info <- filing.text[grep("^ITEM INFORMATION:", filing.text, ignore.case = TRUE)]
        
        if (length(item.info) > 0) {
            item.info <- gsub("ITEM INFORMATION|:|\t", "", item.info)
        } else {
            item.info = ""
        }
        item.info <- gsub(",", " ", item.info)
        
        
        temp.df <- data.frame(cik = output$cik[i], company.name = output$company.name[i], 
                              form.type= output$form.type[i], date.filed = output$date.filed[i],
                              item.info = item.info)

        output.8K.df <- rbind(output.8K.df, temp.df)
        # update progress bar
        setTxtProgressBar(progress.bar, i)
    }
    
    # # # convert dates into R dates
    output.8K.df$date.filed <- as.Date(as.character(output.8K.df$date.filed), "%Y-%m-%d")
    
    return(output.8K.df)
}
