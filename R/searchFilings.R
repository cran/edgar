#' Search EDGAR filings for specific words 
#'
#' \code{searchFilings} Search EDGAR filings for specific words 
#'
#' searchFilings function takes search word vector, CIK(s), form type(s), and 
#' year(s) as input parameters. The function first imports available 
#' downloaded filings in local woking directory 
#' 'Edgar filings' created by \link[edgar]{getFilings} function; otherwise, 
#' it downloads the filings which is not already been downloaded.
#' It then reads the filings, cleans the filings, and 
#' search for the input words. The function returns a dataframe
#' with filing information, size of the filing, and the number of word hits.
#' 
#' @usage searchFilings(cik.no, form.type, filing.year, word.list)
#' 
#' @param cik.no vector of CIK number of firms in integer format. Suppress leading 
#' zeroes from CIKs. Keep cik.no = 'ALL' if needs to download for all CIK's.
#'  
#' @param form.type character vector containing form type to be downloaded. 
#' form.type = 'ALL' if need to download all forms.  
#'
#' @param filing.year vector of four digit numeric year
#' 
#' @param word.list vector of words to search in the filing
#' 
#' @return Function returns dataframe containing filing information, size of the 
#' filing, and the number of word hits based on the input phrases. Following are 
#' the definitions of some important variables:  
#'  
#' file.size = Total number of words in the filing. It does not consider stop words.
#' nword.hits = Number of total hits found in the filing based on the search word list.
#' 
#' @examples
#' \dontrun{
#'
#' word.list = c('derivative','hedging','currency forwards','currency futures')
#' output <- searchFilings(cik.no = c('1000180', '38079'), 
#'                      form.type = c("10-K", "10-K405","10KSB", "10KSB40"), 
#'                      filing.year = c(2005, 2006), word.list) 
#'}

searchFilings <- function(cik.no, form.type, filing.year, word.list) {
  
    output <- getFilings(cik.no, form.type, filing.year, quarter = c(1, 2, 3, 4), 
						 downl.permit = "y")
    
    if (is.null(output)){
      # cat("Please check the CIK number.")
      return()
    }
    
    cat("Searching filing(s) for the input word list...\n")

    progress.bar <- txtProgressBar(min = 0, max = nrow(output), style = 3)
    
    output$file.size <- NA
    output$nword.hits <- NA
    
    for (i in 1:nrow(output)) {
        
        f.type <- gsub("/", "", output$form.type[i])
        
        dest.filename <- paste0("Edgar filings_full text/Form ", f.type, 
                                "/", output$cik[i], "/", output$cik[i], "_", f.type, "_", 
                                output$date.filed[i], "_", output$accession.number[i], ".txt")
        
        # Read filing
        filing.text <- readLines(dest.filename)
        
        # Extract data from first <DOCUMENT> to </DOCUMENT>
        filing.text <- filing.text[(grep("<DOCUMENT>", filing.text, ignore.case = TRUE)[1]):(grep("</DOCUMENT>", 
            filing.text, ignore.case = TRUE)[1])]
        
        # See if 10-K is in XLBR or old text format
        if (any(grepl(pattern = "<xml>|<type>xml|<html>|10k.htm", filing.text, ignore.case = T))) {
            
            doc <- XML::htmlParse(filing.text, asText = TRUE)
            f.text <- XML::xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", 
                XML::xmlValue)
            f.text <- iconv(f.text, "latin1", "ASCII", sub = " ")
            
        } else {
            f.text <- filing.text
        }
        
        # Preprocessing the filing text
        f.text <- gsub("\\n|\\t|,", " ", f.text)
        f.text <- paste(f.text, collapse=" ")
        f.text <- gsub("'s ", "", f.text)
        f.text <- gsub("[[:punct:]]", "", f.text, perl=T)
        # f.text <- gsub("[[:digit:]]", "", f.text, perl=T)
        f.text <- iconv(f.text, from = 'UTF-8', to = 'ASCII//TRANSLIT')
        f.text <- tolower(f.text)
        f.text <- gsub("\\s{2,}", " ", f.text)
        
        ### Clean text and find number of total words
        text_words <- unlist(strsplit(f.text, " "))
        text_df <- data.frame(word = unlist(text_words), nchar = nchar(text_words))
        text_df <- text_df[text_df$nchar >=3, ]
        text_df <- text_df[!(text_df$word %in% tm::stopwords("en")), ]
        file.size <- nrow(text_df)   # Total Word count
        
        ### Search for word list 
        count_func <- function(word,text){
         stringr::str_count(text, word)
        }
        
        # Count words as well as general derivatives words
        nword.hits <- sum(sapply(word.list, count_func, text = f.text))
        
        # Assign all the varibles
        output$file.size[i] <- file.size
        output$nword.hits[i] <- nword.hits
        
        # update progress bar
        setTxtProgressBar(progress.bar, i)
        
    }
    

    # Close progress bar
    close(progress.bar)

    output$status <- NULL
    output$quarter <- NULL
    output$filing.year <- NULL
    output$accession.number <- NULL
    
    ## convert dates into R dates
    output$date.filed <- as.Date(as.character(output$date.filed), "%Y-%m-%d")

    return(output)
}
