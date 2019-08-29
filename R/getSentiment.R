#' Provides sentiment measures of EDGAR filings
#'
#' \code{getSentiment} computes sentiment measures of EDGAR filings
#'
#' getSentiment function takes CIK(s), form type(s), and year(s) as input parameters.  
#' The function first imports available downloaded filings in local woking directory 
#' 'Edgar filings' created by \link[edgar]{getFilings} function; otherwise, 
#' it downloads the filings which is not already been downloaded.
#' It then reads the filings, cleans the filings, and 
#' computes the sentiment measures. The function returns a dataframe
#' with filing information, and sentiment measures.
#' 
#' @usage getSentiment(cik.no, form.type, filing.year)
#' 
#' @param cik.no vector of CIK number of firms in integer format. Suppress leading 
#' zeroes from CIKs. Keep cik.no = 'ALL' if needs to download for all CIKs.
#'  
#' @param form.type character vector containing form type to be downloaded. 
#' form.type = 'ALL' if need to download all forms.  
#'
#' @param filing.year vector of four digit numeric year
#' 
#' @return Function returns dataframe containing CIK number, company name, 
#' date of filing, accession number, and various sentiment measures. 
#' This function takes the help of Loughran-McDonald (L&M) sentiment 
#' dictionaries (https://sraf.nd.edu/textual-analysis/resources/) to 
#' compute sentiment measures of a EDGAR filing. Following are the 
#' definitions of the text characteristics and the sentiment measures:
#' 
#' file.size = Total number of words in the filing. It does not consider stop words.
#' 
#' char.count = Total number of characters in the filing. It does not consider stop words.
#' 
#' complex.word.count = Total number of complex words in filing. A word is count 
#' as a complex word if contains vowels(a, e, i, o, u) more than three times. 
#' It does not consider stop words. 
#' 
#' lm.dictionary.count = The number of words that occur in the L&M master dictionary.
#' 
#' lm.negative.count = The number of L&M Financial-Negative words in the filing.
#' 
#' lm.positive.count = The number of L&M Financial-Positive words in the filing.
#' 
#' lm.strong.modal.count = The number of L&M Financial-Strong Modal words in the filing.
#' 
#' lm.moderate.modal.count = The number of L&M Financial-Moderate Modal words in the filing.
#' 
#' lm.weak.modal.count = The number of L&M Financial-Weak Modal words in the filing.
#' 
#' lm.uncertainty.count = The number of L&M Financial-Uncertainty words in the filing.
#' 
#' lm.litigious.count =  The number of L&M Financial-Litigious words in the filing.
#' 
#' harvard.negative.count = Number of words in the filing. that occur in the Harvard 
#' General Inquirer Negative word list, as defined by L&M. 
#' 
#' @examples
#' \dontrun{
#' 
#' senti.df <- getSentiment(cik.no = c('1000180', '38079'), 
#'                          form.type = '10-K', filing.year = 2006) 
#'                          
#' ## Returns dataframe with sentiment measures of firms with CIKs 
#' 1000180 and 38079 filed in year 2006 for form type '10-K'.
#' 
#' senti.df <- getSentiment(cik.no = '38079', form.type = c('10-K', '10-Q'), 
#'                          filing.year = c(2005, 2006))
#'}

getSentiment <- function(cik.no, form.type, filing.year) {
    
    output <- getFilings(cik.no, form.type, filing.year, quarter = c(1, 2, 3, 4), 
						 downl.permit = "y")
    
    if (is.null(output)){
      # cat("Please check the CIK number.")
      return()
    }
    
    cat("Computing sentiment measures...\n")
    
    # load Loughran & McDonald Master Dictionary and import word lists
    LoadLMDictionary <- function() {
      
      load(system.file("data/LMMasterDictionary.rda", package = "edgar"))
      
      uncertainty <- ifelse(LMMasterDictionary$uncertainty != 0, LMMasterDictionary$word, NA)
      uncertainty <- uncertainty[!is.na(uncertainty)]
      
      negative <- ifelse(LMMasterDictionary$negative != 0, LMMasterDictionary$word, NA)
      negative <- negative[!is.na(negative)]
      
      positive <- ifelse(LMMasterDictionary$positive != 0, LMMasterDictionary$word, NA)
      positive <- positive[!is.na(positive)]
      
      litigious <- ifelse(LMMasterDictionary$litigious != 0, LMMasterDictionary$word, NA)
      litigious <- litigious[!is.na(litigious)]
      
      strong.modal <- ifelse(LMMasterDictionary$modal == 1, LMMasterDictionary$word, NA)
      strong.modal <- strong.modal[!is.na(strong.modal)]
      
      moderate.modal <- ifelse(LMMasterDictionary$modal == 2, LMMasterDictionary$word, NA)
      moderate.modal <- moderate.modal[!is.na(moderate.modal)]
      
      weak.modal <- ifelse(LMMasterDictionary$modal == 3, LMMasterDictionary$word, NA)
      weak.modal <- weak.modal[!is.na(weak.modal)]
      
      harvard.iv <- ifelse(LMMasterDictionary$harvard_iv != 0, LMMasterDictionary$word, NA)
      harvard.iv <- harvard.iv[!is.na(harvard.iv)]
      
      lm.out <- list(LMMasterDictionary = LMMasterDictionary, uncertainty = uncertainty, 
                     negative = negative, positive = positive, 
                     litigious = litigious, strong.modal =strong.modal,
                     moderate.modal = moderate.modal, weak.modal = weak.modal,
                     harvard.iv = harvard.iv)
      
      return(lm.out)
    }
    
    
    lm.dict <- LoadLMDictionary()

    progress.bar <- txtProgressBar(min = 0, max = nrow(output), style = 3)
    
    output$file.size <- NA
    output$char.count <- NA
    output$complex.word.count  <- NA
    output$lm.dictionary.count <- NA
    output$lm.negative.count <- NA
    output$lm.positive.count <- NA
    output$lm.strong.modal.count <- NA
    output$lm.moderate.modal.count <- NA
    output$lm.weak.modal.count <- NA
    output$lm.uncertainty.count <- NA
    output$lm.litigious.count <- NA
    output$harvard.negative.count <- NA
    
    for (i in 1:nrow(output)) {
        
        f.type <- gsub("/", "", output$form.type[i])

        dest.filename <- paste0("Edgar filings_full text/Form ", f.type, 
                                "/", output$cik[i], "/", output$cik[i], "_", f.type, "_", 
                                output$date.filed[i], "_", output$accession.number[i], ".txt")
        
        # Read filing
        filing.text <- readLines(dest.filename)
        
        # Take data from first <DOCUMENT> to </DOCUMENT>
        doc.start.line <- (grep("<DOCUMENT>", filing.text, ignore.case = TRUE)[1])
        doc.end.line   <- (grep("</DOCUMENT>", filing.text, ignore.case = TRUE)[1])
        
        if( (!is.na(doc.start.line)) & (!is.na(doc.end.line)) ){
          filing.text <- filing.text[doc.start.line : doc.end.line]
        }
        
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
        f.text <- gsub("[[:digit:]]", "", f.text, perl=T)
        f.text <- iconv(f.text, from = 'UTF-8', to = 'ASCII//TRANSLIT')
        f.text <- tolower(f.text)
        f.text <- gsub("\\s{2,}", " ", f.text)
        
        ### Clean text and find number of total words
        text_words <- unlist(strsplit(f.text, " "))
        text_df <- data.frame(word = unlist(text_words), nchar = nchar(text_words))
        text_df <- text_df[text_df$nchar >=3, ]
        text_df <- text_df[!(text_df$word %in% tm::stopwords("en")), ]
        file.size <- nrow(text_df)   # Word count
        char.count <- sum(text_df$nchar)
        
        # Complex word count
        syllables.counts <- sapply(regmatches(text_df$word, gregexpr("[aeiouy]", 
                                                                     text_df$word, ignore.case = T)), length)
        syllables.counts <- syllables.counts[syllables.counts >= 3]
        complex.word.count <- length(syllables.counts)
        
        ############################## Sentiment Measures #############################
        lm.master.dict <- data.frame(word = unique(lm.dict$LMMasterDictionary$word))
        lm.master.merge <- merge(text_df, lm.master.dict, by = "word")
        lm.dictionary.count <- nrow(lm.master.merge)
        
        # Loughran-McDonald Negative word proportion
        lm.negative.count <-  nrow(text_df[which(text_df$word %in% lm.dict$negative), ])

        # Loughran-McDonald positive word proportion
        lm.positive.count <-  nrow(text_df[which(text_df$word %in% lm.dict$positive), ])

        # Loughran-McDonald strong.modal word proportion
        lm.strong.modal.count <-  nrow(text_df[which(text_df$word %in% lm.dict$strong.modal), ])

        # Loughran-McDonald moderate.modal word proportion
        lm.moderate.modal.count <-  nrow(text_df[which(text_df$word %in% lm.dict$moderate.modal), ])
        
        # Loughran-McDonald weak.modal word proportion
        lm.weak.modal.count <-  nrow(text_df[which(text_df$word %in% lm.dict$weak.modal), ])
        
        # Loughran-McDonald uncertainty word proportion
        lm.uncertainty.count <-  nrow(text_df[which(text_df$word %in% lm.dict$uncertainty), ])
        
        # Loughran-McDonald litigious word proportion
        lm.litigious.count <-  nrow(text_df[which(text_df$word %in% lm.dict$litigious), ])
        
        # harvard.negative.count
        harvard.negative.count <-  nrow(text_df[which(text_df$word %in% lm.dict$harvard.iv), ])
        
        # Assign all the varibles
        output$file.size[i] <- file.size
        output$char.count[i] <- char.count
        output$complex.word.count [i] <- complex.word.count 
        output$lm.dictionary.count[i] <- lm.dictionary.count
        output$lm.negative.count[i] <- lm.negative.count
        output$lm.positive.count[i] <- lm.positive.count
        output$lm.strong.modal.count[i] <- lm.strong.modal.count
        output$lm.moderate.modal.count[i] <- lm.moderate.modal.count
        output$lm.weak.modal.count[i] <- lm.weak.modal.count
        output$lm.uncertainty.count[i] <- lm.uncertainty.count
        output$lm.litigious.count[i] <- lm.litigious.count
        output$harvard.negative.count[i] <- harvard.negative.count
        
        # update progress bar
        setTxtProgressBar(progress.bar, i)

    }
    

    # Close progress bar
    close(progress.bar)
    
    output$status <- NULL
    output$quarter <- NULL
    output$filing.year <- NULL
    
    ## convert dates into R dates
    output$date.filed <- as.Date(as.character(output$date.filed), "%Y-%m-%d")

    return(output)
}

globalVariables('LMMasterDictionary')