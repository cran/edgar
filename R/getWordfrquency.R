#' Creates words frequency data frame from EDGAR filing.
#'
#' \code{getWordfrquency} creates word frequency data frame using filing text.
#'
#' getWordfrquency function takes path of filing which can be download using 
#' \link[edgar]{getFilings} function. Function cleans text from filing
#' and creates words frequency data frame. This data frame is used
#' in \link[edgar]{getWordcloud}, \link[edgar]{getWordHistogram}, and 
#' \link[edgar]{getSentimentCount} functions.
#'  
#' @usage getWordfrquency(filepath)
#'
#' @param filepath Path of downloaded filing.
#'  
#' @return Function returns words frequency data frame.
#'   
#' @examples
#' \dontrun{
#' 
#' word.frq <- getWordfrquency('Edgar Filings/1000180_10-K_2006/1000180_10-K_2006-03-15.txt')
#' }

getWordfrquency <- function(filepath) {
  
  word.frq <- data.frame()
  
  if (grepl(".txt", filepath)) {
    
    if (file.exists(filepath)) {
      
      text <- readLines(filepath)
      text <- paste(text, collapse = " ")
      
      # Extract text from html file
      doc <- XML::htmlParse(text, asText = TRUE)
      text <- XML::xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", 
        XML::xmlValue)
      text <- paste(text, collapse = " ")
      
      # convert into corpus
      text <- tm::Corpus(tm::VectorSource(text))
      # clean text
      cleantext <- function(data.text.corpus) {
        data.text.corpus <- tm::tm_map(data.text.corpus, 
          tm::removePunctuation)  # Remove punctuation marks
        data.text.corpus <- tm::tm_map(data.text.corpus, 
          tm::removeNumbers)  # Remove Numbers
        data.text.corpus <- tm::tm_map(data.text.corpus, 
          tm::stripWhitespace)  # Remove punctuation marks
        data.text.corpus <- tm::tm_map(data.text.corpus, 
          function(x) tm::removeWords(x, tm::stopwords()))  # Remove stop words
        data.text.corpus <- tm::tm_map(data.text.corpus, 
          tm::content_transformer(tolower))  # Convert text to lower case
        return(data.text.corpus)
      }
      text <- cleantext(text)
      word.frq <- tm::termFreq(text[[1]])
      wordMatrix = as.data.frame((as.matrix(word.frq)))
      word.frq <- data.frame(WORD = row.names(wordMatrix), 
        FREQUENCY = wordMatrix[, 1], row.names = NULL)
      
      # Order data frame descending on frequency
      word.frq <- word.frq[order(-word.frq$FREQUENCY), 
        ]
      rownames(word.frq) <- NULL
      
    } else {
      msg2 <- "Filing Does not exists. Please download filing using 'getFilings' function."
      cat(msg2)
      return()
    }
    
  } else {
    msg3 <- "Please select filing in text format"
    cat(msg3)
    return()
  }
  return(word.frq)
}
