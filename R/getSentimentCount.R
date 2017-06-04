#' Parse sentiment words from EDGAR filing.
#'
#' \code{getSentimentCount} get sentiment words count from filing.
#'
#' getSentimentCount function takes words frequency dataframe as an input from 
#' \link[edgar]{getWordfrquency} function. It compares these words 
#' with the input dictionary and parse matched words with their frequencies.
#'  
#' @usage getSentimentCount(word.frq, words.list)
#'  
#' @param word.frq Word frequency dataframe created using 
#' \link[edgar]{getWordfrquency} function.
#' @param words.list Word list as a sentiment dictionary.
#' 
#' @return Function returns sentiment words frequency dataframe.
#'   
#' @examples
#' \dontrun{
#' 
#' words.list <- scan(system.file('data/negwords.txt', package = 'edgar'), what='character')
#' ## User can apply any desired user defined dictionary other than 
#' ## default dictionaries from this package in txt format with each word in separate line.
#' 
#' senti.words <- getSentimentCount(word.frq, words.list)
#' }
#' 

getSentimentCount <- function(word.frq, words.list) {
  
  if (!is.data.frame(word.frq)) {
    msg1 <- "word.frq is not a valid dataframe"
    cat(msg1)
    return()
  }
  
  if (nrow(word.frq) == 0) {
    msg2 <- "word.frq dataframe is empty"
    cat(msg2)
    return()
  }
  col <- paste0(names(word.frq), collapse = " ")
  
  if (grepl("FREQUENCY", col) && grepl("WORD", col)) {
    
    words <- unlist(word.frq$WORD)
    word.table <- word.frq[words %in% words.list, ]
    rownames(word.table) <- 1:nrow(word.table)
    
    return(word.table)
    
  } else {
    msg3 <- "word.frq dataframe is invalid"
    cat(msg3)
    return()
  }
  
}



