#' Creates wordcloud of words from EDGAR filing.
#'
#' \code{getWordcloud} creates wordcloud of words from filing.
#'
#' getWordcloud function takes words frequency data frame as an input from 
#' \link[edgar]{getWordfrquency} function. It compares this words frequency data frame 
#' with the input dictionary and generates wordcloud using match words.
#'  
#' @usage getWordcloud(word.frq, words.list)
#'  
#' @param word.frq Word frequency data frame created using 
#' \link[edgar]{getWordfrquency} function.
#' @param words.list Word list as a sentiment dictionary.
#' 
#' @return Function creates wordcloud.
#'   
#' @examples
#' \dontrun{
#' 
#' words.list <- scan(system.file('data/negwords.txt', package = 'edgar'), what='character')
#' ## User can apply any desired user defined dictionary other than 
#' ## default dictionaries from this package.
#' 
#' getWordcloud(word.frq, words.list)
#' }
#' 

getWordcloud <- function(word.frq, words.list) {
  
  if (!is.data.frame(word.frq)) {
    msg1 <- "word.frq is not a data frame"
    cat(msg1)
    return()
  }
  
  if (nrow(word.frq) == 0) {
    msg2 <- "word.frq data frame is empty"
    cat(msg2)
    return()
  }
  col <- paste0(names(word.frq), collapse = " ")
  
  options(warn = -1)  # remove warnings
  
  if (grepl("FREQUENCY", col) && grepl("WORD", col)) {
    
    words <- unlist(word.frq$WORD)
    word.table <- word.frq[words %in% words.list, ]
    # Creates wordcloud
    wordcloud::wordcloud(words = word.table$WORD, freq = word.table$FREQUENCY, 
      scale = c(4, 0.8), max.words = Inf, random.order = F, 
      colors = RColorBrewer::brewer.pal(8, "Dark2"))
  } else {
    msg3 <- "word.frq data frame is invalid"
    cat(msg3)
    return()
  }
  
}
