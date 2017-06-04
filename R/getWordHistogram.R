#' Creates histogram of most frequent words in EDGAR filing.
#'
#' \code{getWordHistogram} creates histogram of most frequent words in filing.
#'
#' getWordHistogram function takes words frequency dataframe as an input from 
#' \link[edgar]{getWordfrquency} function. It compares these words 
#' with input dictionary and generates histogram of 15 most frequent matched words 
#' with their frequencies.
#'
#' @usage getWordHistogram(word.frq, words.list)
#'
#' @param word.frq Word frequency dataframe created using 
#' \link[edgar]{getWordfrquency} function.
#' @param words.list Word list as a sentiment dictionary.
#' 
#' @return Function creates histogram.
#'   
#' @examples
#' \dontrun{
#' 
#' words.list <- scan(system.file('data/negwords.txt', package = 'edgar'), what='character')
#' ## User can apply any desired user defined dictionary other than 
#' ## default dictionaries from this package.
#' 
#' getWordHistogram(word.frq, words.list)
#' }
#' 

getWordHistogram <- function(word.frq, words.list) {
  
  if (!is.data.frame(word.frq)) {
    msg1 <- "Input for word frequency dataframe is not a valid dataframe"
    cat(msg1)
    return()
  }
  
  if (nrow(word.frq) == 0) {
    msg2 <- "Word frequency dataframe is empty"
    cat(msg2)
    return()
  }
  col <- paste0(names(word.frq), collapse = " ")
  
  options(warn = -1)  # remove warnings
  
  if (grepl("FREQUENCY", col) && grepl("WORD", col)) {
    
    words <- unlist(word.frq$WORD)
    word.table <- word.frq[words %in% words.list, ]
    
    # get top 15 negative words occurred in 10-K statement
    top.word.data <- word.table[1:15, ]
    top.word.data <- top.word.data[order(-top.word.data$FREQUENCY), ]
    
    # Creates negative words histogram
    ggplot2::qplot(top.word.data$WORD, weight = top.word.data$FREQUENCY, 
      data = top.word.data, geom = "bar", xlab = "Word", 
      ylab = "Frequency") + ggplot2::coord_flip() + ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1, 
      vjust = 0.5)) + ggplot2::geom_bar(fill = "#FF5733") + 
      ggplot2::theme(axis.text = ggplot2::element_text(size = 11, 
        face = "bold"), axis.title = ggplot2::element_text(size = 13, 
        face = "bold")) + ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", 
      colour = "black"))
    
  } else {
    msg3 <- "Word frequency dataframe is invalid"
    cat(msg3)
    return()
  }
  
}
