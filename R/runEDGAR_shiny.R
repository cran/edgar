#' Shiny app for EDGAR filling management.
#'
#' \code{RunEdgarShiny } is a shiny app can download quarterly master index, edgar fillings, 
#' daily master index, and do sentiment analysis on 10-K statement.
#'
#' RunEdgarShiny  app has tabs: 'Get Master Index', 'Fillings Info', 'Download Fillings',
#' 'Get Daily EDGAR Info', and 'Sentiment Analysis of 10K'.
#' User can have GUI for EDGAR filling management and for sentiment analysis 
#' of 10-K statement. See the directory structure at home page.
#' Do not run the app on any browser, the app can only run on R shiny app GUI.
#' 
#' @examples
#' \dontrun{
#' 
#' RunEdgarShiny()
#' }
#' 
RunEdgarShiny <- function() {
    appDir <- system.file("shiny-edgar", "edgarapp", package = "edgar")
    if (appDir == "") {
        stop("Could not find edgar directory. Try re-installing `edgar`.", call. = FALSE)
    }
	options(warn = -1)  # remove warnings    
    shiny::runApp(appDir, display.mode = "normal")
} 
