# create year list from 1994 to current year
yr <- as.character(c(1994:as.numeric(format(Sys.Date(), "%Y"))))

# message for choosing working directory
tryCatch({
    setwd(jchoose.dir(default = getwd(), caption = "Choose working directory"))
}, error = function(e) {
    msg0 <- paste0("All files will be downloaded into current working directory: ", 
	               getwd(), "\n You can change working directory from home page")
    err <- tcltk::tkmessageBox(message = msg0, icon = "info")
})

## import functions from edgar_shiny_functions.R 
source(system.file("shiny-edgar/edgarapp/edgar_shiny_functions.R", package = "edgar"),local=TRUE)

# read positive and negative dictionary
neg.words <- utils::read.csv(system.file("data/negwords.csv", package = "edgar"))
pos.words <- utils::read.csv(system.file("data/poswords.csv", package = "edgar"))
neg.words <- neg.words$WORDS
pos.words <- pos.words$WORDS


shinyServer(function(input, output, session) {
  
    # Event observer for Change working directory button
    observeEvent(input$change.wd, {
        tryCatch({
            setwd(jchoose.dir(default = getwd(), caption = "Choose working directory"))
        }, error = function(e) {
            msg01 <- paste0("All files will be downloaded into current working directory: ", 
			                getwd(), "\n You can change working directory from home page")
            err <- tcltk::tkmessageBox(message = msg01, icon = "info")
        })
    })
    
    # Event observer for Get Master Tab
    observeEvent(input$down.master, {
        if (input$getmasteryears > 0) {
            dir.create("Master Files")
            year.array <- unlist(strsplit(input$getmasteryears, " "))
            if ("ALL" %in% year.array) {
                year.array <- as.character(c(1994 : as.numeric(format(Sys.Date(), "%Y"))))
            }
 
            output$master.downlaod.status <- renderTable({
                withProgress(message = "Downloading Master:", value = 0.01, {
					DownloadMasterShiny(year.array)
                })
            }, escape = FALSE)
            
        }
    })
    
    # Event observer for Filling Info Tab
    observeEvent(input$getinfo, {
        if (input$year.getinfo > 0) {
            year <- input$year.getinfo
            if (file.exists(paste0("Master Files/", year, "master.Rda"))) {
				withProgress(message = "Please Wait", {
					load(paste0("Master Files/", year, "master.Rda"))
					data <- year.master
				})
				
				if(nrow(data)>0){
					datasetInput <- reactive({
						CreateLinkShiny(data)
					})
                
                output$master.table <- renderDataTable({
					datasetInput()
                }, escape = FALSE)
                
                withProgress(message = "Please Wait", {
					output$tb <- renderUI({
					dataTableOutput("master.table")
					})
					stop()
                })
				} else {
					output$tb <- renderText({
					("Rda file is corrupted. Please re-download the master Index file for the selected year using 'Get Master Index' tab.")
					})
                stop()                
				} 
            } else {
                output$tb <- renderText({
					("Rda file not found in the current directory. Please firstly download the master Index file for the selected year using 'Get Master Index' tab.")
                })
                stop()
            }
        }
    })

    
    # Event handler for download filling Tab
    output$stat.table.filling1 <- renderDataTable({
        input$downfillings
        year3 <- isolate(input$year3)
        cik3 <- isolate(input$cik3)
        ftype3 <- isolate(input$ftype3)
        
        if (year3 > 0 & cik3 > 0 & ftype3 > 0) {
            withProgress(message = "Please Wait", {
                isolate(DownloadFillingsShiny(year3, cik3, ftype3))
            })
        }
    }, escape = FALSE)

    output$stat.table.filling <- renderUI({
        dataTableOutput("stat.table.filling1")
    })
    
    # Event handler for daily info tab
    observeEvent(input$get.dailyinfo, {
        withProgress(message = "Please Wait", {
            
            if (input$dailydate > 0) {
                dir.create("Daily_Index")
                if (as.numeric(Sys.Date()) > input$dailydate) {
					date <- as.character(input$dailydate)
					date <- unlist(strsplit(date, "-"))
					year <- date[1]
					month <- date[2]
					day <- date[3]
					  
					data <- GetDailyInfoShiny(day, month, year)
                  
					if (data != 0) {
						output$daily.table <- renderDataTable({
							withProgress(message = "Please Wait", {
							data
							})
                    }, escape = FALSE)
                    
                    output$daily.tb <- renderUI({
                      dataTableOutput("daily.table")
                    })
					} else {
						output$daily.table <- renderText({
							"Server error or daily index not available for this date"
						})
                    
                    output$daily.tb <- renderUI({
						textOutput("daily.table")
                    })
					}
                } else {
					output$daily.table <- renderText({
						"Please select the appropriate date."
					})
                  
					output$daily.tb <- renderUI({
						textOutput("daily.table")
					})
                }
            }
        })
    })
    
    # Event observer for sentiment analysis tab
    observeEvent(input$get.sentianalysis, {
        withProgress(message = "Please Wait", {
            filpath <- jchoose.files(default = getwd(), caption = "Select 10-K file", multi = FALSE)
            
            if (grepl("10-K", filpath) && grepl(".txt", filpath)) {
                word_frq <- GetwordfrqShiny(filpath)
                words <- unlist(word_frq$WORD)
                neg.word.table <- word_frq[words %in% neg.words, ]
                pos.word.table <- word_frq[words %in% pos.words, ]
                
                output$negwordcloud <- renderPlot({
					wordcloud::wordcloud(words = neg.word.table$WORD, freq = neg.word.table$FREQUENCY, scale = c(4, 0.8), 
				                         max.words = Inf, random.order = F, colors = RColorBrewer::brewer.pal(8, "Dark2"))
                })
                
                output$poswordcloud <- renderPlot({
					text(x = 0.5, y = 0.5, "Title of my first plot")
					wordcloud::wordcloud(words = pos.word.table$WORD, freq = pos.word.table$FREQUENCY, scale = c(4, 0.8), 
							             max.words = Inf, random.order = F, colors = RColorBrewer::brewer.pal(8, "Dark2"))
                })
                
                res <- data.frame(Polarity = c("Negative", "Positive"), 
								  Total_words = c(sum(neg.word.table$FREQUENCY), sum(pos.word.table$FREQUENCY)))
                
                output$polarity.hist <- renderPlot({
					graphics::plot(Total_words~Polarity,data=res,main="Polarity Plot",xlab="Polarity",ylab="Frequency")
                })
                
                output$negwordcloud1 <- renderUI({
					plotOutput("negwordcloud")
                })
                output$poswordcloud1 <- renderUI({
					plotOutput("poswordcloud")
                })
                output$polarity.hist1 <- renderUI({
					plotOutput("polarity.hist")
                })
                rm(text)
            } else {
                
                output$senti.error <- renderText({
					"Error: Please select 10-K filling"
                })
                
                output$negwordcloud1 <- renderUI({
					NULL
                })
                
                output$poswordcloud1 <- renderUI({
					NULL
                })
                
                output$polarity.hist1 <- renderUI({
					NULL
                })
                
                msg33 <- "Please select 10-K file only.."
                err <- tcltk::tkmessageBox(message = msg33, icon = "error")
            }
        })
    })
}) 
