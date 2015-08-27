# create year list from 1994 to current year
tableyr <- as.character(c(1994:as.numeric(format(Sys.Date(), "%Y"))))
masteryr <- c("ALL",tableyr)

sidebar <- dashboardSidebar(
	sidebarMenu( 
	menuItem("Home", tabName = "homtab", icon = icon("home")),
	menuItem("Get Master Index", tabName = "getmaster", icon = icon("dashboard")),
	menuItem("Fillings Info", icon = icon("th"), tabName = "getfillings"),
	menuItem("Download Fillings", icon = icon("download"), tabName = "fillingsdownload"),
	menuItem("Get Daily EDGAR Info", icon = icon("th"), tabName = "getdailyinfo"),
	menuItem("Sentiment Analysis of 10K", icon = icon("cog"), tabName = "senti")
    ))

body <- dashboardBody(
	tabItems( 
		tabItem(tabName = "homtab",
            # applied css for whole shiny app
		        tags$head(
		          tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
		        ),
            actionButton("change.wd", "Change working directory"),
            br(),br(), tags$img(src = 'edgarimg.jpg', heigth = 700, width = 1100)
        ),
      
		tabItem(tabName = "getmaster",
			fluidRow(
				box(
					width = 8, status = "primary", solidHeader = TRUE,background = "yellow",
					title = "Download EDGAR Master ZIP Files",
					selectizeInput("getmasteryears", "Select Years:" ,choices = masteryr, 
									selected = NULL, multiple = TRUE, options = NULL),
					actionButton("down.master", "Download Master"),br(), br(),
					uiOutput("master.downlaod.status")
				)
			)
		),
    
		tabItem(tabName = "getfillings",
		    fluidRow(
				box(
					width = 8, status = "primary", solidHeader = TRUE, background = "yellow",
					title = "Get Yearly EDGAR Filling Information",            
							selectInput("year.getinfo", "Select Year:",choices = tableyr, selected = NULL),
				    actionButton("getinfo", "Get Info")
				)
			),
				uiOutput("tb"),
				tags$head(tags$style("tfoot {display: table-header-group;}"))
		),

		tabItem(tabName = "fillingsdownload",
		    fluidRow(
				box(
					width = 8, status = "primary", solidHeader = TRUE, background = "yellow",
					title = "Downlaod EDGAR Fillings", 
					selectInput("year3", "Select Years:", choices = c("",tableyr), selected = NULL),
					textInput("cik3", "Type CIK"),
					textInput("ftype3", "Type FORM TYPE"),
					actionButton("downfillings", "Downlaod Fillings")
				)
			),
				uiOutput("stat.table.filling")
		),
    
		tabItem(tabName = "getdailyinfo", 
			fluidRow(
				box(
					width = 8, status = "primary", solidHeader = TRUE,background = "yellow",
					title = "Get Daily EDGAR Filling Information", 
					dateInput("dailydate", label = 'Select the date:', value = NULL),
					actionButton("get.dailyinfo", "Get Info")
                
				)
			),
					br(),
					uiOutput("daily.tb")
		),
		
		tabItem(tabName = "senti",
			fluidRow(
				box(
					width = 7, status = "primary", solidHeader = TRUE, background = "yellow",
						title = "Sentiment Analysis of 10K statement",
					actionButton("get.sentianalysis", "choose 10K filling")
				),
				box(
					width = 7, status = "primary", solidHeader = TRUE, background = "red", 
						collapsible = TRUE, title = "Negative word_cloud",
					uiOutput("negwordcloud1")
				), 
				box(
					width = 7, status = "primary", solidHeader = TRUE, background = "green", 
						collapsible = TRUE, title = "Positive word_cloud",
					uiOutput("poswordcloud1")
				),
				box(
					width = 7, status = "primary", solidHeader = TRUE, background = "yellow", 	
						collapsible = TRUE,  title = "Polarity histogram",
					uiOutput("polarity.hist1")
				)
        
			)
		)
	)
)

# create dashboardPage
dashboardPage( skin="yellow",
		dashboardHeader(title = "EDGAR filling mgmt"),
		sidebar,
		body
)
