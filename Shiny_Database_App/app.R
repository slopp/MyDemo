library(shiny)
library(dplyr)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(rpivotTable)

#-----------------------
# Connect to Database
#-----------------------


air <- src_postgres(dbname = 'airontime', 
                    host = 'localhost', 
                    port = '5432', 
                    user = 'psql_user', 
                    password = 'ABCd4321')

# Tables
carriers <- tbl(air, "carriers")
flights  <- tbl(air, "flights")

#Carrier Names
carrier.names <- carriers %>% dplyr::select(description) %>% distinct()
carrier.names <-collect(carrier.names) %>% arrange(description)

#Cities
cities <- flights %>% dplyr::select(origin) %>% distinct()
cities <- collect(cities)

#-----------------------
# Shiny Dashboard
#-----------------------

#---------
# UI
#---------
ui <- dashboardPage(
  dashboardHeader(title="Airline Delay Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Explore Fields", tabName="explore"),
      menuItem("Pivot Table", tabName="pivot"),
      menuItem("Model", tabName="model")
    ) 
  ),
  dashboardBody(
    #--- Explore Pane
    tabItems(
      tabItem(tabName="explore",
              fluidRow(
                box(title="Inputs", status="warning",solidHeader = TRUE,
                   selectizeInput(inputId = "carrier",label="Select Carrier",choices=carrier.names,selected="American Airlines Inc."),
                   selectizeInput(inputId = "var",label="Select Variable",choices=colnames(flights),selected="arrdelay")
                ),
                
                box(title="Results", status="primary", solidHeader = TRUE,
                    hr("Distribution of Delay Times"),
                    plotOutput("hist")
                )
                
              ),
              
              fluidRow(
                valueBox(nrow(carrier.names), "Total Carriers", icon=icon("info-sign", lib="glyphicon"), color="red"),
                infoBoxOutput("carrierflights")
              )
      ),
      
      #--- Pivot Tabel Pane
      tabItem(tabName="pivot", 
            rpivotTableOutput("pivot")
      ),
      
      #--- Model Pane
      tabItem(tabName = "model",
            fluidRow(
              box(title = "Model Delay", status = "warning",solidHeader = TRUE,
                 selectizeInput(inputId = "mtype", label="Arrival or Departure", choices=c('arrdelay','depdelay'))
              ),
              box(title="Model Description",status="info",solidHeader = TRUE,
                  p('A simple linear model will be fit (for the sake of time). In reality, a zero-inflated gamma model would be more appropriate.'),
                  actionButton(inputId = "runModel", label = "Run Model")
              )
            ),
            
            fluidRow(
              box(title = "Results", status = "success", solidHeader=TRUE,width = 12,
                  plotOutput("delayByAirline"),
                  verbatimTextOutput("modelDetails")
                  
              )
            )
      )
      
    )
  )
)

#--------
# Server
#---------
server <- function(input, output){
  
  
  #------------- Explore Tab
  
  # gets data by carrier for the explore pane
  carrier.data <- reactive({
    validate(need(input$carrier != "", "Please select a Carrier"))
    validate(need(input$var != "", "Pleased select a Variable"))
    carrier.code <- carriers %>%dplyr::select(code) %>% filter(description == input$carrier)
    carrier.code <- collect(carrier.code)
    q <- flights %>%dplyr::select(which(colnames(flights)==input$var)) %>% filter(uniquecarrier == carrier.code$code)
    collect(q)
  })
  
  # histogram (for the explore pane)
  output$hist <- renderPlot({
    validate(need(dim(carrier.data())[1]>0, "No flight information for this carrier"))
    ggplot(data=carrier.data(), aes_string(input$var)) + geom_histogram() +ggtitle(paste(input$carrier, input$var))
  })
  
  # value box (lists # of data points in the explore pane)
  output$carrierflights <- renderInfoBox({
    infoBox(
      dim(carrier.data())[1], paste("Total Flights for", input$carrier), color='teal')
  })
  
  #--------------- Pivot Table Tab
  
  # gets all data (for the rpivotTable)
  full.data <- function(){
    
    full.data <- flights%>%dplyr::select(year,month,dayofweek,uniquecarrier,arrdelay,depdelay,distance,origin)
    full.data <- as.data.frame(collect(full.data))

  }
  
  output$pivot <- renderRpivotTable({
    progress <- shiny::Progress$new()
    progress$set(message ="Fetching Data", value=0.2)
    on.exit(progress$close())
    
    rpivotTable(full.data(), width="100%", height="500px")
    
  })
  
  
  #---------------- Model Tab
  
  # supporting function to parse out coefficients
  parse.model <- function(m){
    coef <- as.data.frame(m$coefficients)
    coef <- cbind(coef, rownames(coef)) 
    colnames(coef)<- c("coefficient","airline")
    coef<-mutate(coef, carrier=grepl(pattern = "unique",coef$airline))
    coef <- coef %>% filter(carrier) %>% dplyr::select(-carrier)%>% mutate(airline=gsub("uniquecarrier",replacement = "",x = airline))
    carrier.data <- carriers %>% dplyr::select(everything()) %>% distinct()
    carrier.data <- collect(carrier.data)
    coef <- dplyr::left_join(coef,carrier.data, by=c("airline"="code"), copy=TRUE)
    coef<- coef %>% dplyr::select(description, coefficient) %>% arrange(coefficient)
  }
  
  
  # reactive event that changes when the model is run
  run.model <- eventReactive(input$runModel, {
    if(input$mtype == "arrdelay"){
      delay.data <- flights %>%dplyr::select(everything()) %>% filter(arrdelay>=0) %>% filter(!is.na(arrdelay))
      delay.data <- collect(delay.data)
      m <- lm(data=delay.data, arrdelay ~ uniquecarrier + factor(dayofweek) + deptime + distance -1)
    }
    
    if(input$mtype == "depdelay"){
      delay.data <- flights %>%dplyr::select(everything()) %>% filter(depdelay>=0) %>% filter(!is.na(depdelay))
      delay.data <- collect(delay.data)
      m <- lm(data=delay.data, depdelay ~ uniquecarrier + factor(dayofweek) + deptime-1)
    }
    
    the.coefs <- parse.model(m)
    the.coefs$description <- strtrim(the.coefs$description,30)
    the.coefs$classify <- ifelse(the.coefs$coefficient >0, "greater delay","less delay")
    
    the.call  <- as.character(m$call)
    the.call <- paste(the.call[1],"(", the.call[2],")")
    results <- list(the.coefs, the.call)
  })
  
  
  output$modelDetails <- renderText({
    results <- run.model()
    results[[2]]
  })
  
  output$delayByAirline <- renderPlot({
    the.results <- run.model()
    the.coef <- the.results[[1]]
    ggplot(the.coef, aes(x=description, y=coefficient, fill=classify))+ geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + xlab("Airline")

  })
}

shinyApp(ui, server)