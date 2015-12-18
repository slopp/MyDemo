library(shiny)
library(dplyr)
library(dplyr)
library(pscl)
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
                valueBox(nrow(carrier.names), "Total Carriers", icon=icon("info-sign", lib="glyphicon"), color="teal"),
                infoBoxOutput("carrierflights")
              )
      ),
      
      
      tabItem(tabName="pivot", 
              rpivotTableOutput("pivot.table")
      ),
      
      tabItem(tabName = "model",
            fluidRow(
              box(title = "Select City", status = "warning",solidHeader = TRUE,
                 selectizeInput(inputId = "city", label="Select Departure City", choices=cities)
              ),
              box(title="Model Description",status="info",solidHeader = TRUE,
                  p('A linear model will be built to predict arrival delay by airline while accounting for the other variables.'),
                  actionButton(inputId = "runModel", label = "Run Model")
              )
            ),
            
            fluidRow(
              box(title = "Results", status = "success", solidHeader=TRUE,width = 12,
                  verbatimTextOutput("modelDetails"),
                  plotOutput("delayByAirline")
                  
              )
            )
      )
      
    )
  )
)

server <- function(input, output){
  carrier.data <- reactive({
    validate(need(input$carrier != "", "Pleasedplyr::select a Carrier"))
    validate(need(input$var != "", "Pleasedplyr::select a Variable"))
    carrier.code <- carriers %>%dplyr::select(code) %>% filter(description == input$carrier)
    carrier.code <- collect(carrier.code)
    q <- flights %>%dplyr::select(which(colnames(flights)==input$var)) %>% filter(uniquecarrier == carrier.code$code)
    collect(q)
  })
  
  full.data <- function(){
    print("loading")
    full.data <- flights  %>%dplyr::select(year,month,dayofweek,uniquecarrier,arrdelay,depdelay,distance,origin)
    collect(full.data)
  }
  
  output$hist <- renderPlot({
    validate(need(dim(carrier.data())[1]>0, "No flight information for this carrier"))
    ggplot(data=carrier.data(), aes_string(input$var)) + geom_histogram() +ggtitle(paste(input$carrier, input$var))
  })
  
  output$carrierflights <- renderValueBox({
    valueBox(
      dim(carrier.data())[1], paste("Total Flights for", input$carrier), color='teal')
  })
  
  output$pivot.table <- renderRpivotTable({
    rpivotTable(full.data())
  })
  
  run.model <- eventReactive(input$runModel, {
    city.data <- flights %>%dplyr::select(everything()) %>% filter(arrdelay>=0) %>% filter(!is.na(arrdelay)) # %>% filter(origin == input$city)
    city.data <- collect(city.data)
    m <- zeroinfl(data=city.data, arrdelay ~ uniquecarrier + factor(dayofweek) + distance | 1)
     
  })
  
  parse.model <- function(m){
    coef <- as.data.frame(m$coefficients$count)
    coef <- cbind(coef, rownames(coef)) 
    colnames(coef)<- c("coefficient","airline")
    coef<-mutate(coef, carrier=grepl(pattern = "unique",coef$airline))
    coef <- coef %>% filter(carrier) %>% dplyr::select(-carrier)%>% mutate(airline=gsub("uniquecarrier",replacement = "",x = airline))
    carrier.data <- carriers %>% dplyr::select(everything()) %>% distinct()
    carrier.data <- collect(carrier.data)
    coef <- dplyr::left_join(coef,carrier.data, by=c("airline"="code"), copy=TRUE)
    coef<- coef %>% dplyr::select(description, coefficient) %>% arrange(coefficient)
  }
  
  output$modelDetails <- renderText({
    m <- run.model()
    as.character(m$call)
  })
  
  output$delayByAirline <- renderPlot({
    ggplot(t, aes(x=description, y=coefficient))+ geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + xlab("Airline")

  })
}

shinyApp(ui, server)