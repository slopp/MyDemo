library(shiny)
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
carrier.names <- carriers %>% select(unique(description))
carrier.names <-collect(carrier.names) %>% arrange(description)


#-----------------------
# Shiny Dashboard
#-----------------------

ui <- dashboardPage(
  dashboardHeader(title="Airline Delay Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Explore Fields", tabName="explore"),
      menuItem("Pivot Table", tabName="pivot")
    ) 
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="explore",
              fluidRow(
                box(title="Inputs", status="warning",solidHeader = TRUE,
                    selectizeInput(inputId = "carrier",label="Select Carrier",choices=carrier.names, selected="American Airlines Inc."),
                    selectizeInput(inputId = "var",label="Select Variable",choices=colnames(flights), selected="arrdelay")
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
      )
      
    )
  )
)

server <- function(input, output){
  carrier.data <- reactive({
    validate(need(input$carrier != "", "Please Select a Carrier"))
    validate(need(input$var != "", "Please Select a Variable"))
    carrier.code <- carriers %>% select(code) %>% filter(description == input$carrier)
    carrier.code <- collect(carrier.code)
    q <- flights %>% select(which(colnames(flights)==input$var)) %>% filter(uniquecarrier == carrier.code$code)
    collect(q)
  })
  
  full.data <- function(){
    print("loading")
    full.data <- flights  %>% select(year,month,dayofweek,uniquecarrier,arrdelay,depdelay,distance,origin)
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
  
}

shinyApp(ui, server)