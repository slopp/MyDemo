library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)

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
  dashboardSidebar(disable=TRUE),
  dashboardBody(
    fluidRow(
      box(title="Inputs", status="warning",solidHeader = TRUE,
          selectizeInput(inputId = "carrier",label="Select Carrier",choices=carrier.names)
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
  )
)

server <- function(input, output){
  carrier.data <- reactive({
    validate(need(input$carrier != "", "Please Select a Carrier"))
    carrier.code <- carriers %>% select(code) %>% filter(description == input$carrier)
    carrier.code <- collect(carrier.code)
    q <- flights %>% select(arrdelay) %>% filter(uniquecarrier == carrier.code$code)
    collect(q)
  })
  
  output$hist <- renderPlot({
    validate(need(dim(carrier.data())[1]>0, "No flight information for this carrier"))
    ggplot(data=carrier.data(), aes(arrdelay))+geom_histogram()+ggtitle(input$carrier)
  })
  
  output$carrierflights <- renderValueBox({
    valueBox(
      dim(carrier.data())[1], paste("Total Flights for", input$carrier), color='teal')
  })
  
}

shinyApp(ui, server)