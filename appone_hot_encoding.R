

# Application 2

library(shiny)
library(ggplot2)
library(ggExtra)
library(DT)
library(fastDummies)  

# Define UI
ui <- fluidPage(
  titlePanel("Dataset Explorer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV or TXT File:",
                accept = c(".csv", ".txt")),
      uiOutput("var_select"),
      actionButton("encodeBtn", "Apply One-Hot Encoding")  
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", verbatimTextOutput("summaryOutput")),
        tabPanel("Data Table", DT::dataTableOutput("dataTableOutput")),
        tabPanel("One-Hot Encoded Data", DT::dataTableOutput("encodedDataOutput"))  
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      read.csv(input$file$datapath)
    } else if (ext == "txt") {
      read.table(input$file$datapath, header = TRUE)
    }
  })
  
  output$var_select <- renderUI({
    req(dataset())
    cols <- names(dataset())
    tagList(
      selectInput("explanatory", "Explanatory Feature:", choices = cols)
    )
  })
  
  output$summaryOutput <- renderPrint({
    req(input$explanatory)
    summary(dataset()[[input$explanatory]])
  })
  
  output$dataTableOutput <- DT::renderDataTable({
    req(input$explanatory)
    data.frame(Feature = dataset()[[input$explanatory]])
  })
  
  
  encoded_data <- eventReactive(input$encodeBtn, {
    req(input$explanatory)
    dummy_cols(dataset(), select_columns = input$explanatory, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
  })
  
  output$encodedDataOutput <- DT::renderDataTable({
    encoded_data()  
  })
}

# Run the application
shinyApp(ui = ui, server = server)
