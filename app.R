library(shiny)
library(shinythemes)
library(readr)
library(readxl)
library(jsonlite)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$style(HTML(paste0(
      "body { background-color: #fafafa; color: #333; font-family: 'Roboto', sans-serif; }",
      
      # Navbar and Header
      ".navbar-default { background-color: #3498db; border-color: #2980b9; }",
      ".navbar-default .navbar-brand { color: #ffffff; font-size: 26px; font-weight: 600; }",
      ".navbar-default .navbar-nav > li > a { color: #ffffff; font-size: 16px; }",
      ".navbar-default .navbar-nav > li > a:hover { color: #f39c12; }",
      
      "#title { 
        text-align: center; 
        color: #ffffff; 
        background-color: #3498db; 
        font-size: 36px; 
        font-weight: 700; 
        padding: 30px 0; 
        border-radius: 8px; 
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); 
        margin-bottom: 30px;
        position: relative;
      }",
      "#group-members { 
        position: absolute; 
        bottom: 8px; 
        right: 20px; 
        color: #ffffff; 
        font-size: 14px; 
        font-weight: 400; 
      }",
      
      # Card-like style for each section
      ".well { background-color: #ffffff; border: none; border-radius: 8px; padding: 20px; box-shadow: 0 4px 10px rgba(0,0,0,0.05); margin-bottom: 20px; }",
      ".well .form-group { margin-bottom: 20px; }",
      ".btn-primary { background-color: #3498db; border-color: #2980b9; border-radius: 6px; font-weight: bold; padding: 12px 20px; text-transform: uppercase; transition: background-color 0.3s ease; }",
      ".btn-primary:hover { background-color: #2980b9; }",
      
      # Tabs and Panel
      ".nav-tabs { justify-content: center; margin-bottom: 30px; }",
      ".nav-tabs .nav-link { font-weight: 600; font-size: 16px; padding: 15px 30px; border-radius: 20px; margin: 0 10px; background-color: transparent; transition: background-color 0.3s ease; }",
      ".nav-tabs .nav-link.active { background-color: #3498db; color: white; border-radius: 20px; font-weight: 700; }",
      ".nav-tabs .nav-link:hover { background-color: #f39c12; color: white; }",
      ".tab-pane { padding: 30px; }",
      
      # Step Arrow and Table
      ".step-arrow { font-size: 20px; color: #3498db; font-weight: bold; margin-left: 10px; }",
      "table { width: 100%; border-collapse: collapse; margin-top: 20px; border-radius: 8px; overflow: hidden; }",
      "th, td { padding: 12px; text-align: center; border: 1px solid #ddd; font-size: 14px; }",
      "tr:nth-child(even) { background-color: #f9f9f9; }",
      "th { background-color: #3498db; color: white; font-weight: bold; font-size: 16px; }",
      "td { font-size: 14px; }",
      
      # Instructions box style
      ".instructions-box { background-color: #ecf0f1; border: 1px solid #bdc3c7; border-radius: 8px; padding: 20px; font-size: 14px; margin-left: 30px; max-width: 350px; box-shadow: 0 4px 10px rgba(0, 0, 0, 0.05); }",
      ".instructions-box h5 { font-size: 16px; font-weight: 600; color: #3498db; margin-bottom: 15px; }",
      ".instructions-box p { color: #7f8c8d; }"
    )))
  ),
  
  # Title Section
  div(id = "title", "Data Processing App",
      div(id = "group-members", "Group Members: Preach Apintanapong, Mengyan Li, Hanzhong Yang, Raymond Li")
  ),
  
  # Main Panel for the tabs
  mainPanel(
    div(style = "display: flex; justify-content: center; width: 100%;",
        tabsetPanel(id = "tabsetPanel", type = "tabs",
                    tabPanel("Loading Datasets",
                             fluidRow(
                               column(6, 
                                      div(class = "well",
                                          fileInput("file", "Upload Dataset (.csv, .xlsx, .json, .rds)", accept = c(".csv", ".xlsx", ".json", ".rds")),
                                          textInput("file_path", "Or enter file path:", value = ""),
                                          selectInput("dataset", "Or choose a dataset:",
                                                      choices = c("kdrama", "congress_118_bills")), 
                                          actionButton("load_data", "Load Data", class = "btn-primary")
                                      )
                               ),
                               column(6, 
                                      div(class = "instructions-box",
                                          h5("Instructions for Using the App"),
                                          p("1. Loading Datasets: Upload your dataset in .csv, .xlsx, .json or .rds format. The data will be shown below."),
                                          p("2. Data Cleaning and Preprocessing: Clean your data by removing inconsistencies."),
                                          p("3. Feature Engineering: Create new features or transform existing ones."),
                                          p("4. Exploratory Data Analysis (EDA): Visualize your data to uncover insights."),
                                          p("Click each tab to proceed with the corresponding step.")
                                      )
                               )
                             )
                    ),
                    tabPanel("Data Cleaning and Preprocessing",
                             div(class = "well",
                                 div(class = "tab-panel-header", h4("TBD", style = "font-weight: 600;"),
                                     span(class = "step-arrow", "→")
                                 )
                             )
                    ),
                    tabPanel("Feature Engineering",
                             div(class = "well",
                                 div(class = "tab-panel-header", h4("TBD", style = "font-weight: 600;"),
                                     span(class = "step-arrow", "→")
                                 )
                             )
                    ),
                    tabPanel("Exploratory Data Analysis (EDA)",
                             div(class = "well",
                                 div(class = "tab-panel-header", h4("TBD", style = "font-weight: 600;"),
                                     span(class = "step-arrow", "→")
                                 )
                             )
                    )
        )
    ),
    
    uiOutput("conditional_table")
  )
)

server <- function(input, output, session) {
  
  datasetInput <- reactive({
    req(input$load_data)
    isolate({
      file <- input$file
      if (!is.null(file)) {
        ext <- tools::file_ext(file$name)
        
        cat("File path:", file$datapath, "\n")
        
        dataset <- switch(ext,
                          "csv" = read_csv(file$datapath),
                          "xlsx" = {
                            df <- read_excel(file$datapath)
                            write.csv(df, "temp_data.csv", row.names = FALSE)
                            read_csv("temp_data.csv")
                          },
                          "json" = {
          
                            df <- fromJSON(file$datapath, flatten = TRUE)
                            
                            if (is.list(df)) {
                              if (length(df) == 1) {
                                df <- df[[1]]  
                              }
                              df <- as.data.frame(df) 
                            }
                            
                            df  
                          },
                          "rds" = {
                            df <- readRDS(file$datapath)
                            write.csv(df, "temp_data.csv", row.names = FALSE)
                            read_csv("temp_data.csv")
                          },
                          stop("Unsupported file type")
        )
        
        return(dataset)
      } else if (input$file_path != "") {
        # Load dataset from file path
        path <- input$file_path
        ext <- tools::file_ext(path)
        
        dataset <- switch(ext,
                          "csv" = read_csv(path),
                          "xlsx" = {
                            df <- read_excel(path)
                            write.csv(df, "temp_data.csv", row.names = FALSE)
                            read_csv("temp_data.csv")
                          },
                          "json" = {
                            df <- fromJSON(path, flatten = TRUE)
                            if (is.data.frame(df)) {
                              print("no")
                              return(df)  
                            } else {
                              if (is.list(df)) {
                                print("yay")
                                df <- as.data.frame(df)
                                return(df)
                              } else {
                                stop("Unsupported data format after flattening JSON.")
                              }
                            }
                          },
                          "rds" = {
                            df <- readRDS(path)
                            write.csv(df, "temp_data.csv", row.names = FALSE)
                            read_csv("temp_data.csv")
                          },
                          stop("Unsupported file type")
        )
        return(dataset)
      } else {
        dataset_paths <- list(
          kdrama = "sample_data/top100_kdrama.csv",
          congress_118_bills = "sample_data/congress_118_bills.csv"
        )
        
        path <- dataset_paths[[input$dataset]]
        if (!is.null(path)) {
          if (grepl(".csv$", path)) {
            return(read_csv(path))
          } else if (grepl(".json$", path)) {
            return(fromJSON(path, flatten = TRUE))  
          } else if (grepl(".xlsx$", path)) {
            return(read_excel(path))
          }
        }
      }
    })
  })
  
  output$conditional_table <- renderUI({
    if (input$tabsetPanel == "Loading Datasets") {
      tableOutput("data_table")
    }
  })
  
  output$data_table <- renderTable({
    datasetInput()
  })
}

shinyApp(ui, server)
