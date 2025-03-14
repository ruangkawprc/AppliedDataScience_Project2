library(shiny)
library(shinythemes)
library(readr)
library(readxl)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(zoo)
library(caret)
library(purrr)
library(DT)
library(fastDummies)

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$style(HTML(paste0(
      "body { background-color: #fafafa; color: #333; font-family: 'Roboto', sans-serif; }",
      ".navbar-default { background-color: #3498db; border-color: #2980b9; }",
      ".navbar-default .navbar-brand { color: #ffffff; font-size: 26px; font-weight: 600; }",
      ".navbar-default .navbar-nav > li > a { color: #ffffff; font-size: 16px; }",
      ".navbar-default .navbar-nav > li > a:hover { color: #f39c12; }",
      "#title { text-align: center; color: #ffffff; background-color: #3498db; font-size: 36px; font-weight: 700; padding: 30px 0; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); margin-bottom: 30px; position: relative; }",
      "#group-members { position: absolute; bottom: 8px; right: 20px; color: #ffffff; font-size: 14px; font-weight: 400; }",
      ".well { background-color: #ffffff; border: none; border-radius: 8px; padding: 20px; box-shadow: 0 4px 10px rgba(0,0,0,0.05); margin-bottom: 20px; }",
      ".well .form-group { margin-bottom: 20px; }",
      ".btn-primary { background-color: #3498db; border-color: #2980b9; border-radius: 6px; font-weight: bold; padding: 12px 20px; text-transform: uppercase; transition: background-color 0.3s ease; }",
      ".btn-primary:hover { background-color: #2980b9; }",
      ".nav-tabs { justify-content: center; margin-bottom: 30px; }",
      ".nav-tabs .nav-link { font-weight: 600; font-size: 16px; padding: 15px 30px; border-radius: 20px; margin: 0 10px; background-color: transparent; transition: background-color 0.3s ease; }",
      ".nav-tabs .nav-link.active { background-color: #3498db; color: white; border-radius: 20px; font-weight: 700; }",
      ".nav-tabs .nav-link:hover { background-color: #f39c12; color: white; }",
      ".tab-pane { padding: 30px; }",
      ".step-arrow { font-size: 20px; color: #3498db; font-weight: bold; margin-left: 10px; }",
      "table { width: 100%; border-collapse: collapse; margin-top: 20px; border-radius: 8px; overflow: hidden; }",
      "th, td { padding: 12px; text-align: center; border: 1px solid #ddd; font-size: 14px; }",
      "tr:nth-child(even) { background-color: #f9f9f9; }",
      "th { background-color: #3498db; color: white; font-weight: bold; font-size: 16px; }",
      "td { font-size: 14px; }",
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
    tabsetPanel(
      tabPanel("Loading Datasets",
               fluidRow(
                 column(6, 
                        div(class = "well",
                            fileInput("file", "Upload Dataset (.csv, .xlsx, .json, .rds)", accept = c(".csv", ".xlsx", ".json", ".rds")),
                            textInput("file_path", "Or enter file path:", value = ""),
                            selectInput("dataset", "Or choose a dataset:",
                                        choices = c("Congress 118 Bills" = "congress_118_bills", 
                                                    "Korean Drama" = "korean_drama")), 
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
               ),
               uiOutput("conditional_table")
      ),
      tabPanel("Data Cleaning",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("missing_method", "Choose Missing Value Handling Method", 
                               choices = c("Remove" = "remove", 
                                           "Mean Imputation" = "mean", 
                                           "Median Imputation" = "median", 
                                           "Mode Imputation" = "mode",
                                           "Forward Fill" = "ffill",
                                           "Backward Fill" = "bfill"))
                 ),
                 mainPanel(
                   verbatimTextOutput("summary"),
                   tableOutput("data_head")
                 )
               )
      ),
      tabPanel("Data Transformation",
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("var_select"),
                   radioButtons("transformation", "Choose Transformation:",
                                choices = c("None", "Log-transformation", "Normalizing", "Standardizing", "One-Hot Encoding")),
                   uiOutput("one_hot_var_select"),  # Dropdown for one-hot encoding
                   actionButton("apply", "Apply Transformation")
                 ),
                 mainPanel(
                   tabsetPanel(
                     
                     tabPanel("Histogram", plotOutput("histPlotTansformation")),
                     tabPanel("One-Hot Encoded Data", DT::dataTableOutput("encodedDataOutput"))  # New tab for one-hot encoded data
                   )
                 )
               )
      ),
      tabPanel("Exploratory Data Analysis (EDA)",
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("var_select_histogram"),
                   uiOutput("var_select_boxplot"),
                   checkboxInput("top15_corr", "Show Top 15 in Correlation Heatmap", value = FALSE),
                   hr(),
                   h4("Select Columns to Export"),
                   uiOutput("var_select_export"),
                   downloadButton("download_filtered", "Download Selected Columns")
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Summary Statistics", 
                              verbatimTextOutput("summaryOutput"), 
                              tableOutput("missingValues")
                     ),
                     tabPanel("Histogram", plotOutput("histPlot")),
                     tabPanel("Boxplot", plotOutput("boxPlot")),
                     tabPanel("Correlation Matrix", tableOutput("correlationMatrix")),
                     tabPanel("Correlation Heatmap", plotOutput("correlationHeatmap"))
                   )
                 )
               )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store the loaded dataset
  loaded_data <- reactiveVal(NULL)
  
  # Load Data in the first tab
  observeEvent(input$load_data, {
    if (!is.null(input$file)) {
      # Load from uploaded file
      ext <- tools::file_ext(input$file$name)
      dataset <- switch(ext,
                        "csv" = read_csv(input$file$datapath),
                        "xlsx" = {
                          df <- read_excel(input$file$datapath)
                          write.csv(df, "temp_data.csv", row.names = FALSE)
                          read_csv("temp_data.csv")
                        },
                        "json" = {
                          df <- fromJSON(input$file$datapath, flatten = TRUE)
                          if (is.list(df)) {
                            if (length(df) == 1) {
                              df <- df[[1]]  
                            }
                            df <- as.data.frame(df) 
                          }
                          df  
                        },
                        "rds" = {
                          df <- readRDS(input$file$datapath)
                          write.csv(df, "temp_data.csv", row.names = FALSE)
                          read_csv("temp_data.csv")
                        },
                        stop("Unsupported file type")
      )
      loaded_data(dataset)
    } else if (input$file_path != "") {
      # Load from file path
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
                            return(df)  
                          } else {
                            if (is.list(df)) {
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
      loaded_data(dataset)
    } else {
      # Load from predefined dataset
      dataset_paths <- list(
        congress_118_bills = "sample_data/congress_118_bills.csv",
        korean_drama = "sample_data/korean_drama-2.csv"
      )
      path <- dataset_paths[[input$dataset]]
      if (!is.null(path)) {
        dataset <- read_csv(path)
        loaded_data(dataset)
      }
    }
  })
  
  # Display loaded data in the first tab
  output$conditional_table <- renderUI({
    if (!is.null(loaded_data())) {
      tableOutput("data_table")
    }
  })
  
  output$data_table <- renderTable({
    loaded_data()
  })
  
  # Data Cleaning Tab
  output$data_head <- renderTable({
    req(loaded_data())
    head(loaded_data())
  })
  
  # Data Transformation Tab
  output$var_select <- renderUI({
    req(loaded_data())
    numeric_cols <- names(loaded_data())[sapply(loaded_data(), is.numeric)]
    selectInput("variables", "Select Variables (1-3):", 
                choices = numeric_cols, 
                multiple = TRUE, 
                selected = numeric_cols[1:min(3, length(numeric_cols))])  
  })
  
  # Dropdown for selecting categorical variables for one-hot encoding
  output$one_hot_var_select <- renderUI({
    req(loaded_data())
    categorical_cols <- names(loaded_data())[sapply(loaded_data(), is.character)]
    selectInput("one_hot_var", "Select Categorical Variable for One-Hot Encoding:", choices = categorical_cols)
  })
  
  # Reactive to apply transformations
  transformed_data <- eventReactive(input$apply, {
    req(input$variables, loaded_data())
    
    data <- loaded_data()[, input$variables, drop = FALSE]
    
    # Ensure data is numeric
    if (!all(sapply(data, is.numeric))) {
      return(NULL)
    }
    
    # Apply the chosen transformation
    transformed <- data  
    if (input$transformation == "Log-transformation") {
      transformed <- log(data + 1)
    } else if (input$transformation == "Normalizing") {
      transformed <- as.data.frame(lapply(data, function(x) (x - min(x)) / (max(x) - min(x))))
    } else if (input$transformation == "Standardizing") {
      transformed <- as.data.frame(scale(data))
    } else if (input$transformation == "One-Hot Encoding") {
      req(input$one_hot_var)
      transformed <- dummy_cols(loaded_data(), select_columns = input$one_hot_var, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
    }
    
    # Combine original and transformed data for visualization
    data.frame(Variable = rep(input$variables, each = nrow(data)), 
               Original = as.vector(as.matrix(data)), 
               Transformed = as.vector(as.matrix(transformed)))
  })
  
  # Plot histograms for original and transformed data using ggplot2
  output$histPlotTansformation <- renderPlot({
    req(transformed_data())
    
    data_df <- transformed_data()
    vars <- unique(data_df$Variable)
    
    # Use ggplot2 for histograms
    ggplot(data_df, aes(x = Transformed)) +
      geom_histogram(fill = "blue", color = "black", alpha = 0.7, bins = 30) +
      facet_wrap(~ Variable, scales = "free") +
      labs(title = "Transformed Data Histogram", x = "Value", y = "Count") +
      theme_minimal()
  })
  
  # Display one-hot encoded data
  output$encodedDataOutput <- DT::renderDataTable({
    req(input$transformation == "One-Hot Encoding")
    transformed_data()
  })
  
  # EDA Tab
  numeric_cols_eda <- reactive({
    req(loaded_data())  
    cols <- names(loaded_data())[sapply(loaded_data(), is.numeric)]
    if (length(cols) == 0) {
      return(NULL)
    }
    return(cols)
  })
  
  all_cols_eda <- reactive({
    req(loaded_data())
    names(loaded_data())
  })
  
  output$summaryOutput <- renderPrint({
    req(loaded_data())
    summary(loaded_data())
  })
  
  output$missingValues <- renderTable({
    req(loaded_data())
    missing <- sapply(loaded_data(), function(x) sum(is.na(x)))
    data.frame(Variable = names(missing), Missing_Values = missing)
  })
  
  output$var_select_histogram <- renderUI({
    req(all_cols_eda())
    selectInput("hist_variable", "Select a Variable (Histogram):", choices = all_cols_eda())
  })
  
  output$var_select_boxplot <- renderUI({
    req(numeric_cols_eda())
    if (is.null(numeric_cols_eda())) {
      return("No numeric variables found in dataset.")
    }
    selectInput("boxplot_variable", "Select a Variable (Boxplot):", choices = numeric_cols_eda())
  })
  
  # Use ggplot2 for histograms in EDA
  output$histPlot <- renderPlot({
    req(input$hist_variable, loaded_data())
    data <- loaded_data()
    if (is.numeric(data[[input$hist_variable]])) {
      ggplot(data, aes(x = .data[[input$hist_variable]])) +
        geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
        labs(title = paste("Histogram of", input$hist_variable), x = input$hist_variable, y = "Count") +
        theme_minimal()
    } else {
      ggplot(data, aes(x = .data[[input$hist_variable]])) +
        geom_bar(fill = "blue", color = "black", alpha = 0.7) +
        labs(title = paste("Count of", input$hist_variable), x = input$hist_variable, y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    }
  })
  
  # Use ggplot2 for boxplots in EDA
  output$boxPlot <- renderPlot({
    req(input$boxplot_variable, loaded_data())
    ggplot(loaded_data(), aes(y = .data[[input$boxplot_variable]])) +
      geom_boxplot(fill = "red", alpha = 0.5) +
      labs(title = paste("Boxplot of", input$boxplot_variable), y = input$boxplot_variable) +
      theme_minimal()
  })
  
  output$correlationMatrix <- renderTable({
    req(loaded_data())
    numeric_data <- loaded_data()[, sapply(loaded_data(), is.numeric)]
    if (ncol(numeric_data) < 2) {
      return(data.frame(Message = "Not enough numeric columns for correlation analysis."))
    }
    round(cor(numeric_data, use = "pairwise.complete.obs"), 2)  
  })
  
  # Use ggplot2 for correlation heatmap
  output$correlationHeatmap <- renderPlot({
    req(loaded_data())
    numeric_data <- loaded_data()[, sapply(loaded_data(), is.numeric)]
    numeric_data <- numeric_data[, !grepl("id|ID", names(numeric_data))]
    if (input$top15_corr && ncol(numeric_data) > 15) {
      cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
      max_corr <- apply(abs(cor_matrix), 2, function(x) max(x[x < 1], na.rm = TRUE))
      top_vars <- names(sort(max_corr, decreasing = TRUE))[1:15]
      numeric_data <- numeric_data[, top_vars]
    }
    cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
    melted_cor <- reshape2::melt(cor_matrix)
    ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab",
                           name="Correlation") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold")
      ) +
      labs(title = "Correlation Heatmap", x = "", y = "")
  })
  
  output$var_select_export <- renderUI({
    req(all_cols_eda())
    checkboxGroupInput("selected_columns", "Select Columns to Export:", choices = all_cols_eda(), selected = all_cols_eda())
  })
  
  output$download_filtered <- downloadHandler(
    filename = function() { "filtered_data.csv" },
    content = function(file) {
      req(input$selected_columns)
      write.csv(loaded_data()[, input$selected_columns, drop = FALSE], file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)