library(shiny)
library(ggplot2)
library(jsonlite)  
library(dplyr)  
library(tidyr)  
library(reshape2)  

# Define UI
ui <- fluidPage(
  titlePanel("Exploratory Data Analysis (EDA)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV or JSON File:", accept = c(".csv", ".json")),
      uiOutput("var_select_histogram"),
      uiOutput("var_select_boxplot"),
      checkboxInput("top15_corr", "Show Top 15 in Correlation Heatmap", value = FALSE),
      hr(),  # 分隔线
      h4("Select Columns to Export"),
      uiOutput("var_select_export"),  # 选择导出列的多选框
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

# Define server logic
server <- function(input, output, session) {
  
  dataset <- reactive({
    req(input$file)  
    ext <- tools::file_ext(input$file$name)  
    
    if (ext == "csv") {
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else if (ext == "json") {
      json_data <- fromJSON(input$file$datapath, flatten = TRUE)
      if (is.data.frame(json_data)) {
        df <- json_data
      } else if (is.list(json_data)) {
        df <- as.data.frame(json_data)
      } else {
        stop("Unsupported JSON format")
      }
    }
    
    # Convert numeric columns
    for (col in colnames(df)) {
      if (all(grepl("^[0-9.]+$", df[[col]]))) {  
        df[[col]] <- as.numeric(df[[col]])
      }
    }
    
    return(df)
  })
  
  # Compute numeric columns
  numeric_cols <- reactive({
    req(dataset())  
    cols <- names(dataset())[sapply(dataset(), is.numeric)]
    if (length(cols) == 0) {
      return(NULL)
    }
    return(cols)
  })
  
  # Compute all columns
  all_cols <- reactive({
    req(dataset())
    names(dataset())
  })
  
  # Summary statistics
  output$summaryOutput <- renderPrint({
    req(dataset())
    summary(dataset())
  })
  
  # Missing values table
  output$missingValues <- renderTable({
    req(dataset())
    missing <- sapply(dataset(), function(x) sum(is.na(x)))
    data.frame(Variable = names(missing), Missing_Values = missing)
  })
  
  # Dynamic variable selection for histogram (supports numeric & categorical)
  output$var_select_histogram <- renderUI({
    req(all_cols())
    selectInput("hist_variable", "Select a Variable (Histogram):", choices = all_cols())
  })
  
  # Dynamic variable selection for boxplot (only numeric)
  output$var_select_boxplot <- renderUI({
    req(numeric_cols())
    if (is.null(numeric_cols())) {
      return("No numeric variables found in dataset.")
    }
    selectInput("boxplot_variable", "Select a Variable (Boxplot):", choices = numeric_cols())
  })
  
  # Histogram plot (supports numeric & categorical)
  output$histPlot <- renderPlot({
    req(input$hist_variable, dataset())
    data <- dataset()
    
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
  
  # Boxplot
  output$boxPlot <- renderPlot({
    req(input$boxplot_variable, dataset())
    ggplot(dataset(), aes(y = .data[[input$boxplot_variable]])) +
      geom_boxplot(fill = "red", alpha = 0.5) +
      labs(title = paste("Boxplot of", input$boxplot_variable), y = input$boxplot_variable) +
      theme_minimal()
  })
  
  # Correlation Matrix (Full Table)
  output$correlationMatrix <- renderTable({
    req(dataset())
    numeric_data <- dataset()[, sapply(dataset(), is.numeric)]
    
    if (ncol(numeric_data) < 2) {
      return(data.frame(Message = "Not enough numeric columns for correlation analysis."))
    }
    
    round(cor(numeric_data, use = "pairwise.complete.obs"), 2)  
  })
  
  # Correlation Heatmap (Supports Top 15)
  output$correlationHeatmap <- renderPlot({
    req(dataset())
    
    # Select numeric variables
    numeric_data <- dataset()[, sapply(dataset(), is.numeric)]
    
    # Remove ID-like columns
    numeric_data <- numeric_data[, !grepl("id|ID", names(numeric_data))]
    
    # If "Show Top 15" is selected, filter highest correlated variables
    if (input$top15_corr && ncol(numeric_data) > 15) {
        cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
        
        # Get top 15 correlated variables
        max_corr <- apply(abs(cor_matrix), 2, function(x) max(x[x < 1], na.rm = TRUE))
        top_vars <- names(sort(max_corr, decreasing = TRUE))[1:15]
        numeric_data <- numeric_data[, top_vars]
    }
    
    # Compute correlation matrix
    cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
    
    # Transform data for ggplot
    melted_cor <- reshape2::melt(cor_matrix)
    
    # Plot heatmap
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
  
  # Dynamic column selection for exporting
  output$var_select_export <- renderUI({
    req(all_cols())
    checkboxGroupInput("selected_columns", "Select Columns to Export:", choices = all_cols(), selected = all_cols())
  })
  
  # Download selected columns as CSV
  output$download_filtered <- downloadHandler(
    filename = function() { "filtered_data.csv" },
    content = function(file) {
      req(input$selected_columns)
      write.csv(dataset()[, input$selected_columns, drop = FALSE], file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
