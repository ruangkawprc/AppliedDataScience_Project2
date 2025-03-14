library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(scales)

# Define UI
ui <- fluidPage(
  titlePanel("Data Cleaning in R Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Data File", accept = c(".csv")),
      selectInput("missing_method", "Choose Missing Value Handling Method", 
                  choices = c("Remove", "Mean Imputation", "Median Imputation", "Mode Imputation")),
      selectInput("duplicate_method", "Handle Duplicates", 
                  choices = c("Keep", "Remove")),
      selectInput("scaling_method", "Choose Scaling Method", 
                  choices = c("None", "Min-Max", "Z-score")),
      selectInput("encoding_method", "Choose Encoding Method", 
                  choices = c("None", "One-Hot Encoding", "Label Encoding")),
      selectInput("outlier_method", "Handle Outliers", 
                  choices = c("None", "Remove", "Cap")),
      actionButton("apply_changes", "Apply Changes")
    ),
    
    mainPanel(
      DTOutput("data_table")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive value to store uploaded dataset
  data <- reactiveVal(NULL)
  
  # Load Data
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    data(df)
  })
  
  # Data Cleaning Process
  observeEvent(input$apply_changes, {
    req(data())
    df <- data()
    before_rows <- nrow(df)  # Save initial row count before changes
    
    # Handle Missing Values
    if (input$missing_method == "Remove") {
      df <- df %>% drop_na()
    } else if (input$missing_method == "Mean Imputation") {
      df <- df %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
    } else if (input$missing_method == "Median Imputation") {
      df <- df %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
    } else if (input$missing_method == "Mode Imputation") {
      mode_function <- function(x) {
        ux <- unique(na.omit(x))
        ux[which.max(tabulate(match(x, ux)))]
      }
      df <- df %>% mutate(across(where(is.character), ~ifelse(is.na(.), mode_function(.), .)))
    }
    
    # Handle Duplicates
    if (input$duplicate_method == "Remove") {
      df <- df %>% distinct()
    }
    
    # Scaling Methods
    if (input$scaling_method == "Min-Max") {
      df <- df %>% mutate(across(where(is.numeric), ~ ifelse(max(.) == min(.), ., (.-min(.))/(max(.)-min(.)))))
    } else if (input$scaling_method == "Z-score") {
      df <- df %>% mutate(across(where(is.numeric), ~ ifelse(sd(.) == 0, ., scale(.))))
    }
    
    # Encoding Categorical Variables
    if (input$encoding_method == "One-Hot Encoding") {
      df <- df %>% mutate(across(where(is.character), ~ as.factor(.)))
      df <- model.matrix(~.-1, data = df) %>% as.data.frame()
    } else if (input$encoding_method == "Label Encoding") {
      df <- df %>% mutate(across(where(is.character), ~ as.numeric(as.factor(.))))
    }
    
    # Outlier Handling
    if (input$outlier_method != "None") {
      for (col in colnames(df)) {
        if (is.numeric(df[[col]])) {
          Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
          Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
          IQR <- Q3 - Q1
          lower_bound <- Q1 - 1.5 * IQR
          upper_bound <- Q3 + 1.5 * IQR
          
          if (input$outlier_method == "Remove") {
            df <- df %>% filter(df[[col]] >= lower_bound & df[[col]] <= upper_bound)
          } else if (input$outlier_method == "Cap") {
            df[[col]] <- ifelse(df[[col]] < lower_bound, lower_bound, df[[col]])
            df[[col]] <- ifelse(df[[col]] > upper_bound, upper_bound, df[[col]])
          }
        }
      }
    }
    
    after_rows <- nrow(df)  # Save row count after processing
    showNotification(paste("Rows before:", before_rows, "| Rows after:", after_rows))
    
    # Update Data
    data(df)
  })
  
  # Render Table
  output$data_table <- renderDT({
    req(data())
    datatable(data(), options = list(scrollX = TRUE, pageLength = 10))
  })
}

# Run the app
shinyApp(ui, server)
