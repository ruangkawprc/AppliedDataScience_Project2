library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(jsonlite)
library(zoo)
library(caret)
library(tidyr)
library(purrr)


# Function to Load and Flatten Data

load_data <- function(file) {
  ext <- tools::file_ext(file$datapath)
  
  if (ext == "csv") {
    df <- read.csv(file$datapath, stringsAsFactors = FALSE)
    
  } else if (ext %in% c("xls", "xlsx")) {
    df <- read_excel(file$datapath)
    
  } else if (ext == "rds") {
    df <- readRDS(file$datapath)
    
  } else if (ext == "json") {
    raw_json <- fromJSON(file$datapath)
    
    # Convert JSON list to tibble
    df <- tibble::tibble(report_data = raw_json) %>%
      unnest_wider(report_data)
    
    # Convert Dates
    df <- df %>%
      mutate(
        date_created = as.Date(as.character(date_created), format="%Y%m%d"),
        date_started = as.Date(as.character(date_started), format="%Y%m%d")
      )
    
    # Flatten 'consumer' Column
    if ("consumer" %in% colnames(df)) {
      df <- df %>% unnest_wider(consumer, names_sep = "_")
    }
    
    # Extract 'role' and 'name_brand' from 'products'
    if ("products" %in% colnames(df)) {
      df <- df %>%
        mutate(
          product_role = map_chr(products, ~ if (is.list(.x) && length(.x) > 0) .x[[1]]$role else NA_character_),
          product_name_brand = map_chr(products, ~ if (is.list(.x) && length(.x) > 0 && !is.null(.x[[1]]$name_brand)) tolower(.x[[1]]$name_brand) else NA_character_)
        ) %>%
        select(-products)  # Drop 'products' column after extracting data
    }
    
    # Drop Unnecessary Columns
    df <- df %>% select(-c(report_number, age_unit, product_role), everything())
    
    # Explode 'outcomes' and 'reactions'
    df <- df %>%
      mutate(outcomes = map(outcomes, ~ if (is.list(.x)) .x else list(.x))) %>%
      unnest(outcomes, keep_empty = TRUE) %>%
      mutate(outcomes = tolower(outcomes))
    
    df <- df %>%
      mutate(reactions = map(reactions, ~ if (is.list(.x)) .x else list(.x))) %>%
      unnest(reactions, keep_empty = TRUE) %>%
      mutate(reactions = tolower(reactions))
    
    # One-Hot Encoding (Convert Categorical Variables to Dummy Variables)
    df <- df %>%
      mutate(across(c(outcomes, reactions), as.factor)) %>%
      pivot_wider(names_from = outcomes, values_from = outcomes, values_fn = length, names_prefix = "outcome_") %>%
      pivot_wider(names_from = reactions, values_from = reactions, values_fn = length, names_prefix = "reaction_")
    
  } else {
    stop("Unsupported file format. Please upload CSV, Excel, JSON, or RDS.")
  }
  
  # Ensure valid column names
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  
  return(df)
}


#Data Cleaning Function (Standardization & Formatting)

clean_data <- function(df) {
  df <- df %>%
    mutate(across(where(is.character), ~ trimws(tolower(.)))) %>%
    distinct() %>%
    mutate(across(where(is.numeric), as.numeric))
  
  return(df)
}


# Data Transformation Function (Scaling, Encoding, Handling Missing & Outliers)
transform_data <- function(df) {
  df_cleaned <- df
  
  # Handle Missing Values
  df_cleaned <- df_cleaned %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    mutate(across(where(is.character), ~ ifelse(is.na(.), "unknown", .)))  
  
  # Normalize Numerical Features
  numeric_cols <- names(df_cleaned)[sapply(df_cleaned, is.numeric)]
  if (length(numeric_cols) > 0) {
    df_cleaned[numeric_cols] <- as.data.frame(scale(df_cleaned[numeric_cols]))
  }
  
  # Handle Outliers using IQR Method
  handle_outliers <- function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    x[x < (Q1 - 1.5 * IQR)] <- Q1 - 1.5 * IQR
    x[x > (Q3 + 1.5 * IQR)] <- Q3 + 1.5 * IQR
    return(x)
  }
  if (length(numeric_cols) > 0) {
    df_cleaned[numeric_cols] <- lapply(df_cleaned[numeric_cols], handle_outliers)
  }
  
  return(df_cleaned)
}


# Shiny UI (User Interface)

ui <- fluidPage(
  titlePanel("Data Cleaning in R Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Data File", accept = c(".csv", ".xls", ".xlsx", ".json", ".rds")),
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
)


# Shiny Server Logic
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)  
    
    withProgress(message = "Processing Data", value = 0, {
      incProgress(0.2, detail = "Loading dataset...")  
      df <- load_data(input$file)
      
      incProgress(0.3, detail = "Cleaning data...")
      df <- clean_data(df)
      
      incProgress(0.3, detail = "Transforming data...")
      df <- transform_data(df)
      
      incProgress(0.2, detail = "Finalizing...")
      
      if (length(names(df)) < 100) {
        updateSelectInput(session, "col", choices = names(df))
      }
      
      return(df)
    })
  })
  
  output$data_head <- renderTable({
    req(data())
    head(data())
  })
}

shinyApp(ui = ui, server = server)
