

# Application 3

library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Dataset Transformation Explorer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV or TXT File:",
                accept = c(".csv", ".txt")),
      uiOutput("var_select"),
      radioButtons("transformation", "Choose Transformation:",
                   choices = c("None", "Log-transformation", "Normalizing", "Standardizing")),
      actionButton("apply", "Apply Transformation")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset", tableOutput("dataTable")),
        tabPanel("Histogram", plotOutput("histPlot"))
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
    
    
    numeric_cols <- names(dataset())[sapply(dataset(), is.numeric)]
    
    
  
    
    selectInput("variables", "Select Variables (1-3):", 
                choices = numeric_cols, 
                multiple = TRUE, 
                selected = numeric_cols[1:min(3, length(numeric_cols))])  
  })
  
  
  
  
  output$dataTable <- renderTable({
    req(dataset())
    dataset()
  })
  
  transformed_data <- eventReactive(input$apply, {
    req(input$variables, dataset())
    
   
    data <- dataset()[, input$variables, drop = FALSE]
    
    if (!all(sapply(data, is.numeric))) {
      return(NULL) 
    }
    
    transformed <- data  
    
    if (input$transformation == "Log-transformation") {
      transformed <- log(data + 1)
    } else if (input$transformation == "Normalizing") {
      transformed <- as.data.frame(lapply(data, function(x) (x - min(x)) / (max(x) - min(x))))
    } else if (input$transformation == "Standardizing") {
      transformed <- as.data.frame(scale(data))
    }
    
    
    data.frame(Variable = rep(input$variables, each = nrow(data)), 
               Original = as.vector(as.matrix(data)), 
               Transformed = as.vector(as.matrix(transformed)))
  })
  
  
  
  
  
  output$histPlot <- renderPlot({
    req(transformed_data())
    
    data_df <- transformed_data()
    vars <- unique(data_df$Variable)
    num_vars <- length(vars)
    
    
    par(mfrow = c(num_vars, 2))  
    
    for (var in vars) {
      var_data <- subset(data_df, Variable == var)
      
      hist(var_data$Original, main = paste("Original:", var), col = "blue", border = "white")
      hist(var_data$Transformed, main = paste("Transformed:", var), col = "red", border = "white")
    }
  })
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
