library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Sales Data Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV File (input.csv)",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      helpText("Expected columns: Category, Quantity, Price. If no file is uploaded, a built-in sample dataset will be used automatically."),
      hr(),
      h4("Summary Statistics"),
      verbatimTextOutput("summaryStats")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Updated Data & Calculations",
                 h3("Data Frame with Total Sales & Rounded Sales"),
                 DTOutput("updatedTable")
        ),
        tabPanel("Category Frequency",
                 h3("Category Frequency Table"),
                 verbatimTextOutput("catFreq")
        ),
        tabPanel("Row Removal View",
                 h3("Data Frame After Removing Second Row (data[-2,])"),
                 DTOutput("removedRowTable")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Default sample data mimicking input.csv structure
  sampleData <- data.frame(
    Item = c("Apples", "Notebook", "Pen", "Banana", "Folder"),
    Category = c("Fruit", "Stationery", "Stationery", "Fruit", "Stationery"),
    Quantity = c(10, 5, 20, 15, 8),
    Price = c(2.50, 3.00, 1.20, 1.50, 4.00),
    stringsAsFactors = FALSE
  )
  
  # Read uploaded file or fallback to sample data
  rawData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(sampleData)
    }
    
    df <- tryCatch({
      read.csv(inFile$datapath, stringsAsFactors = FALSE)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(df)) {
      showNotification("Error reading CSV file. Using sample data instead.", type = "error")
      return(sampleData)
    }
    
    df
  })
  
  # Processed data with calculated columns (TotalSales & RoundedSales)
  processedData <- reactive({
    df <- rawData()
    if (all(c("Quantity", "Price") %in% colnames(df))) {
      df$TotalSales <- df$Quantity * df$Price
      df$RoundedSales <- round(df$TotalSales, 0)
    }
    df
  })
  
  # Category Frequency Table
  output$catFreq <- renderPrint({
    df <- processedData()
    if ("Category" %in% colnames(df)) {
      print(table(df$Category))
    } else {
      cat("Error: 'Category' column not found in the dataset.")
    }
  })
  
  # Summary Statistics (Highest and Average Sale)
  output$summaryStats <- renderPrint({
    df <- processedData()
    if ("TotalSales" %in% colnames(df) && nrow(df) > 0) {
      max_sale <- max(df$TotalSales, na.rm = TRUE)
      avg_sale <- round(mean(df$TotalSales, na.rm = TRUE), 2)
      cat(paste0("Highest Sale: ", max_sale, "\nAverage Sale: ", avg_sale))
    } else {
      cat("Sales data unavailable.")
    }
  })
  
  # Main Updated Table
  output$updatedTable <- renderDT({
    datatable(processedData(), options = list(pageLength = 5), rownames = TRUE)
  })
  
  # Table with second row removed (data[-2,])
  output$removedRowTable <- renderDT({
    df <- processedData()
    if (nrow(df) >= 2) {
      df_modified <- df[-2, ]
    } else {
      df_modified <- df
    }
    datatable(df_modified, options = list(pageLength = 5), rownames = TRUE)
  })
}

shinyApp(ui = ui, server = server)