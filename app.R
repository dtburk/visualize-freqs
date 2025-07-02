library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(readr)

# UI
ui <- page_sidebar(
  title = "Dataset Comparison Tool",
  sidebar = sidebar(
    fileInput("file", "Upload Tab-Delimited File",
              accept = ".dat"),
    
    conditionalPanel(
      condition = "output.fileUploaded",
      
      h4("Select Datasets to Compare"),
      selectInput("datasets", 
                  label = NULL,
                  choices = NULL,
                  multiple = TRUE,
                  selectize = TRUE),
      
      br(),
      actionButton("selectAll", "Select All", class = "btn-sm"),
      actionButton("clearAll", "Clear All", class = "btn-sm"),
      
      br(), br(),
      h4("Display Options"),
      checkboxInput("showPercentages", "Show Percentages on Bars", value = TRUE),
      checkboxInput("showDataSummary", "Show Data Summary Table", value = FALSE)
    )
  ),
  
  layout_columns(
    card(
      card_header("Comparison Chart"),
      plotOutput("comparisonPlot", height = "600px")
    )
  ),
  
  conditionalPanel(
    condition = "input.showDataSummary",  
    card(
      card_header("Data Summary"),
      DT::dataTableOutput("dataSummary")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive value to store the uploaded data
  data <- reactiveVal(NULL)
  
  # File upload handler
  observeEvent(input$file, {
    req(input$file)
    
    tryCatch({
      # Read the tab-delimited file using readr::read_tsv
      df <- readr::read_tsv(
        input$file$datapath, 
        name_repair = "minimal",
        col_types = readr::cols(.default = readr::col_character())
      )
      
      # Validate file structure
      if (ncol(df) < 3) {
        showNotification("File must have at least 3 columns", type = "error")
        return()
      }
      
      # Extract dataset columns (those with "unweighted %" pattern)
      dataset_cols <- grep("^unweighted % ", names(df), value = TRUE)
      
      if (length(dataset_cols) == 0) {
        showNotification("No dataset columns found with pattern 'unweighted % [dataset_id]'", type = "error")
        return()
      }
      
      # Remove columns that are all blank or missing
      cols_to_keep <- c()
      
      # Always keep the first two columns (response code and label)
      cols_to_keep <- c(1, 2)
      
      # Check dataset columns for non-empty values
      for (i in seq_along(dataset_cols)) {
        col_name <- dataset_cols[i]
        col_index <- which(names(df) == col_name)
        col_values <- df[[col_name]]
        
        # Check if column has any non-missing, non-empty values
        has_data <- any(!is.na(col_values) & col_values != "" & col_values != " ")
        
        if (has_data) {
          cols_to_keep <- c(cols_to_keep, col_index)
        }
      }
      
      # Filter the dataframe to keep only non-empty columns
      df <- df[, cols_to_keep, drop = FALSE]
      
      # Update dataset_cols after filtering
      dataset_cols <- grep("^unweighted % ", names(df), value = TRUE)
      
      if (length(dataset_cols) == 0) {
        showNotification("No dataset columns contain data", type = "error")
        return()
      }
      
      # Store the filtered data
      data(df)
      
      # Extract dataset IDs from column names
      dataset_ids <- gsub("^unweighted % ", "", dataset_cols)
      
      # Update dataset choices using selectInput
      updateSelectInput(session, "datasets",
                        choices = setNames(dataset_ids, dataset_ids),
                        selected = dataset_ids[1:min(3, length(dataset_ids))])
      
      showNotification("File uploaded successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  
  # Output to control conditional panel
  output$fileUploaded <- reactive({
    !is.null(data())
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # Select/Clear all buttons
  observeEvent(input$selectAll, {
    req(data())
    dataset_cols <- grep("^unweighted % ", names(data()), value = TRUE)
    dataset_ids <- gsub("^unweighted % ", "", dataset_cols)
    updateSelectInput(session, "datasets", selected = dataset_ids)
  })
  
  observeEvent(input$clearAll, {
    updateSelectInput(session, "datasets", selected = character(0))
  })
  
  # Prepare data for plotting
  plot_data <- reactive({
    req(data(), input$datasets)
    
    df <- data()
    
    # Get the selected dataset columns
    selected_cols <- paste0("unweighted % ", input$datasets)
    
    # Select relevant columns: response codes, labels, and selected datasets
    plot_df <- df[, c(names(df)[1:2], selected_cols), drop = FALSE]
    
    # Rename columns for easier handling
    names(plot_df)[1:2] <- c("response_code", "response_label")
    
    # Convert to long format for ggplot
    plot_df_long <- plot_df %>%
      pivot_longer(cols = starts_with("unweighted %"),
                   names_to = "dataset",
                   values_to = "percentage") %>%
      mutate(dataset = gsub("^unweighted % ", "", dataset),
             percentage = as.numeric(percentage))
    
    # Remove rows with missing percentages
    plot_df_long <- plot_df_long[!is.na(plot_df_long$percentage), ]
    
    return(plot_df_long)
  })
  
  # Create the comparison plot
  output$comparisonPlot <- renderPlot({
    req(plot_data())
    
    plot_df <- plot_data()
    
    if (nrow(plot_df) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data to display") +
               theme_minimal())
    }
    
    # Create stacked bar chart with datasets on x-axis and response categories as fill
    p <- ggplot(plot_df, aes(x = dataset, y = percentage, fill = response_label)) +
      geom_col(position = "stack")
    
    # Add percentage labels if requested
    if (input$showPercentages) {
      p <- p + geom_text(aes(label = paste0(round(percentage, 1), "%")),
                         position = position_stack(vjust = 0.5),
                         size = 3, color = "white", fontface = "bold")
    }
    
    # Customize the plot
    p <- p +
      labs(title = "Dataset Comparison - Response Distribution",
           x = "Dataset",
           y = "Percentage",
           fill = "Response Category") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 16),
            legend.position = "bottom") +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      guides(fill = guide_legend(title.position = "top", ncol = 2))
    
    return(p)
  })
  
  # Create data summary table (only when requested)
  output$dataSummary <- DT::renderDataTable({
    req(plot_data(), input$showDataSummary)
    
    plot_df <- plot_data()
    
    # Create summary table
    summary_df <- plot_df %>%
      pivot_wider(names_from = dataset, values_from = percentage) %>%
      select(response_code, response_label, everything())
    
    DT::datatable(summary_df,
                  options = list(pageLength = 15,
                                 scrollX = TRUE,
                                 dom = 'ftip'),
                  rownames = FALSE) %>%
      DT::formatRound(columns = 3:ncol(summary_df), digits = 1)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
