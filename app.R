library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)
library(readr)

# UI
ui <- page_navbar(
  title = "Compare Unweighted Percentages",
  fillable = TRUE,
  
  nav_panel("Chart View",
            page_sidebar(
              sidebar = sidebar(
                fileInput("file", "Upload Tab-Delimited File",
                          accept = c(".dat")),
                
                conditionalPanel(
                  condition = "output.fileUploaded",
                  
                  h4("Display Options"),
                  selectInput("sortBy", 
                              "Sort samples by category?",
                              choices = c("No (sort by sample)" = "none"),
                              selected = "none"),
                  
                  checkboxInput("excludeNIU", "Exclude NIU"),
                  checkboxInput("excludeUnknown", "Exclude Unknown"),
                  
                  # br(),
                  
                  h4("Select Samples to Compare"),
                  actionButton("selectAll", "Select All", class = "btn-sm"),
                  actionButton("clearAll", "Clear All", class = "btn-sm"),
                  selectInput("datasets", 
                              label = NULL,
                              choices = NULL,
                              multiple = TRUE,
                              selectize = TRUE)
                  
                  # br(),
                  
                )
              ),
              
              layout_columns(
                card(
                  #card_header("Plot Unweighted Percentages"),
                  div(
                    style = "overflow-x: auto; max-width: 100%;",
                    uiOutput("plotContainer")
                  )
                )
              )
            )
  ),
  
  nav_panel("Table View",
            fillable = TRUE,
            conditionalPanel(
              condition = "output.fileUploaded",
              #card(
              #  fill = TRUE,
              #  card_header("Data Summary Table"),
                DT::dataTableOutput("dataSummary", height = "100%")
              #)
            ),
            conditionalPanel(
              condition = "!output.fileUploaded",
              div(
                style = "text-align: center; margin-top: 100px;",
                h4("Please upload a file first to view the data summary.")
              )
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
      
      # Update sort by choices with category labels
      category_labels <- df[[2]][!is.na(df[[2]]) & df[[2]] != ""]
      sort_choices <- c("No (sort by sample)" = "none")
      sort_choices <- c(sort_choices, setNames(category_labels, category_labels))
      updateSelectInput(session, "sortBy", choices = sort_choices)
      
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
  
  # Grab variable label
  variable_label <- reactive({
    req(data())
    df <- data()
    return(names(df)[[2]])
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
    
    # Convert to long format for plotly
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
  
  # Filter out NIU or Unknown if necessary
  filtered_plot_data <- reactive({
    req(plot_data())
    
    plot_df <- plot_data()
    
    if (!input$excludeNIU & !input$excludeUnknown) {
      return(plot_df)
    }
    
    niu_labels <- c(
      "NIU (not in universe)",
      "NIU (not in universe)*"
    )
    
    if (input$excludeNIU) {
      plot_df <- plot_df |> 
        filter(!response_label %in% niu_labels) |> 
        mutate(
          percentage = round(100 * percentage / sum(percentage), 1),
          .by = dataset
        )
    }
    
    unknown_labels <- c(
      "Unknown",
      "Unknown*",
      "Unknown/missing",
      "Unknown/missing*"
    )
    
    if (input$excludeUnknown) {
      plot_df <- plot_df |> 
        filter(!response_label %in% unknown_labels) |> 
        mutate(
          percentage = round(100 * percentage / sum(percentage), 1),
          .by = dataset
        )
    }
    
    return(plot_df)
  })
  
  # Prepare sorted data for plotting
  filtered_and_sorted_plot_data <- reactive({
    req(filtered_plot_data())
    
    plot_df <- filtered_plot_data()
    
    if (input$sortBy == "none" || is.null(input$sortBy)) {
      return(plot_df)
    }
    
    # Calculate the percentage for the selected category for each dataset
    sort_values <- plot_df %>%
      filter(response_label == input$sortBy) %>%
      select(dataset, percentage) %>%
      arrange(desc(percentage))
    
    if (nrow(sort_values) == 0) {
      return(plot_df)
    }
    
    # Create ordered factor for datasets based on sorting
    plot_df$dataset <- factor(plot_df$dataset, levels = sort_values$dataset)
    
    return(plot_df)
  })
  
  
  # Calculate dynamic plot width based on number of datasets
  plot_width <- reactive({
    req(input$datasets)
    n_datasets <- length(input$datasets)
    
    # Set minimum bar width equivalent (50px per bar minimum)
    min_width_per_bar <- 50
    total_min_width <- n_datasets * min_width_per_bar
    
    # Set minimum plot width (400px) and use dynamic width if more datasets
    plot_width_px <- max(400, total_min_width)
    
    return(plot_width_px)
  })
  
  # Create plot container with dynamic width
  output$plotContainer <- renderUI({
    req(plot_width())
    
    plotlyOutput("comparisonPlot", 
                 height = "600px", 
                 width = paste0(plot_width(), "px"))
  })
  
  # Create the comparison plot using plotly
  output$comparisonPlot <- renderPlotly({
    req(filtered_and_sorted_plot_data())
    
    plot_df <- filtered_and_sorted_plot_data()
    
    if (nrow(plot_df) == 0) {
      return(plot_ly() %>%
               add_annotations(text = "No data to display",
                               x = 0.5, y = 0.5,
                               showarrow = FALSE))
    }
    
    # Create stacked bar chart
    p <- plot_ly(data = plot_df,
                 x = ~dataset,
                 y = ~percentage,
                 color = ~response_label,
                 customdata = ~response_label,
                 type = "bar",
                 # Simplified hover text showing category and percentage
                 hovertemplate = paste0(
                   "<b>%{customdata}</b><br>",
                   "%{y:.1f}%<br>",
                   "<extra></extra>"
                 )) %>%
      layout(
        title = list(text = variable_label(),
                     x = 0,
                     xanchor = "left",
                     font = list(size = 16)),
        xaxis = list(title = "Dataset",
                     tickangle = 45),
        yaxis = list(title = "Percentage",
                     ticksuffix = "%"),
        barmode = "stack",
        # Remove legend since hover provides the info
        showlegend = FALSE,
        # Adjust bar width
        bargap = 0.2,  # This controls space between groups of bars
        margin = list(t = 60, b = 100, l = 60, r = 20)
      ) %>%
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", 
                                        "zoomIn2d", "zoomOut2d", "autoScale2d"))
    
    return(p)
  })
  
  # Create data summary table
  output$dataSummary <- DT::renderDataTable({
    req(plot_data())
    
    plot_df <- plot_data()
    
    # Create summary table
    summary_df <- plot_df %>%
      pivot_wider(names_from = dataset, values_from = percentage) %>%
      select(response_code, response_label, everything())
    
    DT::datatable(summary_df,
                  extensions = "FixedColumns",
                  options = list(
                    pageLength = 15,
                    scrollX = TRUE,
                    fixedColumns = list(leftColumns = 2),
                    scrollY = "100%",
                    scrollCollapse = TRUE,
                    fixedHeader = TRUE,
                    dom = 'ftip'
                  ),
                  rownames = FALSE) %>%
      DT::formatRound(columns = 3:ncol(summary_df), digits = 1)
  })
}

# Run the app
shinyApp(ui = ui, server = server)