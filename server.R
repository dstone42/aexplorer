library(shiny)
library(future)
library(promises)
library(data.table)
library(ggplot2)
library(jsonlite)

# Source visualization modules
source("modules/onset_plot.R")
source("modules/sankey_plot.R")
source("modules/volcano_plot.R")
source("modules/chord_diagram.R")

# ---- CACHE DATA HERE ----
cached_data <- fread("data/data.csv", sep = "$")
drug_class_stats <- fread("data/drug_class_stats.csv")
cancer_type_stats <- fread("data/cancer_type_stats.csv")
# chord_table <- makeFreqTable(cached_data, "AE_Category_Expanded")  # Create frequency table for Chord plot
chord_table <- fread("data/AE_Category_freq_table.csv")
row_names <- chord_table[[1]]
chord_table[[1]] <- NULL
chord_table <- as.matrix(chord_table)
rownames(chord_table) <- row_names
# -------------------------

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive value to store the loaded data
  data <- reactiveVal(cached_data)
  drug_class_stats_data <- reactiveVal(drug_class_stats)
  cancer_type_stats_data <- reactiveVal(cancer_type_stats)
  chord_data <- reactiveVal(chord_table)

  # Read palettes
  ae_category_palette <- fromJSON("www/palettes/ae_categories.json")
  outcomes_palette <- fromJSON("www/palettes/outcomes.json")
  drug_category_palette <- fromJSON("www/palettes/drug_categories.json")

  # Ensure palettes are available globally
  assign("ae_category_palette", ae_category_palette, envir = .GlobalEnv)
  assign("outcomes_palette", outcomes_palette, envir = .GlobalEnv)
  assign("drug_category_palette", drug_category_palette, envir = .GlobalEnv)

  # Initialize tooltips
  addTooltip(session, id = "infoIcon", title = "For the onset plot, values greater than 52 weeks are grouped into '>52'.", placement = "left", trigger = "hover")

  # The selected order I chose in the UI section was never preserved when the figure rendered, so I am setting the selected here
  updateSelectizeInput(session, "sankeyColumns", selected = c("drug_category", "outc_cod"))  # Default selected columns for Sankey plot

  # Observe the subsetData input and update the filterColumn choices accordingly
  observe({
    choices <- names(data())  # Get column names for the filterColumn dropdown
    # Remove primaryid, event_dt, and time_to_onset from choices
    choices <- choices[!choices %in% c("primaryid", "event_dt", "time_to_onset")]
    updateSelectInput(session, "filterColumn", choices = choices)  # Populate filterColumn dropdown
    
  })

  # Observe the filterColumn input and update the filterValues choices accordingly
  observe({
    updateSelectizeInput(session, "filterValues", choices = unique(data()[[input$filterColumn]]), server = TRUE)  # Populate filterValues dropdown
  })

  # Filter data based on user input
  filteredData <- reactive({
    req(data())
    if (input$subsetData == "Yes" && !is.null(input$filterColumn) && !is.null(input$filterValues)) {
      data()[data()[[input$filterColumn]] %in% input$filterValues, ]
    } else {
      data()
    }
  })
  
  # Dynamically calculate plot height based on the number of facets
  plotHeight <- reactive({
    req(filteredData())
    if (input$facetVarRow != "None") {
      num_facets <- length(unique(filteredData()[[input$facetVarRow]]))
      min_height <- 300  # Minimum height for the plot
      facet_height <- 50  # Height per facet
      max_height <- 1200  # Maximum height for the plot
      
      # Calculate total height
      total_height <- min(max(min_height, num_facets * facet_height), max_height)
      return(total_height)
    } else {
      num_facets <- 1  # Default to 1 if no faceting
      return(200)  # Default height for non-faceted plots
    }
  })

  # Make plot container scrollable for all plots except Chord
  output$plot_container <- renderUI({
    if (input$plotType == "Chord") {
      withSpinner(uiOutput("plot"))  # No scrollable div
    } else {
      div(
        style = "overflow-y: auto; max-height: 1200px;",
        withSpinner(uiOutput("plot"))
      )
    }
  })

  # Add plot output based on selected plot type
  output$plot <- renderUI({
    req(filteredData())  # Ensure data is loaded before rendering

    if (input$plotType == "Onset") {
      # Onset Plot
      tagList(
        plotOutput("onsetPlot", height = plotHeight()),  # Use plotOutput for ggplot
        # Caption
        tags$p(
          style = "font-style: italic; font-size: 12px; color: #555; margin-top: 8px;",
          "Time to onset (in weeks) from drug administration to adverse events. Values greater than 52 weeks are grouped into '>52'."
        )
      )
    } else if (input$plotType == "Sankey") {
      # Sankey Plot
      tagList(
        sankeyNetworkOutput("sankeyPlot"),  # Use sankeyNetworkOutput for Sankey plot
        # Caption
        tags$p(
          style = "font-style: italic; font-size: 12px; color: #555; margin-top: 8px;",
          "Proportions of adverse event cases across selected columns."
        )
      )
    } else if (input$plotType == "Volcano") {
      # Volcano Plot

      # Dynamically select the data source for the Volcano plot based on input$volcanoTarget
      req(input$volcanoTarget)
      if (input$volcanoTarget == "drug_class") {
        data_source <- drug_class_stats_data
      } else if (input$volcanoTarget == "cancerType") {
        data_source <- cancer_type_stats_data
      }
      
      tagList(
        volcanoPlotServer("volcano1", data = data_source, target_col = input$volcanoTarget, plot_title = "Volcano Plot"),  # Use volcanoPlotServer for Volcano plot
        # Caption
        tags$p(
          style = "font-style: italic; font-size: 12px; color: #555; margin-top: 8px;",
          "Shows odds (ROR) of reporting an adverse event (AE) and its statistical significance (adjusted p-value) for selected column.
          Points on the top right indicate increased risk for the AE in the context of the selected target.
          Points on the top left indicate decreased risk for the AE in the context of the selected target."
        )
      )
    } else if (input$plotType == "Chord") {
      # Chord Diagram
      tagList(
        chordNetworkOutput("chordPlot", height = "1100px"),  # Use chordNetworkOutput for Chord plot
        # Caption
        tags$p(
          style = "font-style: italic; font-size: 12px; color: #555; margin-top: 8px;",
          "Shows the frequency of co-occurrence for selected column."
        )
      )
    }
  })

  # Render Onset Plot
  output$onsetPlot <- renderPlot({
    req(filteredData())
    renderOnsetPlot(filteredData(), input, output, session)
  })

  # Render Sankey Plot
  output$sankeyPlot <- renderSankeyNetwork({
    req(filteredData())
    renderSankeyPlot(filteredData(), input, output, session)
  })

  # Render Chord Plot
  output$chordPlot <- renderchordNetwork({
    req(chord_data())
    # You may need to preprocess filteredData() to get a data.table with source, target, value columns
    renderChordPlot(
      data = chord_data(),  # your processed data.table
      source_col = "source",
      target_col = "target",
      value_col = "value",
      group_colors = ae_category_palette  # or another palette
    )
  })

  # Observe when Chord plot is shown and trigger JS patch
  observeEvent(input$plotType, {
    if (input$plotType == "Chord") {
      # Delay to ensure plot is rendered
      session$sendCustomMessage("patchChordLabels", list())
    }
  })
}