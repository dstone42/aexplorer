library(shiny)
library(future)
library(promises)
library(data.table)
library(ggplot2)
library(jsonlite)
library(systemfonts)
library(plotly)
library(orca)

# Source visualization modules
source("modules/onset_plot.R")
source("modules/sankey_plot.R")
source("modules/volcano_plot.R")
source("modules/chord_diagram.R")
source("modules/overlap.R")

# ---- CACHE DATA HERE ----
# Main table
cached_data <- fread("data/data.csv", sep = "$")
# Volcano plot data
drug_category_stats <- fread("data/drug_category_stats.csv")
cancer_type_stats <- fread("data/cancer_type_stats.csv")
# Chord diagram data
AE_Category_freq_table <- fread("data/AE_Category_freq_table.csv")
AE_Category_freq_table <- formatChordTable(AE_Category_freq_table)  # Format the frequency table
cancer_type_freq_table <- fread("data/cancer_type_freq_table.csv")
cancer_type_freq_table <- formatChordTable(cancer_type_freq_table)  # Format the frequency table
drug_category_freq_table <- fread("data/drug_category_freq_table.csv")
drug_category_freq_table <- formatChordTable(drug_category_freq_table)  # Format the frequency table
# Overlap coefficient data
AE_category_overlap <- as.matrix(read.csv("data/AE_Category_overlap_coefficient.csv", row.names = 1, check.names = FALSE))
# -------------------------

column_labels <- c(
  "Outcome" = "outc_cod",
  "AE Category" = "AE_Category",
  "Sex" = "sex",
  "Drug Category" = "drug_category",
  "Cancer Type" = "cancerType",
  "Other Drugs" = "other_drug_name",
  "Cancer Drugs" = "cancer_drug_name",
  "Drug Category (Detailed)" = "drug_category_expanded",
  "Quarter" = "quarter",
  "AE" = "AE",
  "AE Category (Detailed)" = "AE_Category_Expanded"
)

# Define the server logic
server <- function(input, output, session) {

  # --- Data Loading and Preprocessing ---
  
  # Reactive value to store the loaded data
  data <- reactiveVal(cached_data)
  drug_category_stats_data <- reactiveVal(drug_category_stats)
  cancer_type_stats_data <- reactiveVal(cancer_type_stats)
  chord_data <- reactiveVal(NULL)
  overlap_data <- reactiveVal(NULL)

  # Read palettes
  ae_category_palette <- fromJSON("www/palettes/ae_categories.json")
  outcomes_palette <- fromJSON("www/palettes/outcomes.json")
  drug_category_palette <- fromJSON("www/palettes/drug_categories.json")

  # Ensure palettes are available globally
  assign("ae_category_palette", ae_category_palette, envir = .GlobalEnv)
  assign("outcomes_palette", outcomes_palette, envir = .GlobalEnv)
  assign("drug_category_palette", drug_category_palette, envir = .GlobalEnv)

  # --- Plot Rendering Logic ---

  # --- Onset Plot ---

  # Observe the onsetSubsetData input and update the onsetFilterColumn choices accordingly
  observe({
    choices <- names(data())  # Get column names for the onsetFilterColumn dropdown
    # Remove primaryid, event_dt, and time_to_onset from choices
    choices <- column_labels
    updateSelectInput(session, "onsetFilterColumn", choices = choices)  # Populate onsetFilterColumn dropdown
    
  })

  # Observe the onsetFilterColumn input and update the onsetFilterValues choices accordingly
  observe({
    updateSelectizeInput(session, "onsetFilterValues", choices = unique(data()[[input$onsetFilterColumn]]), server = TRUE)  # Populate onsetFilterValues dropdown
  })

  # Filter data based on user input
  onsetFilteredData <- reactive({
    req(data())
    if (input$onsetSubsetData == "Yes" && !is.null(input$onsetFilterColumn) && !is.null(input$onsetFilterValues)) {
      data()[data()[[input$onsetFilterColumn]] %in% input$onsetFilterValues, ]
    } else {
      data()
    }
  })

  # Dynamically calculate plot height based on the number of facets
  onsetPlotHeight <- reactive({
    req(onsetFilteredData())
    if (input$facetVarRow != "None") {
      num_facets <- length(unique(onsetFilteredData()[[input$facetVarRow]]))
      min_height <- 300  # Minimum height for the plot
      facet_height <- 50  # Height per facet
      max_height <- 1200  # Maximum height for the plot
      
      # Calculate total height
      total_height <- min(max(min_height, num_facets * facet_height), max_height)
      return(total_height)
    } else {
      num_facets <- 1  # Default to 1 if no faceting
      return(250)  # Default height for non-faceted plots
    }
  })

  output$onset_plot_container <- renderUI({
    div(
      style = "overflow-y: auto; max-height: 1200px;",
      withSpinner(uiOutput("onset_plot_with_caption"))
    )
  })

  output$onset_plot_with_caption <- renderUI({
    tagList(
        plotOutput("onsetPlot", height = onsetPlotHeight()),  # Use plotOutput for ggplot
        # Caption
        tags$p(
          style = "font-style: italic; font-size: 12px; color: #555; margin-top: 8px;",
          "Time to onset (in weeks) from drug administration to adverse events. Values greater than 52 weeks are grouped into '>52'."
        )
      )
  })

  output$onsetPlot <- renderPlot({
    req(onsetFilteredData())
    renderOnsetPlot(onsetFilteredData(), input, output, session)
  })

  output$download_onset_plot <- downloadHandler(
    filename = function() { "onset_plot.png" },
    content = function(file) {
      plot <- renderOnsetPlot(onsetFilteredData(), input, output, session)
      ggsave(file, plot = plot, width = 10, height = onsetPlotHeight() / 100, dpi = 300)
    }
  )

  # --- Sankey Plot ---

  # Observe the sankeySubsetData input and update the sankeyFilterColumn choices accordingly
  observe({
    choices <- names(data())  # Get column names for the sankeyFilterColumn dropdown
    # Remove primaryid, event_dt, and time_to_onset from choices
    choices <- column_labels
    updateSelectInput(session, "sankeyFilterColumn", choices = choices)  # Populate sankeyFilterColumn dropdown
  })

  # Observe the sankeyFilterColumn input and update the sankeyFilterValues choices accordingly
  observe({
    updateSelectizeInput(session, "sankeyFilterValues", choices = unique(data()[[input$sankeyFilterColumn]]), server = TRUE)  # Populate sankeyFilterValues dropdown
  })

  # Filter data based on user input
  sankeyFilteredData <- reactive({
    req(data())
    if (input$sankeySubsetData == "Yes" && !is.null(input$sankeyFilterColumn) && !is.null(input$sankeyFilterValues)) {
      data()[data()[[input$sankeyFilterColumn]] %in% input$sankeyFilterValues, ]
    } else {
      data()
    }
  })

  output$sankey_plot_container <- renderUI({
    div(
      style = "overflow-y: auto; max-height: 1200px;",
      withSpinner(uiOutput("sankey_plot_with_caption"))
    )
  })

  output$sankey_plot_with_caption <- renderUI({
    selected <- input$sankey_selected
    if (is.null(selected) || length(selected) < 2) {
      tags$div(
        class = "alert alert-warning",
        "Please select at least two columns for the Sankey plot."
      )
    } else {
      tagList(
        sankeyNetworkOutput("sankeyPlot"),  # Use sankeyNetworkOutput for Sankey plot
        # Caption
        tags$p(
          style = "font-style: italic; font-size: 12px; color: #555; margin-top: 8px;",
          "Proportions of adverse event cases across selected columns."
        )
      )
    }
  })

  output$sankeyPlot <- renderSankeyNetwork({
    req(sankeyFilteredData())
    req(input$sankey_selected)
    req(length(input$sankey_selected) >= 2)  # Ensure at least two columns are selected
    renderSankeyPlot(sankeyFilteredData(), input, output, session)
  })

  output$download_sankey_plot_html <- downloadHandler(
    filename = function() { "sankey_plot.html" },
    content = function(file) {
      sankey <- renderSankeyPlot(sankeyFilteredData(), input, output, session)
      htmlwidgets::saveWidget(sankey, file)
    }
  )

  output$download_sankey_plot_png <- downloadHandler(
    filename = function() { "sankey_plot.png" },
    content = function(file) {
      sankey <- renderSankeyPlot(sankeyFilteredData(), input, output, session)
      # Save to a temporary HTML file first
      temp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(sankey, temp_html, selfcontained = TRUE)
      # Use webshot2 to capture as PNG
      webshot2::webshot(temp_html, file = file, vwidth = 1200, vheight = 900)
    }
  )

  # --- Volcano Plot ---

  output$volcano_plot_container <- renderUI({
    div(
      style = "overflow-y: auto; max-height: 1200px;",
      withSpinner(uiOutput("volcano_plot_with_caption"))
    )
  })

  output$volcano_plot_with_caption <- renderUI({
    # Dynamically select the data source for the Volcano plot based on input$volcanoTarget
      req(input$volcanoTarget)
      if (input$volcanoTarget == "drug_category") {
        data_source <- drug_category_stats_data
      } else if (input$volcanoTarget == "cancer_type") {
        data_source <- cancer_type_stats_data
      }

      volcanoPlotServer("volcano1", data = data_source, target_col = input$volcanoTarget, plot_title = "Volcano Plot")
      
      tagList(
        girafeOutput("volcano1-volcanoPlot"),
        # Caption
        tags$p(
          style = "font-style: italic; font-size: 12px; color: #555; margin-top: 8px;",
          "Shows odds (ROR) of reporting an adverse event (AE) and its statistical significance (adjusted p-value) for selected column.
          Points on the top right indicate increased risk for the AE in the context of the selected target.
          Points on the top left indicate decreased risk for the AE in the context of the selected target."
        )
      )
  })

  output$download_volcano_plot <- downloadHandler(
    filename = function() { "volcano_plot.png" },
    content = function(file) {
      # Dynamically select the data source for the Volcano plot based on input$volcanoTarget
      req(input$volcanoTarget)
      if (input$volcanoTarget == "drug_category") {
        data_source <- drug_category_stats_data()
      } else if (input$volcanoTarget == "cancer_type") {
        data_source <- cancer_type_stats_data()
      }
      plot <- volcanoPlotObject(data_source, target_col = input$volcanoTarget, plot_title = "Volcano Plot")
      ggsave(file, plot = plot, width = 18, height = 10, dpi = 300)
    }
  )

  # --- Chord Diagram ---

  output$chord_plot_container <- renderUI({
    withSpinner(uiOutput("chord_plot_with_caption"))  # No scrollable div
  })

  output$chord_plot_with_caption <- renderUI({
    # Dynamically select the data source for the Chord plot based on input$chordColumn
      req(input$chordColumn)
      if (input$chordColumn == "AE Category") {
        chord_data(AE_Category_freq_table)  # Use the precomputed frequency table for AE Category
      } else if (input$chordColumn == "Drug Category") {
        chord_data(drug_category_freq_table)  # Use the precomputed frequency table for Drug Category
      } else if (input$chordColumn == "Cancer Type") {
        chord_data(cancer_type_freq_table)  # Use the precomputed frequency table for Cancer Type
      } else {
        chord_data(NULL)  # Handle other cases or set to NULL if no data available
      }
      tagList(
        chordNetworkOutput("chordPlot", height = "1100px"),  # Use chordNetworkOutput for Chord plot
        # Caption
        tags$p(
          style = "font-style: italic; font-size: 12px; color: #555; margin-top: 8px;",
          "Shows the frequency of co-occurrence for selected column."
        )
      )
  })

  output$chordPlot <- renderchordNetwork({
    req(chord_data())
    palette <- NULL
    if (input$chordColumn == "AE Category") {
      palette <- ae_category_palette
    } else if (input$chordColumn == "Drug Category") {
      palette <- drug_category_palette
    } else {
      palette <- NULL
    }
    # You may need to preprocess filteredData() to get a data.table with source, target, value columns
    renderChordPlot(
      data = chord_data(),  # your processed data.table
      source_col = "source",
      target_col = "target",
      value_col = "value",
      group_colors = palette  # or another palette
    )
  })

  # Observe when Chord plot is shown and when a new column is selected and trigger JS patch
  observeEvent(input$chordColumn, {
    # Delay to ensure plot is rendered
    session$sendCustomMessage("patchChordLabels", list())
    session$sendCustomMessage("patchChordTooltips", list())
  })
  observeEvent(input$exploreTab, {
    if (input$exploreTab == "Chord") {
      # Delay to ensure plot is rendered
      session$sendCustomMessage("patchChordLabels", list())
      session$sendCustomMessage("patchChordTooltips", list())
    }
  })

  output$download_chord_plot_html <- downloadHandler(
    filename = function() { "chord_plot.html" },
    content = function(file) {
      palette <- NULL
      if (input$chordColumn == "AE Category") {
        palette <- ae_category_palette
      } else if (input$chordColumn == "Drug Category") {
        palette <- drug_category_palette
      } else {
        palette <- NULL
      }
      chord <- renderChordPlot(
        data = chord_data(),
        source_col = "source",
        target_col = "target",
        value_col = "value",
        group_colors = palette
      )
      htmlwidgets::saveWidget(chord, file)
    }
  )

  output$download_chord_plot_png <- downloadHandler(
    filename = function() { "chord_plot.png" },
    content = function(file) {
      palette <- NULL
      if (input$chordColumn == "AE Category") {
        palette <- ae_category_palette
      } else if (input$chordColumn == "Drug Category") {
        palette <- drug_category_palette
      } else {
        palette <- NULL
      }
      chord <- renderChordPlot(
        data = chord_data(),
        source_col = "source",
        target_col = "target",
        value_col = "value",
        group_colors = palette
      )
      temp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(chord, temp_html, selfcontained = TRUE)
      webshot2::webshot(temp_html, file = file, vwidth = 1200, vheight = 1100)
    }
  )

  # --- Overlap Coefficient Plot ---

  observe({
    if (input$overlapTarget == "AE Category") {
      overlap_data(AE_category_overlap)  # Use the preloaded AE category overlap data
    } else {
      overlap_data(NULL)  # Handle case where no valid target is selected
    }
  })

  overlapHeatmapServer("overlap1", overlap_data, reactive(input$overlapCluster))
}