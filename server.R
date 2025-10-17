library(shiny)
library(future)
library(promises)
library(data.table)
library(ggplot2)
library(jsonlite)
library(systemfonts)
library(plotly)
library(orca)
library(later)   # <--- add this
library(colourpicker)
library(RColorBrewer)
library(viridisLite)

# Null-coalescing helper (was causing "%||%" not found)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

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
  volcano_data <- reactiveVal(NULL)
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
    if (isTRUE(input$onsetSubsetData) && !is.null(input$onsetFilterColumn) && !is.null(input$onsetFilterValues)) {
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

  # --- Onset Plot custom palette helpers ---
  onsetFacetCategories <- reactive({
    req(onsetFilteredData())
    # Prefer row facet; if absent use column facet; else NULL
    if (!is.null(input$facetVarRow) && input$facetVarRow != "None") {
      vals <- unique(na.omit(onsetFilteredData()[[input$facetVarRow]]))
    } else if (!is.null(input$facetVarCol) && input$facetVarCol != "None") {
      vals <- unique(na.omit(onsetFilteredData()[[input$facetVarCol]]))
    } else {
      return(NULL)
    }
    vals[order(vals)]
  })

  output$onsetPresetNameUI <- renderUI({
    req(input$onsetUseCustomPalette, input$onsetPaletteMode == "Preset Palette")
    if (input$onsetPresetFamily == "Brewer") {
      pals <- rownames(RColorBrewer::brewer.pal.info)[RColorBrewer::brewer.pal.info$category %in% c("qual", "div")]
      selectInput("onsetBrewerPalette", "Brewer Palette", choices = pals, selected = "Set3")
    } else if (input$onsetPresetFamily == "Viridis") {
      selectInput("onsetViridisOption", "Viridis Option", choices = c("viridis","magma","plasma","inferno","cividis","turbo","mako","rocket"), selected = "viridis")
    } else {
      NULL
    }
  })

  output$onset_manual_colors <- renderUI({
    req(input$onsetUseCustomPalette, input$onsetPaletteMode == "Manual Colors", input$facetVarRow != "None")
    cats <- onsetFacetCategories()
    req(cats)
    base_cols <- suppressWarnings(grDevices::hcl.colors(length(cats), "Set3"))
    lapply(seq_along(cats), function(i) {
      colourInput(
        inputId = paste0("onsetColor_", make.names(cats[i])),
        label = cats[i],
        value = base_cols[i],
        allowTransparent = FALSE
      )
    })
  })

  onsetCustomPalette <- reactive({
    if (!isTRUE(input$onsetUseCustomPalette)) return(NULL)
    activeFacet <- if (!is.null(input$facetVarRow) && input$facetVarRow != "None") {
      input$facetVarRow
    } else if (!is.null(input$facetVarCol) && input$facetVarCol != "None") {
      input$facetVarCol
    } else NULL

    cats <- onsetFacetCategories()
    mode <- input$onsetPaletteMode
    reverse <- isTRUE(input$onsetPaletteReverse)
    maxN <- input$onsetPresetMax %||% 20

    makeLength <- function(cols, want) {
      if (length(cols) >= want) return(cols[seq_len(want)])
      grDevices::colorRampPalette(cols)(want)
    }

    # Non-faceted
    if (is.null(activeFacet) || is.null(cats)) {
      singleCol <- NULL
      if (mode == "Preset Palette") {
        fam <- input$onsetPresetFamily
        if (fam == "Brewer" && !is.null(input$onsetBrewerPalette)) {
          info <- RColorBrewer::brewer.pal.info[input$onsetBrewerPalette, ]
            n_use <- max(3, min(3, info$max))
            base <- RColorBrewer::brewer.pal(n_use, input$onsetBrewerPalette)
            singleCol <- base[1]
        } else if (fam == "Viridis" && !is.null(input$onsetViridisOption)) {
          singleCol <- viridisLite::viridis(1, option = input$onsetViridisOption)
        } else if (fam == "Okabe-Ito") {
          singleCol <- "#E69F00"
        } else if (fam == "Tableau 10") {
          singleCol <- "#4E79A7"
        }
        if (reverse && !is.null(singleCol)) singleCol <- rev(singleCol)
      } else if (mode == "Manual Colors") {
        singleCol <- grDevices::hcl.colors(1, "Set3")
      } else if (mode == "Upload JSON") {
        f <- input$onsetPaletteFile
        if (!is.null(f)) {
          js <- try(jsonlite::fromJSON(f$datapath), silent = TRUE)
          if (!inherits(js, "try-error") && is.character(js) && length(js) >= 1) singleCol <- js[1]
        }
      }
      if (is.null(singleCol)) singleCol <- "#888888"
      return(setNames(singleCol, "single"))
    }

    # Faceted
    cols <- NULL
    if (mode == "Preset Palette") {
      fam <- input$onsetPresetFamily
      if (fam == "Brewer" && !is.null(input$onsetBrewerPalette)) {
        info <- RColorBrewer::brewer.pal.info[input$onsetBrewerPalette, ]
        n_base <- max(3, min(info$max, length(cats), maxN))
        base <- RColorBrewer::brewer.pal(n_base, input$onsetBrewerPalette)
        cols <- makeLength(base, length(cats))
        if (reverse) cols <- rev(cols)
      } else if (fam == "Viridis" && !is.null(input$onsetViridisOption)) {
        cols <- viridisLite::viridis(length(cats), option = input$onsetViridisOption, direction = if (reverse) -1 else 1)
      } else if (fam == "Okabe-Ito") {
        base <- c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#000000")
        if (reverse) base <- rev(base)
        cols <- makeLength(base, length(cats))
      } else if (fam == "Tableau 10") {
        base <- c("#4E79A7","#F28E2B","#E15759","#76B7B2","#59A14F","#EDC948","#B07AA1","#FF9DA7","#9C755F","#BAB0AC")
        if (reverse) base <- rev(base)
        cols <- makeLength(base, length(cats))
      }
    } else if (mode == "Manual Colors") {
      cols <- vapply(cats, function(ct) {
        val <- input[[paste0("onsetColor_", make.names(ct))]]
        if (is.null(val) || !nzchar(val)) "#888888" else val
      }, character(1))
    } else if (mode == "Upload JSON") {
      f <- input$onsetPaletteFile
      if (is.null(f)) return(NULL)
      js <- try(jsonlite::fromJSON(f$datapath), silent = TRUE)
      if (inherits(js, "try-error")) return(NULL)
      if (is.character(js) && is.null(names(js))) {
        base <- if (reverse) rev(js) else js
        cols <- makeLength(base, length(cats))
      } else if (is.character(js) && !is.null(names(js))) {
        cols <- js[cats]
        if (reverse) cols <- rev(cols)
        if (anyNA(cols)) {
          miss <- which(is.na(cols))
          cols[miss] <- grDevices::hcl.colors(length(miss), "Set3")
        }
      }
    }
    if (is.null(cols)) return(NULL)
    names(cols) <- cats
    cols
  })

  # --- existing onset plot render updated to pass custom palette ---
  output$onsetPlot <- renderPlot({
    req(onsetFilteredData())
    renderOnsetPlot(onsetFilteredData(), input, output, session, custom_palette = onsetCustomPalette())
  })

  output$download_onset_plot <- downloadHandler(
    filename = function() { "onset_plot.png" },
    content = function(file) {
      plot <- renderOnsetPlot(onsetFilteredData(), input, output, session, custom_palette = onsetCustomPalette())
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

  volcano_data <- reactive({
    switch(input$volcanoTarget,
      "drug_category" = drug_category_stats_data(),
      "cancer_type" = cancer_type_stats_data(),
      NULL
    )
  })

  volcanoPlotServer("volcano_plot_container", data = volcano_data, target_col = reactive(input$volcanoTarget), measure = reactive(input$volcanoMeasure), plot_title = "Volcano Plot")

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