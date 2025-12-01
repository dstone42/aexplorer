library(shiny)
library(future)
library(promises)
library(data.table)
library(ggplot2)
library(jsonlite)
library(systemfonts)
library(plotly)
library(orca)
library(later)
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
    # Remove caseid, event_dt, and time_to_onset from choices
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

  # Onset Plot custom palette helpers
  onsetFacetCategories <- reactive({
    req(onsetFilteredData())
    # Prefer row facet; if absent use column facet; else NULL
    if (!is.null(input$facetVarRow) && input$facetVarRow != "None") {
      vals <- unique(na.omit(onsetFilteredData()[[input$facetVarRow]]))
    } else if (!is.null(input$facetVarCol) && input$facetVarCol != "None") {
      vals <- unique(na.omit(onsetFilteredData()[[input$facetVarCol]]))
    } else {
      vals <- c("All")
    }
    vals <- vals[vals != ""]
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
    req(input$onsetUseCustomPalette, input$onsetPaletteMode == "Manual Colors")
    cats <- onsetFacetCategories()
    print(cats)
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
        val <- input[["onsetColor_All"]]
        singleCol <- if (is.null(val) || !nzchar(val)) grDevices::hcl.colors(1, "Set3") else val
      } else if (mode == "Upload JSON") {
        f <- input$onsetPaletteFile
        if (!is.null(f)) {
          js_raw <- try(jsonlite::fromJSON(f$datapath), silent = TRUE)
          if (inherits(js_raw, "try-error")) return(NULL)

          # Normalize the parsed JSON into either:
          # - named character vector (if object provided), or
          # - unnamed character vector (if array provided)
          if (is.list(js_raw) && !is.null(names(js_raw))) {
            # JSON object -> named list; coerce values to character
            js_vals <- vapply(js_raw, function(x) as.character(x[1]), character(1))
            # Keep the names
            names(js_vals) <- names(js_raw)
            singleCol <- js_vals[cats]  # pick in cats order (may produce NA for missing)
          } else if (is.atomic(js_raw) && !is.null(names(js_raw))) {
            # named atomic vector (rare), coerce to character
            js_vals <- as.character(js_raw)
            singleCol <- js_vals[cats]
          } else {
            # treat as an array of colors
            js_arr <- as.character(unlist(js_raw))
            base <- if (reverse) rev(js_arr) else js_arr
            singleCol <- makeLength(base, length(cats))
          }

          # If we got named values but some categories are missing, fill them below
          # (handled by subsequent code)
        }
      }
      if (is.null(singleCol)) singleCol <- "#888888"
      return(setNames(singleCol, "All"))
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
        if (!is.null(f)) {
          js_raw <- try(jsonlite::fromJSON(f$datapath), silent = TRUE)
          if (inherits(js_raw, "try-error")) return(NULL)

          # Normalize the parsed JSON into either:
          # - named character vector (if object provided), or
          # - unnamed character vector (if array provided)
          if (is.list(js_raw) && !is.null(names(js_raw))) {
            # JSON object -> named list; coerce values to character
            js_vals <- vapply(js_raw, function(x) as.character(x[1]), character(1))
            # Keep the names
            names(js_vals) <- names(js_raw)
            cols <- js_vals[cats]  # pick in cats order (may produce NA for missing)
          } else if (is.atomic(js_raw) && !is.null(names(js_raw))) {
            # named atomic vector (rare), coerce to character
            js_vals <- as.character(js_raw)
            cols <- js_vals[cats]
          } else {
            # treat as an array of colors
            js_arr <- as.character(unlist(js_raw))
            base <- if (reverse) rev(js_arr) else js_arr
            cols <- makeLength(base, length(cats))
          }

          # If we got named values but some categories are missing, fill them below
          # (handled by subsequent code)
        }
      }
    if (is.null(cols)) return(NULL)
    names(cols) <- cats
    cols
  })

  # Debounced palette reactive
  debouncedOnsetCustomPalette <- debounce(onsetCustomPalette, 100)  # 100 ms delay

  currentOnsetPalette <- reactiveVal(NULL)

  # Normalize and store palette whenever the (debounced) palette changes OR when categories change.
  # Use an observer (dependent on debouncedOnsetCustomPalette and onsetFacetCategories)
  observe({
    # Depend on both debounced palette and categories
    pal_candidate <- debouncedOnsetCustomPalette()
    cats <- onsetFacetCategories()

    # helper: extend or truncate to desired length
    makeLengthFallback <- function(cols, want) {
      if (length(cols) >= want) return(cols[seq_len(want)])
      grDevices::colorRampPalette(cols)(want)
    }
    
    # Ensure cats is a character vector with at least one item
    if (is.null(cats) || length(cats) == 0) cats <- "All"

    # If the reactive returned a NULL/empty palette, create a fallback palette (Set3)
    if (is.null(pal_candidate) || length(pal_candidate) == 0) {
      pal_normalized <- setNames(
        suppressWarnings(grDevices::hcl.colors(length(cats), "Set3")),
        cats
      )
    } else {
      # If pal_candidate has names, try to subset/reorder to current cats
      if (!is.null(names(pal_candidate))) {
        # Keep only those names that match cats (and in cats order); if names don't match any, fall back
        common <- intersect(cats, names(pal_candidate))
        if (length(common) > 0) {
          pal_normalized <- pal_candidate[common]
          # If there are cats missing from the named palette, append defaults for those
          missing_cats <- setdiff(cats, names(pal_normalized))
          if (length(missing_cats) > 0) {
            pal_normalized <- c(
              pal_normalized,
              setNames(grDevices::hcl.colors(length(missing_cats), "Set3"), missing_cats)
            )
          }
          # Reorder to cats
          pal_normalized <- pal_normalized[cats]
        } else {
          # Named palette didn't match current categories -> treat as array below
          pal_candidate <- unname(as.character(pal_candidate))
          pal_normalized <- setNames(
            makeLengthFallback(pal_candidate, length(cats)),
            cats
          )
        }
      } else {
        # Unnamed palette (array): apply in order to cats. If shorter, extend by ramping.
        pal_candidate <- unname(as.character(pal_candidate))
        pal_normalized <- setNames(makeLengthFallback(pal_candidate, length(cats)), cats)
      }
    }

    # Save normalized palette to reactiveVal
    currentOnsetPalette(pal_normalized)
  })

  # Download (named key-value JSON)
  output$download_onset_palette_json_kv <- downloadHandler(
    filename = function() { "onset_palette_named.json" },
    content = function(file) {
      pal <- currentOnsetPalette()
      jsonlite::write_json(as.list(pal), file, auto_unbox = TRUE, pretty = TRUE)
    }
  )

  # Download (array JSON)
  output$download_onset_palette_json_array <- downloadHandler(
    filename = function() { "onset_palette_array.json" },
    content = function(file) {
      pal <- currentOnsetPalette()
      jsonlite::write_json(as.list(unname(pal)), file, auto_unbox = TRUE, pretty = TRUE)
    }
  )

  # --- existing onset plot render updated to pass custom palette ---
  output$onsetPlot <- renderPlot({
    req(onsetFilteredData())
    pal <- debouncedOnsetCustomPalette()
    # req(pal)  # Only render when palette is ready
    renderOnsetPlot(onsetFilteredData(), input, output, session, custom_palette = pal)
  })

  output$download_onset_plot <- downloadHandler(
    filename = function() { "onset_plot.png" },
    content = function(file) {
      plot <- renderOnsetPlot(onsetFilteredData(), input, output, session, custom_palette = debouncedOnsetCustomPalette())
      ggsave(file, plot = plot, width = 10, height = onsetPlotHeight() / 100, dpi = 300)
    }
  )

  # --- Sankey Plot ---

  # Observe the sankeySubsetData input and update the sankeyFilterColumn choices accordingly
  observe({
    choices <- names(data())  # Get column names for the sankeyFilterColumn dropdown
    # Remove caseid, event_dt, and time_to_onset from choices
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

  # Custom colors for volcano plot
  volcano_custom_colors <- reactive({
    if (!isTRUE(input$volcanoUseCustomColors)) return(NULL)
    if (input$volcanoColorMode == "Color Picker") {
      return(c(
        "Increased Risk" = input$volcanoColorIncreased %||% "#CC0000",
        "Decreased Risk" = input$volcanoColorDecreased %||% "#0071c5",
        "Insignificant" = input$volcanoColorInsignificant %||% "grey"
      ))
    } else if (input$volcanoColorMode == "Upload JSON") {
      f <- input$volcanoColorFile
      if (!is.null(f)) {
        cols <- tryCatch(jsonlite::fromJSON(f$datapath), error = function(e) NULL)
        # Ensure correct names
        if (!is.null(cols) && all(c("Increased Risk", "Decreased Risk", "Insignificant") %in% names(cols))) {
          return(cols)
        }
      }
    }
    NULL
  })

  output$download_volcano_colors_json <- downloadHandler(
    filename = function() { "volcano_colors.json" },
    content = function(file) {
      cols <- volcano_custom_colors()
      if (is.null(cols)) {
        cols <- c(
          "Increased Risk" = "#CC0000",
          "Decreased Risk" = "#0071c5",
          "Insignificant" = "grey"
        )
      }
      jsonlite::write_json(as.list(cols), file, auto_unbox = TRUE, pretty = TRUE)
    }
  )

  volcanoPlotServer(
    "volcano_plot_container",
    data = volcano_data,
    target_col = reactive(input$volcanoTarget),
    measure = reactive(input$volcanoMeasure),
    plot_title = "Volcano Plot",
    custom_colors = volcano_custom_colors
  )

# --- Chord & Overlap (Co-occurrence) ---

  output$cooccurrence_plot_container <- renderUI({
    if (input$cooccurPlotType == "chord") {
      tagList(
        withSpinner(chordNetworkOutput("chordPlot", height = "1100px")),
        tags$p(
          style = "font-style: italic; font-size: 12px; color: #555; margin-top: 8px;",
          "Shows the frequency of co-occurrence for selected column."
        ),
        downloadButton("download_chord_plot_html", "Download Chord as HTML"),
        downloadButton("download_chord_plot_png", "Download Chord as PNG")
      )
    } else {
      # Overlap heatmap module UI (already provides caption)
      overlapHeatmapUI("overlap1")
    }
  })

  # Chord plot data selection (unchanged)
  output$chordPlot <- renderchordNetwork({
    req(input$cooccurPlotType == "chord")
    req(chord_data())
    palette <- if (input$chordColumn == "AE Category") ae_category_palette else NULL
    renderChordPlot(
      data = chord_data(),
      source_col = "source",
      target_col = "target",
      value_col = "value",
      group_colors = palette
    )
  })

  # Chord download handlers (restored)
  output$download_chord_plot_html <- downloadHandler(
    filename = function() "chord_plot.html",
    content = function(file) {
      req(input$cooccurPlotType == "chord")
      req(chord_data())
      palette <- if (input$chordColumn == "AE Category") ae_category_palette else NULL
      widget <- renderChordPlot(
        data = chord_data(),
        source_col = "source",
        target_col = "target",
        value_col = "value",
        group_colors = palette
      )
      htmlwidgets::saveWidget(widget, file, selfcontained = TRUE)
    }
  )

  output$download_chord_plot_png <- downloadHandler(
    filename = function() "chord_plot.png",
    content = function(file) {
      req(input$cooccurPlotType == "chord")
      req(chord_data())
      palette <- if (input$chordColumn == "AE Category") ae_category_palette else NULL
      widget <- renderChordPlot(
        data = chord_data(),
        source_col = "source",
        target_col = "target",
        value_col = "value",
        group_colors = palette
      )
      tmp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(widget, tmp_html, selfcontained = TRUE)
      # Adjust dimensions as desired
      webshot2::webshot(tmp_html, file = file, vwidth = 1300, vheight = 1300)
    }
  )

  # Keep existing chord_data assignment inside chord UI logic
  observe({
    if (input$cooccurPlotType == "chord" && input$chordColumn == "AE Category") {
      chord_data(AE_Category_freq_table)
    } else if (input$cooccurPlotType == "chord") {
      chord_data(NULL)
    }
  })

  # Trigger JS patches only when chord visible
  observeEvent(list(input$chordColumn, input$cooccurPlotType), {
    if (input$cooccurPlotType == "chord") {
      session$sendCustomMessage("patchChordLabels", list())
      session$sendCustomMessage("patchChordTooltips", list())
    }
  })
  observeEvent(list(input$exploreTab, input$cooccurPlotType), {
    if (input$exploreTab == "Co-occurrence" && input$cooccurPlotType == "chord") {
      session$sendCustomMessage("patchChordLabels", list())
      session$sendCustomMessage("patchChordTooltips", list())
    }
  })

  # Overlap data selection (reuses existing logic; input IDs unchanged)
  observe({
    if (input$overlapTarget == "AE Category") {
      overlap_data(AE_category_overlap)
    } else {
      overlap_data(NULL)
    }
  })

  # Overlap heatmap module server (unchanged)
  overlapHeatmapServer("overlap1", overlap_data, reactive(input$overlapCluster))
}