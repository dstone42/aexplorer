library(shiny)
library(shinycssloaders)
library(shinyBS)  # Add this library for tooltips
library(shinyjs)
library(bslib)
library(sortable)
library(colourpicker)

source("modules/overlap.R", local = TRUE)
source("modules/volcano_plot.R", local = TRUE)

# Use a modern Bootstrap theme
custom_theme <- bs_theme(
  version = 5,
  bootswatch = "cosmo",
  base_font = font_google("Inter"),
  heading_font = font_google("Montserrat"),
  primary = "#2C3E50",
  secondary = "#18BC9C"
)

column_labels <- c(
  "Outcome" = "outc_cod",
  "AE Category" = "AE_Category",
  "Sex" = "sex",
  "Drug Category" = "drug_category",
  "Cancer Type" = "cancer_type"
)

# Create another list with the names and values of column_labels switched
column_labels_switched <- setNames(names(column_labels), column_labels)

# Define the UI
ui <- navbarPage(
  title = div(
    style = "display: flex; align-items: center; width: 100vw; padding-right: 32px; position: relative;",
    span(
      style = "flex: 1; z-index: 2;",
      "Cancer Treatment Adverse Event Explorer"
    ),
    tags$img(
      src = "tanLabLogo.png",
      height = "100px",
      style = "
        position: absolute;
        right: 30px;
        top: 5px;
        margin-right: 0;
        z-index: 1;
        "
    )
  ),
  theme = custom_theme,
  id = "main_navbar",
  header = tags$head(
    # tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$link(rel = "icon", type = "image/png", href = "tanLabLogo.png"),
    tags$script(src = "https://d3js.org/d3.v6.min.js"),
    tags$script(src = "patchChordLabels.js"),  # Custom JS for chord label wrapping
    tags$script(src = "patchChordTooltips.js")  # Custom JS for chord
  ),
  tabPanel(
    title = "Data Exploration",
    fluidPage(
      tabsetPanel(
        id = "exploreTab",
        # --- ROR Tab ---
        tabPanel("ROR",
          fluidRow(
            column(
              2,
              div(
                class = "sidebar-card shadow-sm rounded p-3 bg-white",
                # Plot options
                selectizeInput(
                  "volcanoTarget",
                  "Select Target Column",
                  choices = column_labels[c("Drug Category", "Cancer Type")],
                  selected = "Drug Category"
                ),
                radioButtons(
                  "volcanoMeasure",
                  "Disproportionality Measure",
                  choices = c("ROR", "PRR"),
                  selected = "ROR"
                ),
                # Custom color options for volcano plot
                checkboxInput("volcanoUseCustomColors", "Custom Colors", FALSE),
                conditionalPanel(
                  condition = "input.volcanoUseCustomColors",
                  selectInput("volcanoColorMode", "Mode", c("Color Picker", "Upload JSON"), selected = "Color Picker"),
                  conditionalPanel(
                    condition = "input.volcanoColorMode == 'Color Picker'",
                    colourInput("volcanoColorIncreased", "Increased Risk", value = "#CC0000"),
                    colourInput("volcanoColorDecreased", "Decreased Risk", value = "#0071c5"),
                    colourInput("volcanoColorInsignificant", "Insignificant", value = "grey")
                  ),
                  conditionalPanel(
                    condition = "input.volcanoColorMode == 'Upload JSON'",
                    fileInput("volcanoColorFile", "Upload JSON (named object)", accept = c(".json")),
                    downloadButton("download_volcano_colors_json", "Download Current Colors (JSON)")
                  )
                )
              )
            ),
            column(
              10,
              div(
                class = "main-card shadow-sm rounded p-4 bg-white",
                volcanoPlotUI("volcano_plot_container")
              )
            )
          )
        ),
        # --- Onset Tab ---
        tabPanel("Onset",
          fluidRow(
            column(
              2,
              div(
                class = "sidebar-card shadow-sm rounded p-3 bg-white",
                # Plot options
                selectizeInput(
                  "facetVarRow",
                  "Facet By (Rows)",
                  choices = c("None" = "None", column_labels[c("Outcome", "AE Category", "Drug Category", "Sex")]),
                  selected = "None"
                ),
                selectizeInput(
                  "facetVarCol",
                  "Facet By (Columns)",
                  choices = c("None" = "None", column_labels[c("Outcome", "AE Category", "Drug Category", "Sex")]),
                  selected = "None"
                ),
                # Subset options
                checkboxInput(
                  "onsetSubsetData",
                  "Filter by Value",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.onsetSubsetData",
                  selectizeInput(
                    "onsetFilterColumn",
                    "Select Column to Filter",
                    choices = NULL
                  ),
                  selectizeInput(
                    "onsetFilterValues",
                    "Select Values to keep",
                    choices = NULL,
                    multiple = TRUE
                  )
                ),
                checkboxInput("onsetUseCustomPalette", "Custom Colors", FALSE),
                conditionalPanel(
                  condition = "input.onsetUseCustomPalette",
                  selectInput("onsetPaletteMode", "Mode", c("Preset Palette", "Manual Colors", "Upload JSON"), selected = "Preset Palette"),
                  conditionalPanel(
                    condition = "input.onsetPaletteMode == 'Preset Palette'",
                    selectInput("onsetPresetFamily", "Palette Family", c("Brewer", "Viridis", "Okabe-Ito", "Tableau 10"), selected = "Brewer"),
                    uiOutput("onsetPresetNameUI"),
                    checkboxInput("onsetPaletteReverse", "Reverse", FALSE),
                    numericInput("onsetPresetMax", "Max Colors (auto extend if needed)", 20, min = 2, max = 100, step = 1)
                  ),
                  conditionalPanel(
                    condition = "input.onsetPaletteMode == 'Manual Colors'",
                    uiOutput("onset_manual_colors")
                  ),
                  conditionalPanel(
                    condition = "input.onsetPaletteMode == 'Upload JSON'",
                    fileInput("onsetPaletteFile", "Upload JSON (named object or array)", accept = c(".json")),
                    # Download examples of currently-used palette:
                    div(style = "margin-top: 8px;", class = "d-flex flex-column gap-2",
                      downloadButton("download_onset_palette_json_kv", "Download Palette (named JSON)"),
                      downloadButton("download_onset_palette_json_array", "Download Palette (array JSON)")
                    )
                  )
                )
              )
            ),
            column(
              10,
              div(
                class = "main-card shadow-sm rounded p-4 bg-white",
                uiOutput("onset_plot_container"),
                downloadButton("download_onset_plot", "Download Plot")
              )
            )
          )
        ),
        # --- Sankey Tab ---
        tabPanel("Sankey",
          fluidRow(
            column(
              2,
              div(
                class = "sidebar-card shadow-sm rounded p-3 bg-white",
                # Plot options
                tags$label("Select & Order Columns for Sankey Plot"),
                bucket_list(
                  header = NULL,
                  group_name = "sankey_columns",
                  orientation = "vertical",
                  add_rank_list(
                    text = "Available Columns",
                    input_id = "sankey_available",
                    labels = column_labels_switched[c("AE_Category", "sex", "drug_category", "outc_cod")], # not selected by default
                    options = sortable_options(multiselect = FALSE)
                  ),
                  add_rank_list(
                    text = "Selected Columns (Drag to reorder)",
                    input_id = "sankey_selected",
                    labels = character(0), # default selection/order
                    options = sortable_options(multiselect = FALSE)
                  )
                ),
                # Subset options
                radioButtons(
                  "sankeySubsetData",
                  "Subset Data?",
                  choices = c("No", "Yes"),
                  selected = "No"
                ),
                conditionalPanel(
                  condition = "input.sankeySubsetData == 'Yes'",
                  selectizeInput(
                    "sankeyFilterColumn",
                    "Select Column to Filter",
                    choices = NULL
                  ),
                  selectizeInput(
                    "sankeyFilterValues",
                    "Select Values to keep",
                    choices = NULL,
                    multiple = TRUE
                  )
                )
              )
            ),
            column(
              10,
              div(
                class = "main-card shadow-sm rounded p-4 bg-white",
                uiOutput("sankey_plot_container"),
                downloadButton("download_sankey_plot_html", "Download Plot as HTML"),
                downloadButton("download_sankey_plot_png", "Download Plot as PNG")
              )
            )
          )
        ),
        # --- Co-occurrence Tab ---
        tabPanel("Co-occurrence",
          fluidRow(
            column(
              2,
              div(
                class = "sidebar-card shadow-sm rounded p-3 bg-white",
                selectInput(
                  "cooccurPlotType",
                  "Plot Type",
                  choices = c("Chord" = "chord", "Overlap Heatmap" = "heatmap"),
                  selected = "chord"
                ),
                conditionalPanel(
                  condition = "input.cooccurPlotType == 'chord'",
                  selectizeInput(
                    "chordColumn",
                    "Select Column for Chord Diagram",
                    choices = c("AE Category"),
                    selected = "AE Category"
                  )
                ),
                conditionalPanel(
                  condition = "input.cooccurPlotType == 'heatmap'",
                  tagList(
                    selectizeInput(
                      "overlapTarget",
                      "Select Target Column",
                      choices = c("AE Category"),
                      selected = "AE Category"
                    ),
                    checkboxInput(
                      "overlapCluster",
                      "Cluster Heatmap",
                      value = FALSE
                    )
                  )
                )
              )
            ),
            column(
              10,
              div(
                class = "main-card shadow-sm rounded p-4 bg-white",
                uiOutput("cooccurrence_plot_container")
              )
            )
          )
        )
      )
    )
  ),
  tabPanel(
    title = "About",
    fluidPage(
      div(
        class = "main-card shadow-sm rounded p-4 bg-white",
        tags$h2("About This Dashboard"),
        tags$p("This dashboard allows you to explore and visualize adverse event (AE) data related to cancer therapies. 
                Use the Visualizations tab to interact with the data."),
        tags$h6("Data Source"),
        tags$p("The data used in this dashboard is sourced from the FDA Adverse Event Reporting System (FAERS).
                We have processed and cleaned the data to provide meaningful insights into cancer treatment related adverse events."),
        tags$a(href = "AExplorer_User_Manual.pdf", "Download User Manual", target = "_blank")
      )
    )
  )
)