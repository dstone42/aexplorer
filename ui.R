library(shiny)
library(shinycssloaders)
library(shinyBS)  # Add this library for tooltips
library(shinyjs)
library(bslib)
library(sortable)

# Use a modern Bootstrap theme
custom_theme <- bs_theme(
  version = 5,
  bootswatch = "cosmo",
  base_font = font_google("Inter"),
  heading_font = font_google("Montserrat"),
  primary = "#2C3E50",
  secondary = "#18BC9C"
)

# Define the UI
ui <- navbarPage(
  title = div(
    # tags$span(class = "fa fa-dna", style = "margin-right: 10px; color: #18BC9C;"),
    "Cancer Treatment Adverse Event Explorer"
  ),
  theme = custom_theme,
  id = "main_navbar",
  header = tags$head(
    # tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    tags$link(rel = "stylesheet", href = "custom.css"),  # Add your custom CSS here
    tags$script(src = "https://d3js.org/d3.v6.min.js"),
    tags$script(src = "patchChordLabels.js"),  # Custom JS for chord label wrapping
    tags$script(src = "patchChordTooltips.js")  # Custom JS for chord
  ),
  tabPanel(
    title = "Data Exploration",
    fluidPage(
      tabsetPanel(
        id = "exploreTab",
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
                  choices = c("None", "outc_cod", "AE_Category", "drug_category", "sex"),
                  selected = "None"
                ),
                selectizeInput(
                  "facetVarCol",
                  "Facet By (Columns)",
                  choices = c("None", "outc_cod", "AE_Category", "drug_category", "sex"),
                  selected = "None"
                ),
                # Subset options
                radioButtons(
                  "onsetSubsetData",
                  "Subset Data?",
                  choices = c("No", "Yes"),
                  selected = "No"
                ),
                conditionalPanel(
                  condition = "input.onsetSubsetData == 'Yes'",
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
                    labels = c("AE_Category", "sex", "drug_category", "outc_cod"), # not selected by default
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
        # --- Volcano Tab ---
        tabPanel("Volcano",
          fluidRow(
            column(
              2,
              div(
                class = "sidebar-card shadow-sm rounded p-3 bg-white",
                # Plot options
                selectizeInput(
                  "volcanoTarget",
                  "Select Target Column",
                  choices = c("drug_category", "cancer_type"),
                  selected = "drug_category"
                )
              )
            ),
            column(
              10,
              div(
                class = "main-card shadow-sm rounded p-4 bg-white",
                uiOutput("volcano_plot_container"),
                downloadButton("download_volcano_plot", "Download Plot")
              )
            )
          )
        ),
        # --- Chord Tab ---
        tabPanel("Chord",
          fluidRow(
            column(
              2,
              div(
                class = "sidebar-card shadow-sm rounded p-3 bg-white",
                # Plot options
                selectizeInput(
                  "chordColumn",
                  "Select Column for Chord Diagram",
                  choices = c("AECategory", "Drug Category", "Cancer Type"),
                  selected = "AECategory"
                )
              )
            ),
            column(
              10,
              div(
                class = "main-card shadow-sm rounded p-4 bg-white",
                uiOutput("chord_plot_container"),
                downloadButton("download_chord_plot_html", "Download Plot as HTML"),
                downloadButton("download_chord_plot_png", "Download Plot as PNG")
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
                We have processed and cleaned the data to provide meaningful insights into cancer treatment related adverse events.")
      )
    )
  )
)