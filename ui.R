library(shiny)
library(shinycssloaders)
library(shinyBS)  # Add this library for tooltips
library(shinyjs)
library(bslib)

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
    tags$link(rel = "stylesheet", href = "custom.css")  # Add your custom CSS here
  ),
  tabPanel(
    title = "Visualizations",
    fluidPage(
      # Custom script to make chord labels wrap correctly
      tags$script(HTML("
        Shiny.addCustomMessageHandler('patchChordLabels', function(message) {
          setTimeout(function() {
            // Select all text elements in the chord diagram
            d3.selectAll('#chordPlot text').each(function() {
              var text = d3.select(this).text();
              if (text.indexOf('<br>') !== -1) {
                var lines = text.split('<br>');
                d3.select(this).text(null);
                for (var i = 0; i < lines.length; i++) {
                  d3.select(this)
                    .append('tspan')
                    .attr('x', 0)
                    .attr('dy', i === 0 ? 0 : '1.2em')
                    .text(lines[i]);
                }
              }
            });
          }, 200); // Wait for rendering
        });
      ")),
      fluidRow(
        column(
          2,
          div(
            class = "sidebar-card shadow-sm rounded p-3 bg-white",
            # Plot type selector
            selectizeInput("plotType", "Select Plot Type", choices = c("Onset", "Sankey", "Volcano", "Chord")),
            # Subset options
            conditionalPanel(
              condition = "input.plotType != 'Chord' && input.plotType != 'Volcano'",
              radioButtons(
                "subsetData",
                "Subset Data?",
                choices = c("No", "Yes"),
                selected = "No"
              ),
              conditionalPanel(
                condition = "input.subsetData == 'Yes'",
                selectizeInput(
                  "filterColumn",
                  "Select Column to Filter",
                  choices = NULL
                ),
                selectizeInput(
                  "filterValues",
                  "Select Values to keep",
                  choices = NULL,
                  multiple = TRUE
                )
              )
            ),
            # Onset plot options
            conditionalPanel(
              condition = "input.plotType == 'Onset'",
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
              )
            ),
            # Sankey plot options
            conditionalPanel(
              condition = "input.plotType == 'Sankey'",
              selectizeInput(
                "sankeyColumns",
                "Select Columns for Sankey Plot",
                choices = c("outc_cod", "AE_Category", "drug_category", "sex"),
                multiple = TRUE
              )
            ),
            # Volcano plot options
            conditionalPanel(
              condition = "input.plotType == 'Volcano'",
              selectizeInput(
                "volcanoTarget",
                "Select Target Column",
                choices = c("drug_class", "cancerType"),
                selected = "drug_class"
              )
            )
          )
        ),
        column(
          10,
          div(
            class = "main-card shadow-sm rounded p-4 bg-white",
            uiOutput("plot_container"),
            conditionalPanel(
              condition = "input.plotType == 'Onset'",
              div(
                style = "text-align: right; margin-top: 10px;",
                tags$i(
                  id = "infoIcon",
                  class = "fa fa-info-circle",
                  style = "cursor: pointer; font-size: 16px;"
                )
              ),
              bsTooltip(
                id = "infoIcon",
                title = "For the onset plot, values greater than 52 weeks are grouped into '>52'."
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