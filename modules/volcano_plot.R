library(ggplot2)
library(ggrepel)
library(ggiraph)

#' Volcano Plot Module UI
#' @param id Namespace id
volcanoPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(girafeOutput(ns("volcanoPlot"))),
    # Caption
    tags$p(
      style = "font-style: italic; font-size: 12px; color: #555; margin-top: 8px;",
      "Shows odds (ROR) of reporting an adverse event (AE) and its statistical significance (adjusted p-value) for selected column.
      Points on the top right indicate increased risk for the AE in the context of the selected target.
      Points on the top left indicate decreased risk for the AE in the context of the selected target."
    )
  )
}

#' Volcano Plot Module Server
#' @param id Namespace id
#' @param data Reactive data frame with required columns
#' @param target_col Name of the column for target/group (string)
#' @param plot_title Title for the plot (string)
volcanoPlotServer <- function(id, data, target_col, plot_title = "Volcano Plot") {
  moduleServer(id, function(input, output, session) {
    output$volcanoPlot <- renderGirafe({
      df <- data()
      req(df)
      print(df)
      # Ensure numeric
      df$ROR <- as.numeric(df$ROR)
      df$`adjusted p-value` <- as.numeric(df$`adjusted p-value`)
      df$`-log10(adjusted p-value)` <- -log10(df$`adjusted p-value`)
      df$`log2(ROR)` <- log2(df$ROR)
      # Remove infs
      df <- df[is.finite(df$`log2(ROR)`) & is.finite(df$`-log10(adjusted p-value)`), ]
      # Color logic
      pvalueThreshold <- -log10(0.05)
      df$colorColumn <- ifelse(
        df$`-log10(adjusted p-value)` < pvalueThreshold | (df$`log2(ROR)` < 1 & df$`log2(ROR)` > -1),
        "Insignificant",
        ifelse(df$`log2(ROR)` > 0, "Increased Risk", "Decreased Risk")
      )
      # Set size to 1 for Insignificant, keep 'a' for others
      df$`Number of Cases` <- ifelse(df$colorColumn == "Insignificant", 1, df$a)
      mycolors <- c("Decreased Risk" = "#0071c5", "Increased Risk" = "#CC0000", "Insignificant" = "grey")
      # browser()
      # Size logic
      # max_significant_size <- max(df[df$colorColumn != "Insignificant"]$a)
      # Plot

      df$label_text <- paste0(df[[target_col()]], " & ", df$AE)

      # Add a text column for tooltips
      df$tooltip <- paste0(
        df$label_text, "<br>",
        "log2(ROR): ", round(df$`log2(ROR)`, 2), "<br>",
        "-log10(p): ", round(df$`-log10(adjusted p-value)`, 2), "<br>",
        "Cases: ", df$`Number of Cases`
      )

      p <- ggplot(df, aes(
        x = `log2(ROR)`,
        y = `-log10(adjusted p-value)`,
        color = colorColumn,
        size = `Number of Cases`
      )) +
        geom_point_interactive(aes(tooltip = tooltip), alpha = 0.5) +
        scale_color_manual(values = mycolors) +
        scale_size_continuous(range = c(1, 9), breaks = seq(from = 50, to = max(df$`Number of Cases`), by = as.integer(max(df$`Number of Cases`) / 6))) +
        geom_vline(xintercept = c(-1, 1), col = "pink", linetype = "dashed") +
        geom_hline(yintercept = pvalueThreshold, col = "pink", linetype = "dashed") +
        theme_minimal() +
        labs(color = "Risk") +
        theme(
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 15),
          axis.text = element_text(size = 10)
        )

      girafe(ggobj = p, width_svg = 10, height_svg = 6, options = list(
        opts_toolbar(saveaspng = FALSE)
      ))

    })
  })
}

# Standalone function to generate volcano ggplot for download
volcanoPlotObject <- function(df, target_col, plot_title = "Volcano Plot") {
  req(df)
  df$ROR <- as.numeric(df$ROR)
  df$`adjusted p-value` <- as.numeric(df$`adjusted p-value`)
  df$`-log10(adjusted p-value)` <- -log10(df$`adjusted p-value`)
  df$`log2(ROR)` <- log2(df$ROR)
  df <- df[is.finite(df$`log2(ROR)`) & is.finite(df$`-log10(adjusted p-value)`), ]
  pvalueThreshold <- -log10(0.05)
  df$colorColumn <- ifelse(
    df$`-log10(adjusted p-value)` < pvalueThreshold | (df$`log2(ROR)` < 1 & df$`log2(ROR)` > -1),
    "Insignificant",
    ifelse(df$`log2(ROR)` > 0, "Increased Risk", "Decreased Risk")
  )
  df$`Number of Cases` <- ifelse(df$colorColumn == "Insignificant", 1, df$a)
  mycolors <- c("Decreased Risk" = "#0071c5", "Increased Risk" = "#CC0000", "Insignificant" = "grey")
  ggplot(df, aes(
    x = `log2(ROR)`,
    y = `-log10(adjusted p-value)`,
    color = colorColumn,
    size = `Number of Cases`,
    label = ifelse(colorColumn != "Insignificant", paste0(df[[target_col]], " &\n", df$AE), "")
  )) +
    geom_point(alpha = 0.5) +
    scale_color_manual(values = mycolors) +
    scale_size_continuous(range = c(1, 9), breaks = seq(from = 50, to = max(df$`Number of Cases`), by = as.integer(max(df$`Number of Cases`) / 6))) +
    geom_vline(xintercept = c(-1, 1), col = "pink", linetype = "dashed") +
    geom_hline(yintercept = pvalueThreshold, col = "pink", linetype = "dashed") +
    ggrepel::geom_text_repel(size = 5, show.legend = FALSE) +
    theme_minimal() +
    labs(color = "Risk") +
    theme(
      axis.title = element_text(size = 15),
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 15),
      axis.text = element_text(size = 10)
    )
}