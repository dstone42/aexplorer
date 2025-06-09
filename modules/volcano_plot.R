library(ggplot2)
library(ggrepel)

#' Volcano Plot Module UI
#' @param id Namespace id
volcanoPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("volcanoPlot"), height = "1000px")
  )
}

#' Volcano Plot Module Server
#' @param id Namespace id
#' @param data Reactive data frame with required columns
#' @param target_col Name of the column for target/group (string)
#' @param plot_title Title for the plot (string)
volcanoPlotServer <- function(id, data, target_col, plot_title = "Volcano Plot") {
  moduleServer(id, function(input, output, session) {
    output$volcanoPlot <- renderPlot({
      df <- data()
      req(df)
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
        geom_text_repel(size = 5, show.legend = FALSE) +
        theme_minimal() +
        labs(color = "Risk") +
        theme(
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 15),
          axis.text = element_text(size = 10)
        )
    }, height = 800)
  })
}