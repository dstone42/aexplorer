library(ggplot2)
library(ggrepel)
library(ggiraph)
library(DT)
library(htmltools)

#' Volcano Plot Module UI
#' @param id Namespace id
volcanoPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(girafeOutput(ns("volcanoPlot"))),
    # Caption
    tags$p(
      style = "font-style: italic; font-size: 12px; color: #555; margin-top: 8px;",
      "Shows odds (ROR or PRR) of reporting an adverse event (AE) and its statistical significance (adjusted p-value) for selected column.
      Points on the top right indicate increased risk for the AE in the context of the selected target.
      Points on the top left indicate decreased risk for the AE in the context of the selected target."
    ),
    tags$div(
      style = "margin:6px 0 10px 0;",
      downloadButton(ns("download_plot"), "Download Plot")
    ),
    tags$hr(style = "margin:12px 0;"),
    tags$h5("Significant Adverse Events"),
    DTOutput(ns("significant_table")),
    tags$div(
      style = "margin:6px 0 4px 0;",
      downloadButton(ns("download_table"), "Download Table (CSV)")
    ),
    tags$p(
      style = "font-style: italic; font-size: 11px; color: #555; margin-top: 6px;",
      "Includes AEs with adjusted p < 0.05 and |log2(ROR/PRR)| ≥ 1."
    )
  )
}

#' Volcano Plot Module Server
#' @param id Namespace id
#' @param data Reactive data frame with required columns
#' @param target_col Name of the column for target/group (string)
#' @param plot_title Title for the plot (string)
volcanoPlotServer <- function(id, data, target_col, measure, plot_title = "Volcano Plot") {
  moduleServer(id, function(input, output, session) {

    processedVolcanoData <- reactive({
      df <- data()
      req(df)
      measure <- measure()
      req(measure %in% names(df))
      df[[measure]] <- as.numeric(df[[measure]])
      df$`adjusted p-value` <- as.numeric(df$`adjusted p-value`)
      df$`-log10(adjusted p-value)` <- -log10(df$`adjusted p-value`)
      df$log2_value <- log2(df[[measure]])
      df <- df[is.finite(df$log2_value) & is.finite(df$`-log10(adjusted p-value)`), ]
      pvalueThreshold <- -log10(0.05)
      df$colorColumn <- ifelse(
        df$`-log10(adjusted p-value)` < pvalueThreshold | (df$log2_value < 1 & df$log2_value > -1),
        "Insignificant",
        ifelse(df$log2_value > 0, "Increased Risk", "Decreased Risk")
      )
      # Set size to 1 for Insignificant, keep 'a' for others
      df$`Number of Cases` <- ifelse(df$colorColumn == "Insignificant", 1, df$a)
      list(df = df, measure = measure, p_thresh = pvalueThreshold)
    })

    output$volcanoPlot <- renderGirafe({
      proc <- processedVolcanoData()
      df <- proc$df
      measure <- proc$measure
      pvalueThreshold <- proc$p_thresh
      mycolors <- c("Decreased Risk" = "#0071c5", "Increased Risk" = "#CC0000", "Insignificant" = "grey")

      df$label_text <- paste0(df[[target_col()]], " & ", df$AE)

      # Add a text column for tooltips
      df$tooltip <- paste0(
        df$label_text, "<br>",
        "log2(", measure, "): ", round(df$log2_value, 2), "<br>",
        "-log10(p): ", round(df$`-log10(adjusted p-value)`, 2), "<br>",
        "Cases: ", df$`Number of Cases`
      )

      p <- ggplot(df, aes(
        x = log2_value,
        y = `-log10(adjusted p-value)`,
        color = colorColumn,
        size = `Number of Cases`
      )) +
        geom_point_interactive(aes(tooltip = tooltip), alpha = 0.5) +
        scale_color_manual(values = mycolors) +
        scale_size_continuous(
          name = "Number of Cases",
          range = c(1, 9),
          breaks = if (max(df$`Number of Cases`, na.rm = TRUE) > 50)
            seq(from = 50, to = max(df$`Number of Cases`, na.rm = TRUE),
                by = max(1, as.integer(max(df$`Number of Cases`, na.rm = TRUE) / 6))) else NULL
        ) +
        geom_vline(xintercept = c(-1, 1), col = "pink", linetype = "dashed") +
        geom_hline(yintercept = pvalueThreshold, col = "pink", linetype = "dashed") +
        theme_minimal() +
        labs(
          x = paste0("log2(", measure, ")"),
          y = "-log10(adjusted p-value)",
          color = "Risk"
        ) +
        guides(
          color = guide_legend(order = 1),
          size = guide_legend(order = 2)
        ) +
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

    output$significant_table <- renderDT({
      proc <- processedVolcanoData()
      df <- proc$df
      measure <- proc$measure
      lower_col <- paste0(measure, " lower bound")
      upper_col <- paste0(measure, " upper bound")

      sig <- df[df$colorColumn != "Insignificant", , drop = FALSE]
      if (nrow(sig) == 0) {
        return(datatable(
          data.frame(Message = "No significant adverse events found.", stringsAsFactors = FALSE),
          rownames = FALSE,
          options = list(pageLength = 5, dom = 't')
        ))
      }

      # Check for CI columns
      if (!all(c(lower_col, upper_col) %in% names(sig))) {
        missing_cols <- setdiff(c(lower_col, upper_col), names(sig))
        return(datatable(
          data.frame(Warning = paste("Missing CI columns:", paste(missing_cols, collapse = ", ")), stringsAsFactors = FALSE),
          rownames = FALSE,
          options = list(pageLength = 5, dom = 't')
        ))
      }

      sig[[lower_col]] <- as.numeric(sig[[lower_col]])
      sig[[upper_col]] <- as.numeric(sig[[upper_col]])

      # Determine target column label
      target_label <- switch(
        target_col(),
        "drug_category" = "Drug Category",
        "cancer_type" = "Cancer Type",
        target_col()
      )

      tbl <- data.frame(
        AE = sig$AE,
        Target = sig[[target_col()]],
        value = sig[[measure]],
        CI = sprintf("(%.2f - %.2f)", sig[[lower_col]], sig[[upper_col]]),
        `Adjusted p-value` = sig$`adjusted p-value`,
        Cases = sig$a,
        stringsAsFactors = FALSE
      )
      colnames(tbl)[2] <- target_label
      colnames(tbl)[3] <- measure
      colnames(tbl)[4] <- "95% CI"
      colnames(tbl)[5] <- "Adjusted p-value"
      tbl <- tbl[order(-tbl[[measure]], tbl$AE), , drop = FALSE]

      datatable(
        tbl,
        rownames = FALSE,
        options = list(
          pageLength = 25,
          lengthChange = FALSE,
          scrollX = TRUE,
          dom = 'lfrtip',
          order = list(list(2, 'desc')),
          columnDefs = list(
            list(targets = 0, width = '20%'),
            list(targets = 1, width = '20%'),
            list(targets = 3, orderable = FALSE),
            list(
              targets = 4,
              render = JS(
                "function(data, type, row, meta) {",
                "  if(type === 'display'){",
                "    var num = parseFloat(data);",
                "    if(isNaN(num)) return data;",
                "    return num.toExponential(2);",
                "  }",
                "  return data;",
                "}"
              )
            )
          )
        )
      ) %>% 
        formatRound(columns = measure, digits = 2)
    })

    output$download_table <- downloadHandler(
      filename = function() {
        paste0("significant_AEs_", tolower(target_col()), "_", tolower(measure()), ".csv")
      },
      content = function(file) {
        proc <- processedVolcanoData()
        df <- proc$df
        measure <- proc$measure
        lower_col <- paste0(measure, " lower bound")
        upper_col <- paste0(measure, " upper bound")
        sig <- df[df$colorColumn != "Insignificant", , drop = FALSE]
        if (!all(c(lower_col, upper_col) %in% names(sig)) || nrow(sig) == 0) {
          write.csv(data.frame(Message = "No significant adverse events found."), file, row.names = FALSE)
        } else {
          # Build table using only existing columns
          tbl <- data.frame(
            AE = sig$AE,
            Target = sig[[target_col()]],
            value = sig[[measure]],
            CI = sprintf("(%.2f - %.2f)", as.numeric(sig[[lower_col]]), as.numeric(sig[[upper_col]])),
            `Adjusted p-value` = sig$`adjusted p-value`,
            Cases = sig$a,
            stringsAsFactors = FALSE
          )
          # Rename columns for clarity
          colnames(tbl)[2] <- switch(
            target_col(),
            "drug_category" = "Drug Category",
            "cancer_type" = "Cancer Type",
            target_col()
          )
          colnames(tbl)[3] <- measure
          colnames(tbl)[4] <- "95% CI"
          colnames(tbl)[5] <- "Adjusted p-value"
          write.csv(tbl, file, row.names = FALSE)
        }
      }
    )

    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("volcano_plot_", tolower(target_col()), "_", tolower(measure()), ".png")
      },
      content = function(file) {
        proc <- processedVolcanoData()
        df <- proc$df
        measure <- proc$measure
        pvalueThreshold <- proc$p_thresh
        mycolors <- c("Decreased Risk" = "#0071c5", "Increased Risk" = "#CC0000", "Insignificant" = "grey")
        p <- ggplot(df, aes(
          x = log2_value,
          y = `-log10(adjusted p-value)`,
          color = colorColumn,
          size = `Number of Cases`
        )) +
          geom_point(alpha = 0.5) +
          scale_color_manual(values = mycolors) +
          scale_size_continuous(
            name = "Number of Cases",
            range = c(1, 9),
            breaks = if (max(df$`Number of Cases`, na.rm = TRUE) > 50)
              seq(from = 50, to = max(df$`Number of Cases`, na.rm = TRUE),
                  by = max(1, as.integer(max(df$`Number of Cases`, na.rm = TRUE) / 6))) else NULL
          ) +
          geom_vline(xintercept = c(-1, 1), col = "pink", linetype = "dashed") +
          geom_hline(yintercept = pvalueThreshold, col = "pink", linetype = "dashed") +
          theme_minimal() +
          labs(
            x = paste0("log2(", measure, ")"),
            y = "-log10(adjusted p-value)",
            color = "Risk"
          ) +
          theme(
            axis.title = element_text(size = 15),
            legend.text = element_text(size = 13),
            legend.title = element_text(size = 15),
            axis.text = element_text(size = 10)
          )
        ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
      }
    )
  })
}

