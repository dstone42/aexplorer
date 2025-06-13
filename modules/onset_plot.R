library(ggplot2)
library(data.table)

renderOnsetPlot <- function(data, input, output, session) {
  plot_data <- data[!is.na(time_to_onset)]
  
  if (input$facetVarRow != "None" || input$facetVarCol != "None") {
    # Filter and order data based on selected facets
    if (!is.null(input$facetVarRow) && input$facetVarRow != "None") {
      plot_data <- plot_data[!is.na(get(input$facetVarRow)) & get(input$facetVarRow) != ""]
      medians_row <- plot_data[, .(median_time = median(time_to_onset, na.rm = TRUE)), by = eval(input$facetVarRow)]
      order_of_medians_row <- medians_row[order(medians_row$median_time)][[input$facetVarRow]]
      plot_data[[input$facetVarRow]] <- factor(plot_data[[input$facetVarRow]], levels = order_of_medians_row)
      medians_row[[input$facetVarRow]] <- factor(medians_row[[input$facetVarRow]], levels = order_of_medians_row)
    } else {
      medians_row <- NULL
    }

    if (!is.null(input$facetVarCol) && input$facetVarCol != "None") {
      plot_data <- plot_data[!is.na(get(input$facetVarCol)) & get(input$facetVarCol) != ""]
      medians_col <- plot_data[, .(median_time = median(time_to_onset, na.rm = TRUE)), by = eval(input$facetVarCol)]
      order_of_medians_col <- medians_col[order(medians_col$median_time)][[input$facetVarCol]]
      plot_data[[input$facetVarCol]] <- factor(plot_data[[input$facetVarCol]], levels = order_of_medians_col)
      medians_col[[input$facetVarCol]] <- factor(medians_col[[input$facetVarCol]], levels = order_of_medians_col)
    } else {
      medians_col <- NULL
    }
    
    plot <- ggplot(plot_data, aes(x = time_to_onset)) +
      geom_density(aes(fill = if (input$facetVarRow != "None") get(input$facetVarRow) else get(input$facetVarCol), 
                      color = if (input$facetVarRow != "None") get(input$facetVarRow) else get(input$facetVarCol)), alpha = 0.25) +
      scale_x_continuous(limits = c(0, 52), breaks = c(seq(0, 40, by = 10), 52), labels = c(seq(0, 40, by = 10), ">52")) +
      labs(title = "Onset of Adverse Events", x = "Time to Onset (Weeks)") +
      theme_minimal() +
      theme(
        title = element_text(size = 16, family = "DejaVu Sans"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 16, family = "DejaVu Sans"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),
        legend.position = "none",
        plot.margin = margin(t = 20, r = 20, b = 50, l = 20)
      )

    # Apply palettes
    if (input$facetVarRow == "AE_Category") {
      plot <- plot + scale_fill_manual(values = ae_category_palette) +
                    scale_color_manual(values = alpha(ae_category_palette, 1))
    } else if (input$facetVarRow == "outc_cod" || input$facetVarCol == "outc_cod") {
      plot <- plot + scale_fill_manual(values = outcomes_palette) +
                    scale_color_manual(values = alpha(outcomes_palette, 1))
    } else if (input$facetVarRow == "drug_category") {
      plot <- plot + scale_fill_manual(values = drug_category_palette) +
                    scale_color_manual(values = alpha(drug_category_palette, 1))
    } else if (input$facetVarCol == "AE_Category") {
      plot <- plot + scale_fill_manual(values = ae_category_palette) +
                    scale_color_manual(values = alpha(ae_category_palette, 1))
    } else if (input$facetVarCol == "outc_cod" || input$facetVarCol == "outc_cod") {
      plot <- plot + scale_fill_manual(values = outcomes_palette) +
                    scale_color_manual(values = alpha(outcomes_palette, 1))
    } else if (input$facetVarCol == "drug_category") {
      plot <- plot + scale_fill_manual(values = drug_category_palette) +
                    scale_color_manual(values = alpha(drug_category_palette, 1))
    } 

    # Prepare row and column facets
    row_facet <- if (input$facetVarRow != "None") vars(factor(get(input$facetVarRow), levels = order_of_medians_row)) else NULL
    col_facet <- if (input$facetVarCol != "None") vars(factor(get(input$facetVarCol))) else NULL

    plot <- plot + facet_grid(
      rows = if (!is.null(row_facet)) row_facet else NULL, 
      cols = if (!is.null(col_facet)) col_facet else NULL, 
      scales = "free_y",
      switch = "y"
      ) +
      theme(
        strip.text = element_text(size = 14), # Increase facet label size
        strip.text.y.left = element_text(angle = 0) # Rotate row labels to horizontal
      )

    # Calculate the maximum y-axis limit for each facet to set y-level for labels
    # Extract density data from ggplot object
    plot_obj <- ggplot_build(plot)
    plot_data_density <- plot_obj$data[[1]]
    plot_data_density <- as.data.table(plot_data_density)
    
    # Get the facet mapping from the plot object and select PANEL column and column 4 if we're only faceting by one variable and column 4 & 5 if we're faceting by two variables
    facet_mapping <- plot_obj$layout$layout
    if (input$facetVarRow != "None" && input$facetVarCol != "None") {
      facet_mapping <- facet_mapping[, c(1, 4, 5)]
      setnames(facet_mapping, c("PANEL", "facet_row", "facet_col"))
    } else {
      facet_mapping <- facet_mapping[, c(1, 4)]
      setnames(facet_mapping, c("PANEL", "facet"))
    }

    facet_mapping <- as.data.table(facet_mapping)
    facet_mapping[, PANEL := as.factor(PANEL)] # Convert PANEL to factor to match plot_data_density
    max_densities <- plot_data_density[, .(max_density = max(density, na.rm = TRUE)), by = PANEL] # Calculate maximum density for each PANEL
    max_densities <- merge(max_densities, facet_mapping, by = "PANEL") # Merge max_densities with facet_mapping to get the faceting variable values

    # Merge max_densities with medians
    grouping_columns <- list()
    if (input$facetVarRow != "None" && input$facetVarCol != "None") {
      medians <- plot_data[, .(
        median_time = median(time_to_onset, na.rm = TRUE)
      ), by = .(get(input$facetVarRow), get(input$facetVarCol))]
      medians <- merge(medians, max_densities, by.x = c("get", "get.1"), by.y = c("facet_row", "facet_col"))
      colnames(medians) <- c(input$facetVarRow, input$facetVarCol, 'median_time', 'PANEL', 'max_density')
    } else if (input$facetVarRow != "None") {
      medians <- plot_data[, .(
        median_time = median(time_to_onset, na.rm = TRUE)
      ), by = .(get(input$facetVarRow))]
      medians <- merge(medians, max_densities, by.x = c("get"), by.y = c("facet"))
      colnames(medians) <- c(input$facetVarRow, 'median_time', 'PANEL', 'max_density')
    } else if (input$facetVarCol != "None") {
      medians <- plot_data[, .(
        median_time = median(time_to_onset, na.rm = TRUE)
      ), by = .(get(input$facetVarCol))]
      medians <- merge(medians, max_densities, by.x = c("get"), by.y = c("facet"))
      colnames(medians) <- c(input$facetVarCol, 'median_time', 'PANEL', 'max_density')
    }
    
    medians[, y_text := max_density * 0.9] # Set y_text for label placement


    # Add dashed vertical lines for medians
    plot <- plot + geom_vline(
        data = medians,
        aes(xintercept = median_time, color = if (input$facetVarRow == "None") "grey" else get(input$facetVarRow)),
        linetype = "dashed"
    ) +
    geom_text(
        data = medians,
        aes(x = median_time, y = y_text, label = round(median_time, 1)),
        color = "black",
        hjust = -0.5,
        size = 4
    )

    # Add sample size labels
    # Get n from plot_data_density
    case_counts <- unique(plot_data_density[, c("n", "PANEL")])
    medians <- merge(medians, case_counts, by = "PANEL")
    plot <- plot +
        geom_text(
        data = medians,
        aes(x = 52, y = y_text, label = paste0("n = ", n)),
        hjust = 1.2,
        size = 4,
        fontface = "plain"
        )
    
    return(plot)
  } else {
    # Non-faceted density plot logic
    medians <- data.table(median_time = median(plot_data$time_to_onset, na.rm = TRUE))
    plot <- ggplot(plot_data, aes(x = time_to_onset)) +
      geom_density(aes(fill = "grey", color = "grey"), alpha = 0.25) +
      scale_x_continuous(limits = c(0, 52), breaks = c(seq(0, 52, by = 10), 52), labels = c(seq(0, 50, by = 10), ">52")) +
      labs(title = "Onset of Adverse Events", x = "Time to Onset (Weeks)") +
      theme_minimal() +
      theme(
        title = element_text(size = 16, family = "DejaVu Sans"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 16, family = "DejaVu Sans"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),
        legend.position = "none",
        plot.margin = margin(t = 20, r = 20, b = 50, l = 20)
      )

    # Apply palettes
    plot <- plot + scale_fill_manual(values = c("grey")) +
                    scale_color_manual(values = alpha(c("grey"), 1))

    # Extract density data from ggplot object
    plot_data_density <- ggplot_build(plot)$data[[1]]

    # Convert to data.table
    plot_data_density <- as.data.table(plot_data_density)

    # Calculate maximum density for the entire dataset to set y-level for labels
    max_density <- max(plot_data_density$density)
    medians[, y_text := max_density * 0.9]

    # Add dashed vertical lines for medians
    plot <- plot + geom_vline(
        data = medians,
        aes(xintercept = median_time, color = if (input$facetVarRow == "None") "grey" else get(input$facetVarRow)),
        linetype = "dashed"
    ) +
    geom_text(
        data = medians,
        aes(x = median_time, y = y_text, label = round(median_time, 1)),
        color = "black",
        hjust = -0.5,
        size = 4
    )

    # Add sample size labels
    total_cases <- plot_data[, .N]
    # Make table with total cases and max density
    table <- data.table(
        total_cases = total_cases,
        max_density = max_density
    )
    plot <- plot +
        geom_text(
        data = table,
        aes(x = 52, y = max_density * 0.9, label = paste0("n = ", total_cases)),
        hjust = 1.2,
        size = 4,
        fontface = "plain"
        ) +
        theme(
        text = element_text(face = "plain")
        )

    return(plot)
  }
}