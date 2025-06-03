library(networkD3)
library(data.table)

renderSankeyPlot <- function(data, input, output, session) {

    # If only one column is selected, show a message saying to select at least two columns
    if (length(input$sankeyColumns) < 2) {
    return(tags$div(
        class = "alert alert-warning",
        "Please select at least two columns for the Sankey plot."
    ))
    } else {
        # Filter data to include only selected columns
        sankey_data <- data[, eval(input$sankeyColumns), with = FALSE]
        # browser()
        # Create nodes and links for Sankey plot
        nodes <- unique(unlist(sankey_data))
        # Remove NA values and replace spaces with underscores
        nodes <- nodes[nodes != ""]
        nodes <- gsub(" ", "_", nodes)
        nodes <- data.table(name = nodes)
        nodes[, id := .I - 1]

        links <- rbindlist(lapply(1:(ncol(sankey_data) - 1), function(i) {
            from_to <- sankey_data[, .(source = .SD[[i]], target = .SD[[i + 1]])]
            from_to[, .(value = .N), by = .(source, target)]
        }))

        # Remove empty strings and replace spaces with underscores
        links <- links[source != "" & target != ""]
        links[, source := gsub(" ", "_", source)]
        links[, target := gsub(" ", "_", target)]

        links <- merge(links, nodes, by.x = "source", by.y = "name", all.x = TRUE)
        setnames(links, "id", "source_id")
        links <- merge(links, nodes, by.x = "target", by.y = "name", all.x = TRUE)
        setnames(links, "id", "target_id")

        links <- links[, .(source = source_id, target = target_id, value, group = source)]

        # Assign each node its originating column
        nodes[, source_column := NA_character_]

        for (col in input$sankeyColumns) {
            col_values <- sankey_data[[col]]
            col_values <- gsub(" ", "_", col_values)
            nodes[is.na(source_column) & name %in% col_values, source_column := col]

        }
        
        # Determine color palette for nodes and links
        column_palettes <- list(
            "outc_cod" = if (exists("outcomes_palette", envir = .GlobalEnv)) outcomes_palette else NULL,
            "AE_Category" = if (exists("ae_category_palette", envir = .GlobalEnv)) ae_category_palette else NULL,
            "drug_category" = if (exists("drug_category_palette", envir = .GlobalEnv)) drug_category_palette else NULL
        )

        # Replace spaces in column_palettes with underscores
        column_palettes <- lapply(column_palettes, function(palette) {
            names(palette) <- gsub(" ", "_", names(palette))
            return(palette)
        })

        # Generate a combined color palette for all nodes
        node_palette <- setNames(rep(NA, length(nodes$name)), nodes$name)  # Initialize with NA
        for (i in seq_len(nrow(nodes))) {
            node <- nodes$name[i]
            col  <- nodes$source_column[i]

            if (!is.null(column_palettes[[col]])) {
                palette <- column_palettes[[col]]
                if (node %in% names(palette)) {
                node_palette[node] <- palette[[node]]
                }
            } else {
                # Fallback to dynamically generated colors for values without predefined palettes
                if (is.na(node_palette[node])) {
                    # Generate consistent dynamic palette per column
                    if (!exists(paste0("dynamic_palette_", col))) {
                        unique_values <- unique(sankey_data[[col]])
                        assign(paste0("dynamic_palette_", col),
                            setNames(grDevices::rainbow(length(unique_values)), unique_values),
                            envir = .GlobalEnv)
                    }
                    palette <- get(paste0("dynamic_palette_", col), envir = .GlobalEnv)
                    if (node %in% names(palette)) {
                        node_palette[node] <- palette[[node]]
                    }
                }
            }
        }


        # Ensure all nodes have a color (fallback to a default color if needed)
        node_palette[is.na(node_palette)] <- "#D3D3D3"  # Light gray for unmapped nodes

        # Convert the palette to a JavaScript-compatible format
        node_colors <- paste0(
            "d3.scaleOrdinal().domain([",
            paste0('"', names(node_palette), '"', collapse = ","),
            "]).range([",
            paste0('"', unlist(node_palette), '"', collapse = ","),
            "])"
        )
        

        # Create Sankey plot
        sankey <- sankeyNetwork(
            Links = links,
            Nodes = nodes,
            Source = "source",
            Target = "target",
            Value = "value",
            NodeID = "name",
            fontSize = 14,          # Increase font size for better readability
            nodeWidth = 40,         # Adjust node width
            colourScale = node_colors, # Apply node colors
            LinkGroup = "group"        # Use custom link colors
        )

        # Add custom JavaScript to replace underscores with spaces in node labels
        sankey <- htmlwidgets::onRender(
            sankey,
            "
            function(el, x) {
                // Select all node labels and replace underscores with spaces
                d3.select(el).selectAll('.node text')
                    .text(function(d) {
                        return d.name.replace(/_/g, ' ');
                    });
            }
            "
        )

        return(sankey)
    }
  
}