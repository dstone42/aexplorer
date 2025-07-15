
library(plotly)

overlapHeatmapUI <- function(id) {
    ns <- NS(id)
    tagList(
        withSpinner(plotlyOutput(ns("overlap_heatmap"), height = "800px")),
        # Caption
        tags$p(
            style = "font-style: italic; font-size: 12px; color: #555; margin-top: 8px;",
            "Shows the overlap coefficient between selected variables."
        )
    )
}

overlapHeatmapServer <- function(id, overlap_data, cluster) {
    moduleServer(id, function(input, output, session) {
        output$overlap_heatmap <- renderPlotly({
            req(overlap_data())
            mat <- overlap_data()
            # Cluster if requested
            if (isTruthy(cluster())) {
                # Compute distance and hierarchical clustering
                dist_mat <- as.dist(1 - mat)
                hc <- hclust(dist_mat)
                ord <- hc$order
                mat <- mat[ord, ord]
            }
            # Mask upper triangle
            mat[upper.tri(mat, diag = TRUE)] <- NA
            plot_ly(
                z = mat,
                x = colnames(mat),
                y = rownames(mat),
                type = "heatmap",
                colorscale = list(c(0, "white"), c(1, "#F40009")),
                zmin = 0, zmax = 0.5,
                hovertemplate = paste(
                    "<b>Row:</b> %{y}<br>",
                    "<b>Col:</b> %{x}<br>",
                    "<b>Overlap:</b> %{z:.2f}<extra></extra>"
                ),
                showscale = TRUE
            ) %>%
            layout(
                xaxis = list(title = "", tickangle = -45),
                yaxis = list(title = ""),
                margin = list(l = 100, b = 100)
            )
        })

        output$download_heatmap <- downloadHandler(
            filename = function() "overlap_heatmap.png",
            content = function(file) {
                # Save as static image
                orca::orca(
                    plotly_build(output$overlap_heatmap()), 
                    file = file
                )
            }
        )
    })
}