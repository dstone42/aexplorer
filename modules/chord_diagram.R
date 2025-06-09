library(networkD3)
library(data.table)

renderChordPlot <- function(data, source_col, target_col, value_col, group_colors = NULL) {

  wrap_label <- function(label, width = 25) {
    # Insert line breaks at spaces, not in the middle of words
    paste(strwrap(label, width = width), collapse = "<br>")
  }

  labels = rownames(data)
  wrapped_labels <- unname(sapply(labels, wrap_label, width = 30))
  rownames(data) <- wrapped_labels
  colnames(data) <- wrapped_labels

  # Prepare group colors if provided
  group_color_vec <- NULL
  if (!is.null(group_colors)) {
    # Match colors to the wrapped labels
    # Remove HTML breaks for matching
    clean_labels <- gsub("<br>", " ", wrapped_labels)
    # Try to match by original names (before wrapping)
    color_names <- names(group_colors)
    # If wrapped labels match color_names, use them directly
    if (all(clean_labels %in% color_names)) {
      group_color_vec <- unname(group_colors[clean_labels])
    } else if (all(labels %in% color_names)) {
      group_color_vec <- unname(group_colors[labels])
    } else {
      # fallback: repeat a default color
      group_color_vec <- rep("#D3D3D3", length(wrapped_labels))
    }
  }

  w <- chordNetwork(
    Data = data,
    width = 900,
    height = 900,
    labels = wrapped_labels,
    labelDistance = 140,
    padding = 0.09,
    fontSize = 13,
    colourScale = group_color_vec
  )
  # str(w$x$options)
  w
}

makeFreqTable <- function(df, subset_column) {
  unique_vals <- unique(unlist(strsplit(df[[subset_column]], ",")))
  freq_table <- matrix(0, nrow = length(unique_vals), ncol = length(unique_vals),
                       dimnames = list(unique_vals, unique_vals))
  for (row in df[[subset_column]]) {
    split <- unlist(strsplit(row, ","))
    if (length(split) > 1) {
      for (i in seq_len(length(split) - 1)) {
        for (j in (i+1):length(split)) {
          freq_table[split[i], split[j]] <- freq_table[split[i], split[j]] + 1
          freq_table[split[j], split[i]] <- freq_table[split[j], split[i]] + 1
        }
      }
    }
  }
  return(freq_table)
}

formatChordTable <- function(freq_table) {
  row_names <- freq_table[[1]]
  freq_table[[1]] <- NULL
  freq_table <- as.matrix(freq_table)
  rownames(freq_table) <- row_names
  return(freq_table)
}