# FUNCTION to check and install missing libraries
install_if_missing <- function(libs) {
  for (lib in libs) {
    if (!require(lib, character.only = TRUE)) {
      install.packages(lib, dependencies = TRUE)
      library(lib, character.only = TRUE)
    }
  }
}

# FUNCTION to load the installed libraries
load_libraries <- function(libs) {
  for (lib in libs) {
    library(lib, character.only = TRUE)
  }
}

# FUNCTION to create a stacker bar chart
create_stacked_bar_chart <- function(
    data, x_metric, y_metric, fill_metric, 
    x_label = "X-Axis", y_label = "Y-Axis", fill_label = "Fill Metric", 
    title = "Stacked Bar Chart", colors = NULL
) {
  library(ggplot2)
  
  # Set default colors if none are provided
  if (is.null(colors)) {
    colors <- c("goals_in_op_left" = "#fc8d59", 
                "goals_in_op_centre" = "#ffffbf", 
                "goals_in_op_right" = "#91bfdb")
  }
  
  # Create the plot
  ggplot(data, aes(x = .data[[x_metric]], y = .data[[y_metric]], fill = .data[[fill_metric]])) +
    geom_bar(stat = "identity", color = "black") +  # Added 'color = "black"' for borders
    geom_text(
      aes(label = ifelse(.data[[y_metric]] > 0, .data[[y_metric]], "")),
      position = position_stack(vjust = 0.5), 
      size = 5,
      color = "black",
      fontface = "bold"
    ) +
    labs(
      x = x_label, 
      y = y_label, 
      fill = fill_label,
      title = title
    ) +
    scale_fill_manual(
      values = colors,
      labels = c("goals_in_op_left" = "Left", 
                 "goals_in_op_centre" = "Centre", 
                 "goals_in_op_right" = "Right")
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold"), 
      axis.title.y = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
    )
}


# FUNCTION to plot a heatmap 
create_heatmap <- function(data, x_metric, y_metric, fill_metric, title = "Heatmap") {
  library(ggplot2)
  
  # Check if metrics exist in the dataset
  if (!all(c(x_metric, y_metric, fill_metric) %in% colnames(data))) {
    stop("One or more metrics do not exist in the dataset.")
  }
  
  # Create the heatmap
  ggplot(data, aes(x = .data[[x_metric]], y = .data[[y_metric]], fill = .data[[fill_metric]])) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(.data[[fill_metric]], 2)), color = "yellow2", fontface = "bold", size = 6) +
    scale_fill_gradient(low = "#ffeda0", high = "#f03b20", name = "Attacks/Game") +
    scale_x_discrete(
      labels = c("Attacksfromleft" = "Left", "Attacksfromcentre" = "Centre", "Attacksfromright" = "Right")
    ) +
    labs(
      title = title,
      x = "Attack Direction",
      y = y_metric
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold", angle = 0, hjust = 1, size = 10),
      axis.text.y = element_text(face = "bold", size = 10),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.title.y = element_text(face = "bold", size = 12),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
}


# FUNCTION to plot a dot-plot / scatterplot for attacking metrics
plot_team_comparison <- function(
    data, 
    x_metric, 
    y_metric, 
    label_metric = "TeamName", 
    x_label = NULL, 
    y_label = NULL, 
    title = NULL
) {
  # Calculate medians for the x and y metrics
  x_median <- median(data[[x_metric]], na.rm = TRUE)
  y_median <- median(data[[y_metric]], na.rm = TRUE)
  
  # Use custom labels if provided, otherwise default to the metric names
  x_label <- ifelse(is.null(x_label), x_metric, x_label)
  y_label <- ifelse(is.null(y_label), y_metric, y_label)
  
  # Use custom title if provided, otherwise create a default title
  title <- ifelse(is.null(title), paste("Comparison of Teams Based on", x_metric, "and", y_metric), title)
  
  # Create the plot
  ggplot(data, aes_string(x = x_metric, y = y_metric, label = label_metric)) +
    # Add shaded quadrants
    annotate("rect", xmin = x_median, xmax = Inf, ymin = y_median, ymax = Inf, fill = "lightgreen", alpha = 0.2) + 
    annotate("rect", xmin = -Inf, xmax = x_median, ymin = -Inf, ymax = y_median, fill = "lightcoral", alpha = 0.2) + 
    annotate("rect", xmin = -Inf, xmax = x_median, ymin = y_median, ymax = Inf, fill = "yellow3", alpha = 0.2) + 
    annotate("rect", xmin = x_median, xmax = Inf, ymin = -Inf, ymax = y_median, fill = "yellow3", alpha = 0.2) + 
    # Points
    geom_point(size = 3, color = "blue", alpha = 0.7) +
    # Improved labels with ggrepel
    geom_text_repel(
      aes(label = !!sym(label_metric)),
      size = 3.5,
      fontface = "bold",
      box.padding = 0.3,
      point.padding = 0.3,
      max.overlaps = Inf
    ) +
    # Median lines
    geom_vline(xintercept = x_median, linetype = "dashed", color = "orange") +
    geom_hline(yintercept = y_median, linetype = "dashed", color = "orange") +
    # Labels and theme
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.title.y = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 10)
    )
}

