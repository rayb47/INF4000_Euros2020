# INF4000_Euros2020

The main goal of this project is to conduct an analysis on the attacking output of the top 8 teams (Quarterfinalists) at the 2020 Euros, and identify whether the team that possessed the best attack went on to win the tournament.

This question is relevant since evaluating the attacking output of football teams provides key insights into their performances, approaches, and the relation it has with success across a tournament. While goals are the end product of attacking success, the underlying metrics of possession, attempts, and directional play provide a more nuanced understanding of offensive effectiveness in the sport.

## Composite Visualisation

![image](https://github.com/user-attachments/assets/fcc73329-68bb-433b-b2b5-17588e42b557)



## Code
`main.R`

```R
# Loads functions from functions.R file
source("functions.R")

# Lists out the required libraries
required_libraries <- c(
  "dplyr", "ggplot2", "ggrepel", "tibble", "tidyr", 
  "reshape2", "randomForest", "corrplot", "factoextra", 
  "stringr", "ggtext"
)

# Check and install missing libraries
install_if_missing(required_libraries)

# Load all libraries
load_libraries(required_libraries)

# Loads data from CSV files
match_events <- read.csv("Match events.csv")
match_information <- read.csv("Match information.csv")
match_team_statistics <- read.csv("Match team statistics.csv")

# Define stats of interest
stats_of_interest <- c(
  "Goals", "Goals conceded", "Ball Possession", "Corners", "Total Attempts", "Attempts on target", 
  "Tackles won", "Clearances", "Recovered balls", "Fouls committed",
  "Passes completed", "Passes accuracy", "Attempts Accuracy", "Blocks",
  "Lost balls", "Big Chances", "Instance of possession ", "Change of possession",
  "Yellow cards", "Total Attacks", "Attacks from left", "Attacks from centre", "Attacks from right",
  "Goals in open play from left", "Goals in open play from centre", "Goals in open play from right"
)

# Create a single vector for numeric and N/A replacement
numeric_columns <- stats_of_interest

# Process data
detailed_match_team_stats <- match_team_statistics %>%
  filter(StatsName %in% stats_of_interest) %>%
  pivot_wider(names_from = StatsName, values_from = Value) %>%
  left_join(match_information, by = "MatchID") %>%
  mutate(
    Stage = if_else(RoundName == "final tournament", "Group Stage", "Knockouts"),
    across(all_of(numeric_columns), ~ as.numeric(.x)),
    across(all_of(numeric_columns), ~ replace_na(.x, 0))
  )

# Summarize match statistics
detailed_match_team_stats <- detailed_match_team_stats %>%
  group_by(MatchID, TeamName) %>%
  summarize(across(all_of(numeric_columns), ~ sum(.x, na.rm = TRUE), .names = "{.col}"), .groups = "drop") %>%
  left_join(
    detailed_match_team_stats %>%
      select(MatchID, HomeTeamName.x, AwayTeamName.x, RoundName, Stage) %>%
      distinct(),
    by = "MatchID"
  )

# Summarize team tournament stats
team_tournament_stats <- detailed_match_team_stats %>%
  group_by(TeamName) %>%
  summarize(
    # Compute averages for listed columns
    across(
      all_of(numeric_columns), 
      ~ mean(.x, na.rm = TRUE), 
      .names = "{str_replace_all(.col, ' ', '')}"
    ),
    # Compute specific custom calculations
    avg_fouls_per_yellow_card = ifelse(mean(`Yellow cards`, na.rm = TRUE) == 0, NA, mean(`Fouls committed`, na.rm = TRUE) / mean(`Yellow cards`, na.rm = TRUE)),
    avg_inst_poss = mean(`Instance of possession `, na.rm = TRUE),
    avg_change_poss = mean(`Change of possession`, na.rm = TRUE),
    goals_in_op_left = sum(`Goals in open play from left`, na.rm = TRUE),
    goals_in_op_centre = sum(`Goals in open play from centre`, na.rm = TRUE),
    goals_in_op_right = sum(`Goals in open play from right`, na.rm = TRUE),
    .groups = "drop"
  )

# Top 8 Teams in the Tournament
top_8_teams <- c("Belgium", "Italy", "Switzerland", "Spain", "England", "Ukraine", "Czech Republic", "Denmark")

# --------- Stacked Bar Chart Creation -------------------
stacked_goal_data <- team_tournament_stats %>%
  filter(TeamName %in% top_8_teams) %>%
  select(TeamName, goals_in_op_left, goals_in_op_centre, goals_in_op_right)

# Melts data for a stacked bar plot
data_melted <- melt(stacked_goal_data, id.vars = "TeamName", 
                    variable.name = "Goal_Position", 
                    value.name = "Goals")

# Function call
create_stacked_bar_chart(
  data = data_melted, 
  x_metric = "TeamName", 
  y_metric = "Goals", 
  fill_metric = "Goal_Position", 
  x_label = "Team Name", 
  y_label = "Number of Goals", 
  fill_label = "Open Play Goals from the", 
  title = "Open Play Goals by Top 8 Teams (across Tournament)",
  colors = c(
    "goals_in_op_left" = "#FF9999", 
    "goals_in_op_centre" = "#99CC99", 
    "goals_in_op_right" = "#9999FF"
  )
)
# -------------------------------------------------

# ------------- Heat Map Creation -----------------------
# Prepares tournament data to be suitable for display in the heatmap
heatmap_attacks_data <- team_tournament_stats %>%
  mutate(across(starts_with("Attacks"), ~ round(.x, 2))) %>%
  filter(TeamName %in% top_8_teams) %>%
  # Reshape the data to long format
  pivot_longer(
    cols = starts_with("Attacks"),
    names_to = "AttackDirection",
    values_to = "AttackCount"
  ) %>%
  mutate(AttackDirection = factor(
    AttackDirection,
    levels = c("Attacksfromleft", "Attacksfromcentre", "Attacksfromright")
  ))

create_heatmap(
  data = heatmap_attacks_data,
  x_metric = "AttackDirection",
  y_metric = "TeamName",
  fill_metric = "AttackCount",
  title = "Heatmap of Attacking Directions (per game) by Top 8 Teams"
)
# ----------------------------------------------------------------

# ------- Dotplot / Scatterplot Creation -----------------
dotplot_data <- team_tournament_stats %>%
  filter(TeamName %in% top_8_teams)

plot_team_comparison(dotplot_data, "AttemptsAccuracy", "TotalAttempts", "TeamName", "Attempts Accuracy (%)", "Total Attempts",
                     "Attempts Accuracy (%) vs Total Attempts (per game) for Top 8 Teams")
plot_team_comparison(dotplot_data, "BallPossession", "Goals", "TeamName", "Ball Possession (%)", "Goals Scored", 
                     "Goals Scored vs Ball Possession (per game) for Top 8 Teams")
# --------------------------------------------------------
```

`functions.R`

```R
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
    colors <- c("goals_in_op_left" = "#FF9999", 
                "goals_in_op_centre" = "#99CC99", 
                "goals_in_op_right" = "#9999FF")
  }
  
  # Create the plot
  ggplot(data, aes(x = .data[[x_metric]], y = .data[[y_metric]], fill = .data[[fill_metric]])) +
    geom_bar(stat = "identity") +
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
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Attacks/Game") +
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
```
