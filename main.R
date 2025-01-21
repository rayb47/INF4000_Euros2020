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
    "goals_in_op_left" = "#fc8d59", 
    "goals_in_op_centre" = "#ffffbf", 
    "goals_in_op_right" = "#91bfdb"
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
