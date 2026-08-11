# Install and load devtools library and tidyverse
if (!requireNamespace("tidyverse", quietly = TRUE))install.packages("tidyverse")
if (!requireNamespace("remotes", quietly = TRUE))install.packages("remotes", type = "binary")
library(tidyverse)
library(readr)


# Install and load StatsBombR and SBpitch libraries from GitHub
remotes::install_github("statsbomb/StatsBombR")

if (!requireNamespace("ggsoccer", quietly = TRUE))install.packages("ggsoccer")


library(StatsBombR)
library(ggsoccer)


# Read all free competitions and matches from StatsBombR library
Comp <- FreeCompetitions()
Matches <- FreeMatches(Comp)
Matches


# Review matches class, dimensions, column names, first rows,
class(Matches)
dim(Matches)
colnames(Matches)
glimpse(Matches)
head(Matches)

#Convert match_date from character to date format
Matches_Corrected <- Matches %>%
  mutate(match_date = as.Date(match_date))

Matches_Corrected

# Print the distinct seasons in the data
Matches %>% distinct(season.season_name) %>% print(n=Inf)

#Print the distinct competitions in the data
comp_names <- distinct(Matches, competition.competition_name, competition.competition_id) 
print(comp_names, n=Inf)

#print available Arsenal seasons in the data
Matches %>%
  filter(
    (home_team.home_team_name == "Arsenal" | away_team.away_team_name == "Arsenal"),
     home_team.home_team_gender == "male", 
     competition.competition_name == "Premier League"
     ) %>%
    View()
  
# Filter by Arsenal's Invincibles Season
arsenal_games_2023_2024_premier <- Matches %>%
  filter(
    (home_team.home_team_name == "Arsenal" | away_team.away_team_name == "Arsenal"), 
    home_team.home_team_gender == "male", 
    season.season_name == "2003/2004", 
    competition.competition_id == 2)

# Verify filtering is ok, should be 38 games
count(arsenal_games_2023_2024_premier)

# View the data
View(arsenal_games_2023_2024_premier)


# Download event data
events <- arsenal_games_2023_2024_premier %>% 
  as.data.frame() %>%
  split(1:nrow(.)) %>%
  map_dfr(~ get.matchFree(.x))

# Save the events data frame to working directory
write_rds(events, "arsenal_events_2023_2024.rds")

# Apply allclean() to events, run ?allclean in the console for more information
events <- StatsBombR::allclean(events)
View(events)

# Filter a single match
arsenal_vs_liverpool <- filter(events, match_id == 3749448)
View(arsenal_vs_liverpool)

# Count the frequency of event types
arsenal_vs_liverpool %>%
  count(type.name, sort = TRUE) %>%
  head(20)


# Create a table to visualize passing accuracy by team
pass_table <- arsenal_vs_liverpool %>% 
  filter(type.name == "Pass") %>%
  group_by(team.name) %>%
  summarize(Attempted = n(), 
            Completed = sum(is.na(pass.outcome.name)),
            Accuracy = Completed / Attempted)

pass_table
  
# Create a table to visualize match shots and xG by team
shots_table <- arsenal_vs_liverpool %>%
  filter(type.name == "Shot") %>%
  group_by(team.name) %>%
  summarize(Shots = n(),
            Goals = sum(shot.outcome.name == "Goal", na.rm = TRUE),
            xg = round(sum(shot.statsbomb_xg, na.rm = TRUE),2),
            xg_per_shot = round(xg / Shots, 3))

shots_table
  

# Visualize xg 
library(ggplot2)

# Create xg histogram
arsenal_shots <- filter(arsenal_vs_liverpool, type.name == "Shot", team.name == "Arsenal")
nrow(arsenal_shots)

hist <- ggplot(arsenal_shots, aes(x = shot.statsbomb_xg)) +
  geom_histogram(bins = 25,
                 fill = "blue",
                 color = "white") +
  labs(title = "Arsenal shot quality (xG)",
       x = "Shot xG",
       y = "Count") +
  theme_minimal()

hist



# Visualize soccer field, shot map
arsenal_shot_map <-  ggplot(arsenal_shots) +
    annotate_pitch(
      dimensions = pitch_statsbomb,
      fill = "white",
      colour = "gray60"
    ) +
    coord_cartesian(
      xlim = c(60,122),
      ylim = c(-2, 82)
    ) +
    geom_point(
      aes(
        x = location.x,
        y = location.y,
        size = shot.statsbomb_xg,
        color = shot.outcome.name,
        shape = shot.body_part.name
      ),
      alpha = 0.8,
      stroke = 1.2
    ) +
    scale_size_area(max_size = 10, limits = c(0,1)) +
    scale_shape_manual(
      values = c("Right Foot" = 16, "Left Foot" = 1, "Head" = 17, "Other" = 15)
      ) +
    theme_pitch()
  
arsenal_shot_map






# Visualize a passing graph


# 1. Filter arsenal successful passes
arsenal_successful_passes <- arsenal_vs_liverpool %>%
  filter(
    type.name == "Pass",
    team.name == "Arsenal",
    is.na(pass.outcome.name)
  )

# 2. Calculate average player positions
arsenal_player_positions <- arsenal_successful_passes %>%
  group_by(player.name) %>%
  summarise(
    avg_x = mean(location.x, na.rm = TRUE),
    avg_y = mean(location.y, na.rm = TRUE),
    num_passes = n()
  )

arsenal_player_positions

# 3. Calculate pass frequencies between players

min_passes <- 3 #Use this to filter out low-frequency passing connections

arsenal_pass_between <- arsenal_successful_passes %>%
  filter(!is.na(pass.recipient.name)) %>%
  group_by(player.name, pass.recipient.name) %>%
  summarise(
    pass_count = n()
  ) %>%
  filter(pass_count >= min_passes) %>%
  left_join(
    arsenal_player_positions %>% select(player.name, avg_x, avg_y), 
    by = "player.name"
    ) %>%
  left_join(
    arsenal_player_positions %>% 
      select(player.name, avg_x_end = avg_x, avg_y_end = avg_y),
    by = c("pass.recipient.name" = "player.name"))

arsenal_pass_between

#Create a passing network visual
arsenal_passing_network <- ggplot() +
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "gray60",
    fill = "white"
  ) +
  scale_y_reverse(limits = c(82, -2)) +
  
  coord_cartesian(
    xlim = c(-2, 122)
    ) +
  theme_pitch() +
  geom_segment(
    data = arsenal_pass_between,
    aes(
      x = avg_x,
      y = avg_y,
      xend = avg_x_end,
      yend = avg_y_end,
      linewidth = pass_count,
      alpha = pass_count
      ),
    color = "red"
  ) +
  geom_point(
    data = arsenal_player_positions,
    aes(
      x = avg_x,
      y = avg_y,
      size = num_passes
    ),
    color = "red",
    stroke = 1.5
  ) +
  geom_text(
    data = arsenal_player_positions,
    aes(
      x = avg_x,
      y = avg_y - 2,
      label = player.name
    ),
    size = 3
  ) +
  scale_linewidth_continuous(range = c(0.5, 2.5)) +
  scale_size_continuous(range = c(4, 10))  +
  scale_alpha_continuous(range = c(0.3, 0.9)) +
  labs(
    title = "Arsenal Passing Network",
    linewidth = "Pass Volume",
    size = "Total Passes"
  )

#Display the passing network
arsenal_passing_network



# Create a stats table by player

player_successful_passes <- unique(arsenal_successful_passes$player.name)
player_successful_passes

#create an empty list
player_summaries <- list()

#Loop through players that executed a succesful pass
for (player in player_successful_passes) {
  player_data <- arsenal_successful_passes %>%
    filter(player.name == player)
  
  stats <- tibble(
    player_name = player,
    total_passes = nrow(player_data),
    avg_x_pos = mean(player_data$location.x, na.rm = TRUE),
    avg_y_pos = mean(player_data$location.y, na.rm = TRUE)
  )
  
  player_summaries[[player]] <- stats
}

#Join the list into a table
arsenal_player_stats <- bind_rows(player_summaries)
arsenal_player_stats

# Write table to csv
write.csv(arsenal_player_stats, "arsenal_player_stats_summary.csv")



