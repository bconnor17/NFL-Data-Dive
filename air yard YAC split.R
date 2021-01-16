# Playing with air yards/YAC splits

# Package load
library(caret)
library(DBI)
library(DMwR)
library(ggrepel)
library(leaps)
library(lme4)
library(MASS)
library(nflfastR)
library(RSQLite)
library(tidyverse)
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Connect to database
update_db()

connection <- dbConnect(SQLite(), "./pbp_db")
connection

pbp_db <- tbl(connection, "nflfastR_pbp")

# Load in 2020 data (pass plays)
pass_plays_2020 <- pbp_db %>%
  filter(play_type == "pass",
         season == 2020)

run_plays_2020 <- pbp_db %>%
  filter(play_type == "run",
         season == 2020)

pass_plays_2020 <- as.data.frame(pass_plays_2020)

run_plays_2020 <- as.data.frame(run_plays_2020)

# Passing plays by team
pass_plays_2020_byteam <- pass_plays_2020 %>%
  group_by(posteam) %>%
  summarize(dropbacks = n(),
            avg_pass_epa = mean(epa, na.rm = T),
            avg_airyards = mean(air_yards, na.rm = T),
            avg_yac = mean(yards_after_catch, na.rm = T),
            avg_yards_gained_pass = mean(yards_gained, na.rm = T),
            avg_ypa = mean(yards_gained[!is.na(air_yards)])) %>%
  mutate(avg_total_pass = avg_airyards + avg_yac,
         airyard_pct = avg_airyards/avg_total_pass,
         yac_pct = avg_yac/avg_total_pass) %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

run_plays_2020_byteam <- run_plays_2020 %>%
  group_by(posteam) %>%
  summarize(carries = n(),
            avg_run_epa = mean(epa, na.rm = T),
            avg_yards_run = mean(yards_gained, na.rm = T))

plays_2020_byteam = left_join(pass_plays_2020_byteam, run_plays_2020_byteam,
                              by = "posteam")
  
# Running effiency vs. passing efficiency
run_pass_plot <- ggplot(data = plays_2020_byteam, aes(x = avg_run_epa, y = avg_pass_epa)) +
  geom_hline(yintercept = mean(plays_2020_byteam$avg_pass_epa),
             color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(plays_2020_byteam$avg_run_epa),
             color = "red", linetype = "dashed", alpha = 0.5) +
  geom_point(color = plays_2020_byteam$team_color,
             cex = plays_2020_byteam$carries/75,
             alpha = 0.6) +
  geom_text_repel(aes(label = posteam)) +
  stat_smooth(geom = "line", alpha = 0.5, se = FALSE, method = "lm") +
  labs(x = "Average EPA per Rushing Attempt",
       y = "Average EPA per Passing Attempt",
       title = "Passing Efficiency vs. Rushing Efficiency, 2020") +
  theme_bw()

run_pass_plot

# Yards/Dropback vs. EPA
yd_epa_plot <- ggplot(data = plays_2020_byteam, aes(x = avg_yards_gained_pass, y = avg_pass_epa)) +
  geom_hline(yintercept = mean(plays_2020_byteam$avg_pass_epa),
             color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(plays_2020_byteam$avg_yards_gained_pass),
             color = "red", linetype = "dashed", alpha = 0.5) +
  geom_point(color = plays_2020_byteam$team_color,
             cex = plays_2020_byteam$dropbacks/60,
             alpha = 0.6) +
  geom_text_repel(aes(label = posteam)) +
  stat_smooth(geom = "line", alpha = 0.5, se = FALSE, method = "lm") +
  labs(x = "Yards per Dropback",
       y = "Average EPA per Passing Attempt",
       title = "Yards/Dropback vs. Passing EPA, 2020") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

yd_epa_plot
