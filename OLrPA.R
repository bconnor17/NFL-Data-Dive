# NFL offensive line metrics
# Offensive line rushing points added

# Package load
library(caret)
library(DBI)
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

# Data cleaning

## All 2020 plays
plays_2020 <- pbp_db %>%
  filter(season == 2020)

plays_2020.df <- as.data.frame(plays_2020)

plays_2020.df <- plays_2020.df %>%
  mutate(posteam_type = as.factor(posteam_type),
         shotgun = as.factor(shotgun),
         no_huddle = as.factor(no_huddle),
         pass_location = as.factor(pass_location)) %>%
  select(posteam, defteam, season, play_id, desc, passer, passer_id, receiver,
         receiver_id, rusher, rusher_id, posteam_type, shotgun, no_huddle,
         pass_location, air_yards, play_type, run_location, run_gap, side_of_field,
         yardline_100, half_seconds_remaining, game_seconds_remaining, game_half,
         goal_to_go, qb_scramble, yards_after_catch,down, ydstogo, interception,
         qb_hit, sack, touchdown, fumble, roof, surface, temp, wind, epa)

plays_2020.df <- decode_player_ids(plays_2020.df)

## 2020 rosters
rosters_2020 <- fast_scraper_roster(seasons = "2020")

rosters_2020 <- rosters_2020 %>%
  select(position, gsis_id)

rosters_2020_pass <- rosters_2020 %>%
  rename(passer_position = position)

rosters_2020_rec <- rosters_2020 %>%
  rename(rec_position = position)

rosters_2020_rush <- rosters_2020 %>%
  rename(rush_position = position)

# Merge

## Passing plays
plays_2020_pass <- plays_2020.df %>%
  filter(!is.na(receiver_id)) %>%
  left_join(rosters_2020_pass, by = c('passer_id' = 'gsis_id')) %>%
  left_join(rosters_2020_rec, by = c('receiver_id' = 'gsis_id')) %>%
  mutate(rush_position = NA)

## Rushing plays
plays_2020_rush <- plays_2020.df %>%
  filter(!is.na(rusher_id)) %>%
  mutate(passer_position = NA,
         rec_position = NA) %>%
  left_join(rosters_2020_rush, by = c('rusher_id' = 'gsis_id'))

## Combine
plays_2020_all <- rbind(plays_2020_pass,plays_2020_rush) %>%
  left_join(team_epa, by = "posteam") %>%
  mutate(rec_position = as.factor(rec_position),
         rush_position = as.factor(rush_position),
         passer_id = as.factor(passer_id),
         rusher_id = as.factor(rusher_id),
         receiver_id = as.factor(receiver_id),
         location_gap = ifelse(play_type == "run",
                               paste(run_location, run_gap), NA),
         location_gap = ifelse(run_location == "middle", "middle",
                               location_gap),
         team_location_gap = ifelse(play_type == "run",
                                    paste(posteam, location_gap), NA))

plays_2020_all$rec_position <- relevel(plays_2020_all$rec_position, ref = "WR")
plays_2020_all$rush_position <- relevel(plays_2020_all$rush_position, ref = "RB")
plays_2020_all$location_gap <- as.factor(plays_2020_all$location_gap)
plays_2020_all$team_location_gap <- as.factor(plays_2020_all$team_location_gap)

# Model sets
plays_2020_pass_model <- plays_2020_all %>%
  filter(play_type == "pass")

plays_2020_run_model <- plays_2020_all %>%
  filter(play_type == "run")

# Offensive line model set
plays_2020_run_model.final <- plays_2020_run_model %>%
  dplyr::select(posteam_type, rusher_id, shotgun, yardline_100,
                game_seconds_remaining, goal_to_go, ydstogo,
                touchdown, no_huddle, side_of_field, half_seconds_remaining,
                game_half, fumble, rush_position, team_location_gap, down, epa) %>%
  filter(!is.na(epa),
         !is.na(rush_position),
         !is.na(down))

# Stepwise selection test
OL.full.model <- lmerTest::lmer(epa ~ posteam_type  + shotgun + yardline_100 + down+
                        game_seconds_remaining + goal_to_go + ydstogo + touchdown +
                        no_huddle  + half_seconds_remaining +
                        game_half + fumble + rush_position + (1|team_location_gap) +
                        (1|rusher_id), data = plays_2020_run_model.final)

OL.model <- lmerTest::step(OL.full.model)
  
summary(OL.model)

## Lets see the variables
OL.model$fixed
OL.model$random

# Try it another way
set.seed(123)

## Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)

## Train the model
OL.model.2 <- train(epa ~ posteam_type  + shotgun + yardline_100 + down+
                      game_seconds_remaining + goal_to_go + ydstogo + touchdown +
                      no_huddle  + half_seconds_remaining +
                      game_half + fumble + rush_position,
                    data = plays_2020_run_model.final,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:7),
                    trControl = train.control
)

OL.model.2$results

OL.model.2$bestTune

summary(OL.model.2$finalModel)

### Recommendations are largely the same - but the k-fold cross validated model
### does not recommend keeping posteam_type, ydstogo, and shotgun.


# Final model - calculate OLrPAs for 2020!
OLrPA_model <- lmerTest::lmer(epa ~ posteam_type + shotgun + goal_to_go +
                                ydstogo + touchdown + fumble + rush_position +
                                (1|team_location_gap) + (1|rusher_id),
                              data = plays_2020_run_model.final)

OLrPA <- as.data.frame(coef(OLrPA_model)[2])%>%
  rownames_to_column(var = "team_location_gap")%>%
  dplyr::select(team_location_gap, team_location_gap..Intercept.) %>%
  arrange(desc(team_location_gap..Intercept.))
