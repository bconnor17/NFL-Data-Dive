# EPA and CPOE improvements

# Package load
library(caret)
library(data.table)
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

# Pulling out passing stats
## 2019 - 2020
passing_19_20 <- pbp_db %>%
  filter(season_type == "REG",
         season == 2020 | season == 2019,
         pass_attempt == 1) 

passing_19_20 <- as.data.frame(passing_19_20) %>%
  mutate(complete_pass = as.factor(complete_pass),
         posteam_type = as.factor(posteam_type),
         shotgun = as.factor(shotgun),
         no_huddle = as.factor(no_huddle),
         pass_location = as.factor(pass_location),
         defteam = as.factor(defteam))

def_strength <- passing_19_20 %>%
  group_by(defteam, season) %>%
  summarize(dropbacks = n(),
            def_strength = -mean(epa, na.rm = T))

passing_19_20 <- passing_19_20 %>%
  left_join(def_strength, by = c("defteam", "season"))

# CPOE model
cpoe_glm <- glm(complete_pass ~ posteam_type + shotgun + no_huddle +
                  pass_location  + air_yards + def_strength,
                family = "binomial",
                data = passing_19_20)

cpoe_pred <- as.data.frame(predict(cpoe_glm, passing_19_20))
