# load packages
library(tidyverse)
library(teamcolors)
library(nbastatR)
library(jsonlite)
library(extrafont)
library(janitor)
library(ggimage)
library(zoo)
library(paletteer)

# custom ggplot2 theme
theme <- function () { 
  theme_minimal(base_size=9) %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

# get team colors from the teamcolors package
teamcolors <- teamcolors::teamcolors %>% filter(league == "nba") 

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

# get team names and logos
tms <- nba_teams()
tms <- tms %>% filter(isNonNBATeam == 0) %>% select(teamName, urlThumbnailTeam)

# adjust these team names to help our joins later on 
tms <- tms %>% 
  mutate(teamName = case_when(
    teamName == "sixers" ~ "76ers", 
    teamName == "blazers" ~ "trail blazers", 
    TRUE ~ teamName
  ))

# data frame of win totals
win_totals <- data.frame(
  team = c("Hawks", "Celtics", "Nets", "Hornets", "Bulls", "Cavaliers", "Mavericks", "Nuggets", "Pistons", 
           "Warriors", "Rockets", "Pacers", "Clippers", "Lakers", "Grizzlies", "Heat", "Bucks", "Timberwolves", 
           "Pelicans", "Knicks", "Thunder", "Magic", "76ers", "Suns", "Trail Blazers", "Kings", "Spurs", "Raptors", "Jazz", "Wizards"), 
  ou = c(46.5, 46.5, 54.5, 36.5, 41.5, 25.5, 47.5, 47.5, 26.5, 49.5, 24.5, 41.5, 45.5, 51.5, 40.5, 46.5, 53.5, 32.5, 39.5, 42.5, 22.5, 24.5, 
         51.5, 50.5, 44.5, 35.5, 29.5, 35.5, 51.5, 32.5))
