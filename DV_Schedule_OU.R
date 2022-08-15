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
themes <- function () { 
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

x <- read_json('https://cdn.nba.com/static/json/staticData/scheduleLeagueV2_1.json')

x <- x[["leagueSchedule"]][["gameDates"]] %>% 
  tibble() %>%
  unnest_wider(1) %>% 
  unnest_longer(2) %>% 
  unnest_wider(2) %>%
  unnest_wider(homeTeam) %>%
  unnest_wider(awayTeam, names_repair = 'minimal') 

df <- x %>% 
  clean_names() %>% 
  unnest_wider(broadcasters) %>% 
  unnest_longer(nationalTvBroadcasters) %>% 
  unnest_wider(nationalTvBroadcasters) 


# look at national tv games only, select relevant columns
df <- df %>% filter(broadcasterScope == "natl") %>% 
  select(game_date, game_id, broadcasterDisplay, team_name, team_name_2)

# pivot data long 
df <- df %>%
  mutate(teama = team_name,
         teamb = team_name_2) %>% 
  pivot_longer(team_name:team_name_2) %>% 
  mutate(opp = ifelse(value == teama, teamb, teama)) %>% 
  rename(team = value) %>% 
  select(-teama, -teamb, -name) 

# combine data w/ win totals data
df <- left_join(df, win_totals, by = "team") 

df <- df %>% 
  group_by(team) %>% 
  summarise(games = n(), 
            ou = max(ou)) %>% 
  mutate(team = tolower(team)) %>% 
  left_join(., tms, by = c("team" = "teamName")) 

## My code to fix it
df <- df %>%
  filter(!is.na(urlThumbnailTeam))

#make the chart
df %>% 
  ggplot(aes(x = games, y = ou)) + 
  geom_image(aes(image = urlThumbnailTeam), position = position_jitter(width = 0.5, height = 1.5), size = .075, by = "width") +  
  coord_cartesian(clip = 'off') + 
  scale_y_continuous(limits = c(20, 57.5), breaks =  seq(20, 55, 5)) + 
  scale_x_continuous(limits = c(0, 42.5), breaks =  seq(0, 40, 5)) + 
  themes() +  
  theme(plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold'), 
        plot.margin = margin(10, 10, 15, 10)) +
  labs(x = "National TV Games", 
       y = "Vegas Over/Under", 
       title = "Do The Best Teams Play On National TV The Most?",
       subtitle = "Vegas win totals and national TV games for the 2021-22 NBA Season")

ggsave("national_tv_games_over_under.png", p, w = 6, h = 6, dpi = 300, type = 'cairo')
  



