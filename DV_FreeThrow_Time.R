# load packages
library(tidyverse)
library(nbastatR)
library(jsonlite)
library(httr)
library(vroom)
library(extrafont)
library(lubridate)
library(ggfx)
library(ggridges)

# set theme
theme <- function () { 
  theme_minimal(base_size=9) %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}

# set headers h/t Ryan Davis
headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)


url <- "https://cdn.nba.com/static/json/liveData/playbyplay/playbyplay_0042000404.json"

res <- GET(url = url, add_headers(.headers=headers))

json_resp <- fromJSON(content(res, "text"))

df <- data.frame(json_resp[["game"]][["actions"]])

# get game logs from the reg season
game_logs <- game_logs(seasons = 2021, 
                       result_types = 'team', 
                       season_types = "Regular Season")

# Get a list of distinct game ids 
game_ids <- game_logs %>% 
  select(idGame) %>% 
  distinct() 

# create function that gets pbp logs from the 2020-21 season
get_data <- function(id) {
  
  url <- paste0("https://cdn.nba.com/static/json/liveData/playbyplay/playbyplay_00", id, ".json")
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_resp <- fromJSON(content(res, "text"))
  df <- data.frame(json_resp[["game"]][["actions"]])
  
  df$gameid <- id
  
  return(df)
  
}

# get data from all ids (takes awhile)
pbpdat <- map_df(game_ids$idGame, get_data)

# calculate time elapsed between a free throw and whatever action came before it
df <- pbpdat %>%
  arrange(gameid, orderNumber) %>% 
  mutate(dtm = as_datetime(timeActual),
         ptm = lag(dtm),
         elp = dtm-ptm,
         pact = lag(actionType),
         psub = lag(subType), 
         pmake = lag(shotResult)) %>%
  filter(actionType == "freethrow",
         elp > 0) %>%
  select(gameid, clock, actionNumber, orderNumber, subType, pact, psub, dtm, ptm, pmake, elp, personId, playerNameI, shotResult, period)

###

# read in cleaned data from GitHub, if you want 
#df <- vroom("https://raw.githubusercontent.com/Henryjean/data/main/cleanpbplogs2021.csv")

# find average time elapsed between 1st and 2nd (or 2nd and 3rd) FTs when previous action was a FT 

# psub == "offensive" means player missed preceding free throw
df <- df %>% 
  filter((subType == "2 of 2" & (psub == "1 of 2" | psub == "offensive")) | 
           (subType == "2 of 3" & (psub == "1 of 3" | psub == "offensive")) | 
           (subType == "3 of 3" & (psub == "2 of 3" | psub == "offensive"))
  ) %>% 
  group_by(playerNameI, personId) %>% 
  mutate(count = n(), 
         avgtime = mean(elp)) %>%
  filter(count >= 50) %>% 
  ungroup() 

# make chart
df %>% 
  # order players by avgtime elapsed
  ggplot(aes(x = elp, y = fct_reorder(playerNameI, avgtime))) + 
  # add ridgelines with ggfx tweaks
  with_bloom(geom_density_ridges(size = .25, 
                                 color = 'white', 
                                 fill = "#FD625EFF", 
                                 scale = 3, 
                                 alpha = .75, 
                                 bandwidth = .75),
             color = "white",
             sigma = 10,
             keep_alpha = TRUE) +
  # format x-axis
  scale_x_continuous(limits = c(0, 40), 
                     breaks = seq(0, 40, 5)) +
  # turn off clipping
  coord_cartesian(clip = 'off') +
  # add custom theme
  theme() + 
  # make thematic tweaks
  theme(legend.position = 'none', 
        axis.text.y = element_text(size = 6, margin = margin(0,-15,0,0)), 
        plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold')) + 
  # add axis titles, plot title, subtitle, and caption 
  labs(x = "Real Time Elapsed (In Seconds)", 
       y = "", 
       title = "Real Time Elapsed Between Consecutive Free Throw Attempts", 
       subtitle = "Minimum 50 Uninterrupted FTA | 2020 - 2021 Regular Season", 
       caption = "Source: nba.com") 

# save plot
ggsave("RidgeLinePlot.png", w = 6, h = 6, dpi = 300)

