library(tidyverse)
library(janitor)
library(rvest)
library(nbastatR)
library(httr)
library(jsonlite)
library(hablar)
library(grid)
library(ggimage)

## Set connections 
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

#playoffs
url <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=ElbowTouch&Season=2022-23&SeasonSegment=&SeasonType=Playoffs&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))

df <- data.frame(json_resp$resultSets$rowSet)
colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]

# clean variable names and reclassify things
df <- df %>% 
  clean_names() %>% 
  retype() 

df <- df %>%
  arrange(desc(elbow_touches)) %>%
  filter(team_abbreviation %in% c("DEN", "BOS") & 
           min > 5) %>%
  mutate(player_name = replace(player_name, player_name == "Kentavious Caldwell-Pope", "KCP"),
         player_name = replace(player_name, player_name == "Robert Williams III", "Robert Williams")) %>%
  select(player_name, team_abbreviation, elbow_touches)

df2 <- df %>%
  filter(team_abbreviation == "BOS") %>%
  group_by(team_abbreviation) %>%
  summarize(elbow_touches = sum(elbow_touches))

player_name <- as.data.frame("Boston's Whole Team")
df2 <- cbind(player_name, df2)

colnames(df2)[1] <- "player_name"

df1 <- rbind(df, df2)

themes <- function () { 
  theme_minimal(base_size=9, base_family="mono") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}
# count number of unique lineups for each team and make bar plot
p <- df1 %>% 
  ggplot(aes(x=fct_reorder(player_name, elbow_touches), y = elbow_touches, fill = team_abbreviation)) +
  geom_col(alpha = .8) +
  geom_text(aes(label = elbow_touches),
            hjust = 0, nudge_y = .05, family = "mono") +
  themes() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = 'bold', size = 16), 
    plot.title.position = 'plot', 
    plot.margin = margin(10, 10, 15, 10),
    legend.position = "none",
    axis.text.y = element_text(hjust = 1, size = 10),
   axis.text.x = element_text(size = 10)
  ) +
  scale_fill_manual(values = c("#007A33", "#0E2240")) +
  coord_flip() +
  labs(x = "", 
       y = "", 
       title = "Playoff Elbow Touches", 
       subtitle = "No more zone?",
       caption = "Source: nba.com/stats")
p

ggsave("playoffelbowtouches.png", p, height = 6, width = 10, dpi = "retina") 

