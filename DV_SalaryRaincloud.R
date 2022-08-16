# load packages
library(tidyverse)
library(nbastatR)
library(rvest)
library(extrafont)
library(ggdist)
library(gghalves)
library(scales)

# set theme
themes <- function () { 
  theme_minimal(base_size=9) %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}

# assign URL
url <- "https://www.spotrac.com/nba/rankings/2021-22/base/atlanta-hawks"

# read HTML from the URL
team_page <- url %>% read_html() 

#Hawks example
team_page %>% html_elements('a.team-name') %>% html_text2()

team_page %>% html_elements('span.info') %>% html_text2()

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
#get NBA Teams
teams <- nba_teams() %>% filter(isNonNBATeam == 0)

team_names <- gsub(" ", "-",tolower(teams$nameTeam))

#write function to get salaries for each player on a team
get_data <- function(team) {
  
  # set URL
  url <- paste0("https://www.spotrac.com/nba/rankings/2021-22/base/", team)
  
  # read in URL
  team_page <- url %>% read_html() 
  
  # Create a tibble (dataframe) with player names, salaries, and team name
  df <- tibble(player = team_page %>% html_elements('a.team-name') %>% html_text2(),
               salary = team_page %>% html_elements('span.info') %>% html_text2() %>% parse_number(),
               team = team)
  
  return(df)
  
}

get_data("charlotte_hornets")

# get data for all 30 teams
salary_dat <- map_df(team_names, get_data)

# save data
write.csv(salary_dat, "/Users/prestonstevenson/Documents/nba_salaries_2022.csv", row.names = FALSE)

# read in data 
df <- read.csv("/Users/prestonstevenson/Documents/nba_salaries_2022.csv")

# calculate salaries as a percentage of the 2021-22 salary cap
df$cap_pct <- df$salary / 112414000

# Rain cloud plot for the 2021-2022 season 
df %>% 
  ggplot(aes(y = cap_pct)) + 
  # Use the ggidst package to draw distribution 
  ggdist::stat_halfeye(alpha = .55,  adjust = .5, width = .75, .width = 0, point_interval = NULL, fill = "#5D69B1FF") + 
  # Use the gghalves package to draw jittered points 
  gghalves::geom_half_point(side = "l", size = 2, range_scale = .75, alpha = .333, shape = 21, fill = "#5D69B1FF", color = "#5D69B1FF") + 
  # flip plot
  coord_flip() +
  # format axis
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), breaks = seq(0, .45, .05)) +
  # add theme 
  themes() + 
  # add title, subtile, caption 
  labs(y = "Percent of the salary cap", 
       x = "", 
       title = "The Distribution Of NBA Annual Salaries As A Percentage Of The Salary Cap", 
       subtitle = "2021 - 2022 Season", 
       caption = "Source: spotrac.com") + 
  #add dead zone
  geom_hline(yintercept = .2, linetype = 'dashed') +
  geom_hline(yintercept = .25, linetype = 'dashed') +
  annotate(geom = 'text', x = 0.25, y = .225, label = "Dead Zone", family = 'Consolas', size = 2.75) +
  # theme tweaks 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold', size = 10), 
        plot.margin = margin(10, 10, 15, 10), 
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank())
