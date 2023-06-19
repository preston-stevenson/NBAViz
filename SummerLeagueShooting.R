# load packages
library(tidyverse)
library(rvest)
library(extrafont)
library(scales)
library(janitor)
library(prismatic)
library(paletteer)
library(ggpmisc)

# set theme
theme_preston <- function () { 
  theme_minimal(base_family="AppleGothic") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}

# write function to get summer league player totals
get_summer_league_data <- function(year, page_number) {
  
  # set URL
  url <- paste0("https://basketball.realgm.com/nba/stats/", year, "/Totals/All/minutes/All/desc/", page_number, "/Summer_League?rookies=")
  
  # read in URL
  page <- url %>% read_html() 
  
  # Create a tibble (dataframe)
  df <- page %>% 
    html_nodes('table') %>%
    html_table() %>%
    pluck(3) %>% 
    as_tibble()
  
  df$year <- year
  
  return(df)
  
}
# list of years
ys <- rep(seq(2013, 2023, 1), each = 4)

# list of page numbers
ps <- rep(seq(1, 4, 1), 11)

# get summer league data
df_summer_league <- map2_df(ys, ps, get_summer_league_data)

# clean up summer league data, pivot long
df_summer <- df_summer_league %>% 
  clean_names() %>% 
  arrange(player, year) %>% 
  group_by(player) %>% 
  slice(1:1) %>% 
  ungroup() %>% 
  select(player, min, gp, fga, fg_percent, x3pa, x3p_percent, fta, ft_percent, tov, pf, orb, drb, reb, ast, stl, blk, pts, year) %>% 
  mutate(player = as.factor(player)) %>% 
  mutate_if(is.character,as.numeric) %>% 
  filter(min >= 50) %>% 
  mutate(mpg = min / gp, 
         fga_36 = (fga / min) * 36, 
         x3pa_36 = (x3pa / min) * 36, 
         fta_36 = (fta / min) * 36, 
         tov_36 = (tov / min) * 36, 
         pf_36 = (pf / min) * 36, 
         orb_36 = (orb / min) * 36, 
         drb_36 = (drb / min) * 36, 
         ast_36 = (ast / min) * 36, 
         stl_36 = (stl / min) * 36, 
         blk_36 = (blk / min) * 36, 
         pts_36 = (pts / min) * 36,
         type = "summer") #%>% 
  # select(player, mpg, ends_with('_36'), ends_with("_percent")) %>% 
  #pivot_longer(-player, names_to = 'stat', values_to = 'summer') 

# write function to get reg season player totals
get_reg_season_data <- function(year, p) {
  
  # set URL
  url <- paste0("https://basketball.realgm.com/nba/stats/", year, "/Totals/All/minutes/All/desc/1/Regular_Season?rookies=")
  
  # read in URL
  page <- url %>% read_html() 
  
  # Create a tibble (dataframe)
  df <- page %>% 
    html_nodes('table') %>%
    html_table() %>%
    pluck(3) %>% 
    as_tibble()
  
  df$year <- year
  
  return(df)
  
}


# list of years
ys <- seq(2021, 2023, 1)

ps <- rep(1,3)

# get rookie season data
df_reg_season7 <- map2_df(ys, ps, get_reg_season_data)

df_reg_season <- rbind(df_reg_season, df_reg_season2,
                       df_reg_season3, df_reg_season4,
                       df_reg_season5, df_reg_season6,
                       df_reg_season7)

# clean up rookie season data, pivot long
df_reg <- df_reg_season %>% 
  clean_names() %>% 
  arrange(player, year) %>% 
  group_by(player) %>% 
  slice(1:1) %>%
  ungroup() %>%
  select(player, min, gp, fga, fg_percent, x3pa, x3p_percent, fta, ft_percent, tov, pf, orb, drb, reb, ast, stl, blk, pts, year) %>%
  mutate(player = as.factor(player)) %>%
  mutate_if(is.character,as.numeric) %>%
  filter(min >= 250) %>%
  mutate(mpg = min / gp,
         fga_36 = (fga / min) * 36,
         x3pa_36 = (x3pa / min) * 36,
         fta_36 = (fta / min) * 36,
         tov_36 = (tov / min) * 36,
         pf_36 = (pf / min) * 36,
         orb_36 = (orb / min) * 36,
         drb_36 = (drb / min) * 36,
         ast_36 = (ast / min) * 36,
         stl_36 = (stl / min) * 36,
         blk_36 = (blk / min) * 36,
         pts_36 = (pts / min) * 36,
         type = "season") #%>%
  #select(player, mpg, ends_with("percent")) %>% # ends_with('_36'),
  #pivot_longer(-player, names_to = 'stat', values_to = 'reg_season')

# combine summer league and rookie season data 
# df <- left_join(df_summer, df_reg, by = c("player", "stat"))
# df <- df %>%
#   drop_na()

# df1 <- df %>%
#   filter(stat == "year") %>%
#   select(player, summer) %>%
#   rename(year = summer)
  
#df <- left_join(df, df1, by = c("player"))

df <- rbind(df_summer, df_reg)

# write.csv(df, "~/rookie_LVSL_Reg_stats.csv")
# df <- read.csv("~/rookie_LVSL_Reg_stats.csv")

season_3pct <- c(.359, .360, .350, .354, .358, .362,
             .355, .358, .367, .354, .342)
year <- ys <- seq(2013, 2023, 1)

yr3 <- as.data.frame(cbind(season_3pct, year))

df <- left_join(df, yr3, by = c("year"))

df <- df %>%
  mutate(exp_3pct = ((x3pa*x3p_percent)+(242*season_3pct))/(x3pa+242))

df1 <- df %>%
  filter(type == "summer")

df2 <- df %>%
  filter(type == "season")

df1 <- df1 %>%
  select(player, x3pa, min, fga_36, x3pa_36, x3p_percent, year, season_3pct, exp_3pct, type)

df2 <- df2 %>%
  select(player, x3pa, min, fga_36, x3pa_36, x3p_percent, season_3pct, exp_3pct, type)

df3 <- merge(df1, df2, by = "player")

df3 <- df3 %>%
  arrange(year) %>%
  filter(min.x >= 50 &
           min.y >= 250 )

p <- df3 %>% 
  ggplot(aes(x = x3pa_36.x, y = x3pa_36.y)) + 
  geom_point(aes(size = min.y), shape = 21, alpha = 0.5) +
  geom_smooth(method = 'loess', size = .5) + 
  stat_poly_eq(method = 'lm', family = "AppleGothic") +
  theme_preston() + 
  theme(legend.position = 'none', 
        plot.title = element_text(face = 'bold', size = 13), 
        plot.title.position = 'plot', 
        plot.margin = margin(10, 10, 15, 10), 
        axis.text = element_text(size = 4.5)) + 
  labs(x = "Summer League 3PA per 100", 
       y = "Rookie Season 3PA per 100", 
       title = "Summer League Stats vs. Rookie Season Stats", 
       subtitle = "Minimum 50 summer league minutes & 250 rookie season minutes (2013 - 2023)")
p
