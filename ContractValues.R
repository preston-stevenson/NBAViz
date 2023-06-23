library(tidyverse)
library(janitor)
library(rvest)
library(paletteer)
library(scales)
library(ggrepel)

# set theme
theme_preston <- function () { 
  theme_minimal(base_size=9, base_family="AppleGothic") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}


url <- "https://www.basketball-reference.com/contracts/players.html"


df <-  url %>%
  read_html() %>% 
  html_table()

df <- df[[1]]

#convert first row to header names and then clean up the variable name
df <- df %>% 
  row_to_names(row_number = 1) %>% 
  clean_names()

#Consolidate duplicates player entries -- ie, if a player has been on more than one team, use his TOT entry 
df <- df %>% 
  group_by(player) %>% 
  mutate(team_count = n()) %>% 
  ungroup() %>% 
  mutate(keep = case_when(
    team_count == 1 | tm == "TOT" ~ "keep",
    TRUE ~ "Discard"
  )) %>% 
  filter(keep == "keep") 

df$x2022_23 = as.numeric(gsub("[$,]", "", df$x2022_23))
df$x2023_24 = as.numeric(gsub("[$,]", "", df$x2023_24))
df$x2024_25 = as.numeric(gsub("[$,]", "", df$x2024_25))
df$x2025_26 = as.numeric(gsub("[$,]", "", df$x2025_26))
df$x2026_27 = as.numeric(gsub("[$,]", "", df$x2026_27))
df$x2027_28 = as.numeric(gsub("[$,]", "", df$x2027_28))
df$guaranteed = as.numeric(gsub("[$,]", "", df$guaranteed))

df <- df %>% mutate(x2022_23 = ifelse(is.na(x2022_23), 0, x2022_23),
                    x2023_24 = ifelse(is.na(x2023_24), 0, x2023_24),
                    x2024_25 = ifelse(is.na(x2024_25), 0, x2024_25),
                    x2025_26 = ifelse(is.na(x2025_26), 0, x2025_26),
                    x2026_27 = ifelse(is.na(x2026_27), 0, x2026_27),
                    x2027_28 = ifelse(is.na(x2027_28), 0, x2027_28),
                    guaranteed = ifelse(is.na(guaranteed), 0, guaranteed))


df <- df %>%
  select(player, 4:10) %>%
  pivot_longer(-player, names_to = "stat", values_to = "amount")


# for(i in 1:6){       # for-loop over columns
#   df1[, i+3] <- as.numeric(gsub("[$,]", "", df1[, i+3]))
# }

url1 <- "https://www.basketball-reference.com/leagues/NBA_2023_advanced.html"

df1 <-  url1 %>%
  read_html() %>% 
  html_table()

df1 <- df1[[1]]

df1 <- df1[, -c(20,25)]

#Consolidate duplicates player entries -- ie, if a player has been on more than one team, use his TOT entry 
df1 <- df1 %>% 
  group_by(Player) %>% 
  mutate(team_count = n()) %>% 
  ungroup() %>% 
  mutate(keep = case_when(
    team_count == 1 | Tm == "TOT" ~ "keep",
    TRUE ~ "Discard"
  )) %>% 
  filter(keep == "keep")

df2 <- merge(df, df1, by.x = "player", by.y = "Player")

df2$BPM <- as.numeric(df2$BPM)
df2$MP <- as.numeric(df2$MP)
df2$Age <- as.numeric(df2$Age)

#mutate(test = (BPM*(Age/4))/4)

df2 <- df2 %>%
  filter(stat == "x2022_23" & MP > 250 & amount > 600000) %>%
  filter(!is.na(amount) & !is.na(BPM)) %>%
  group_by(Age) %>%
  mutate(amount1 = (mean(amount)/100000000)) %>%
  mutate(avg = 13324274/100000000) %>%
  mutate(multi = amount1 - avg + 1) %>%
  mutate(aBPM = BPM*multi)


  # group_by(Age) %>%
  # mutate(aBPM = ((mean(amount)/100000000)+1)*BPM)


fit <- lm(log(amount) ~ poly(aBPM, 2), data = df2)

df2$expected_amount <- (fitted(fit))
df2$residual <- (resid(fit))

teamchoice <- function(team){
  df2$team_player <- ifelse(df2$Tm == team, 
                               df2$player, "")
  df4 <- df2 %>%
    filter(team_player != "") %>% 
    ungroup() %>%
    summarize(avg_res = mean(residual)*10)
    
  df2 <- df2 %>%
    mutate(avg_res = -(df4$avg_res))
    
  return(df2)
}

df4 <- teamchoice("TOR")


p <- df4 %>%
  ggplot(aes(x = aBPM, y = amount)) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = 'black', 
              linetype = 'dashed', alpha = .15) +
  geom_point(size = 3, alpha = .75, shape = 21, color = 'black', aes(fill = -1*residual)) + 
  geom_text_repel(aes(label = team_player), family = "AppleGothic", 
                  size = 3, max.overlaps = Inf, force = 40) +
  geom_hline(yintercept = 13324274, size = .25, linetype = "dashed") +
  annotate("text", x = 10, y = 15000500, label = "Average Salary", 
           size = 3, family = "AppleGothic") +
  annotate("text", x = 1, y = 550000, label = "Replacement Level Score", 
           size = 3, family = "AppleGothic") +
  geom_text(aes(x = -7, y = 55000000, label = paste0("Residual Score: ",round(mean(avg_res), 2))), 
            size = 2.5, family = "AppleGothic") +
  geom_vline(xintercept = -2, size = .25, linetype = "dashed") +
  scale_y_log10(breaks = c(500000, 5000000, 15000000, 30000000, 60000000), 
                labels = dollar, limits = c(500000, 60000000)) +
  scale_x_continuous(limits = c(-8, 14), breaks = seq(-8, 14, 4)) +
  theme_preston() +
  labs(x = "Adjusted Box Plus/Minus",
       y = "2023 Contract Amount (logged)",
       fill = "Worse Contract                  Better Contract",
       title = "Contracts vs Player Value",
       subtitle = "2023 Player Contracts vs Salary/Age Adjusted BPM",
       family = "AppleGothic") +
  theme(legend.position = 'top',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,-10,-10,-10),
        plot.margin = margin(.25, .25, .5, .25, "cm"), 
        plot.title.position = 'plot',
        plot.title = element_text(size = 15), 
        plot.subtitle = element_text(size = 8)) +
  scale_fill_distiller(palette = "RdBu", direction = 1,  type = "div", breaks = c(-3, -2, -1, 0, 1, 2, 3), labels = c("", "", "", "", "", "", "")) +
  guides(fill = guide_colorbar(
    barwidth = 3,
    barheight = .15,
    default.unit="inch", 
    label.position = 'bottom', 
    title.position = 'bottom',
    title.hjust = .5,
    title.vjust = 7.5,
    nrow = 1))
p
ggsave("TORChart.png", width = 7, height = 6, dpi = 300)       
