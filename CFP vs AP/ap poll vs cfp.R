# AP Poll vs CFP Rankings
library(dplyr)
library(ggplot2)
#devtools::install_github("saiemgilani/cfbfastR")
library(cfbfastR)
#devtools::install_github("Kazink36/cfbplotR")
library(cfbplotR)
library(tidyr)
library(ggthemes)


cfb_rankings <- cfbfastR::cfbd_rankings(year = 2021) %>% 
  # Most Current Week
  dplyr::filter(week == max(week))

# Pull Week Number for Automation
current_week <- paste("Week", unique(cfb_rankings$week))
cfb_rankings <- cfb_rankings %>% select(-week)

cfb_rankings <- cfb_rankings %>% 
  # Committee and AP Top 25
  dplyr::filter(poll %in% c("AP Top 25", "Playoff Committee Rankings")) %>% 
  # Select Variables
  dplyr::select(poll:school) %>% 
  tidyr::pivot_wider(id_cols = school, 
                     names_from = poll,
                     values_from = rank)

# Teams unranked in a poll are "26th"
cfb_rankings[is.na(cfb_rankings)] <- 26

# Spearman correlation
spearman <- cor(cfb_rankings$`AP Top 25`, 
                cfb_rankings$`Playoff Committee Rankings`,
                method = "spearman")


ggplot(cfb_rankings, aes(y = `AP Top 25`,
                         x = `Playoff Committee Rankings`)) +
  geom_abline(slope = 1,
              size = 1.5,
              color = "#0000FF") +
  geom_vline(xintercept = 25,
              color = "#000000") +
  geom_hline(yintercept = 25,
             color = "#000000") +
  cfbplotR::geom_cfb_logos(aes(team = school),
                           width = 0.06) +
  scale_x_reverse(breaks = c(1,seq(5,25,5))) +
  scale_y_reverse(breaks = c(1,seq(5,25,5))) +
  labs(title = paste(current_week, "CFP Rankings vs AP Top 25"),
       subtitle = paste("Spearman Correlation =", round(spearman, 2)),
       caption = "Viz by Billy Fryer | Data From @cfbfastR",
       x = "Playoff Committee Rankings",
       y = "AP Top 25") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 12)) +
  # Below Blue Line
  annotate(geom = "label",
           label = "Teams Ranked Higher\nby the CFP Committee",
           x =  5,
           y = 20,
           size = 3) +
  # Above Blue Line
  annotate(geom = "label",
           label = "Teams Ranked Higher\nby the AP Poll",
           x = 20,
           y =  5,
           size = 3)


ggsave(filename = paste(current_week,"CFP vs AP Top 25.png"),
       width = 8,
       height = 5)
