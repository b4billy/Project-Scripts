# Biscuits Playoffs Chart
setwd("C:\\Users\\billy\\OneDrive\\Desktop\\Sports Analytics\\Biscuits Playoffs 2021")
library(tidyverse)
library(gt)
library(webshot)

year <-     c(2015,  2016,   2017,  2018,  2019,  2021)
opponent <- c("Chattanooga",  "TN",  "Chattanooga",  "TN", "TN", "Braves")
division <-  c("North",   "North",    "North",    "North",  "North",  "South")
result <- c(rep("L in Division Finals", 5), "???")

year2 <-     c(2018,  2019,  2021)
opponent2 <- c("TN", "TN", "MS")
state2 <- c("TN", "TN", "MS")
division2 <-  c("North",  "North",  "South")
result2 <- c(rep("L in Division Finals", 2), "???")

df1 <- data.frame(year, opponent, division, result)
df2 <- data.frame(year2, opponent2, division2, state2, result2)

###### Graphic 1 #####
df1 %>% 
  gt() %>% 
  # Make Country a Flag
  text_transform(
    locations = cells_body(opponent),
    fn = function(x) {
      # loop over the elements of the column
      map_chr(x, ~ local_image(
        filename = paste0(.x, ".jpg")
      ))
    }) %>% 
  cols_label("year" = "Year",
             "opponent" = "Opponent",
             "division" = "Division",
             "result" = "Result") %>% 
  tab_header(
    title = md("**Biscuits Playoff Runs Since 2015**"),
    subtitle = "Montgomery Has Made the Playoffs the Past 6 Seasons") %>% 
  tab_source_note(source_note = "2020 Season Canceled Due to COVID-19 Pandemic") %>% 
  tab_source_note(source_note = "Visualization by Billy Fryer") %>% 
  cols_align(align = "center") %>% 
  gtsave("Biscuits Graph 1.png")
  


##### Graphic 2  #####
df2 %>% 
  gt() %>% 
  # Make Country a Flag
  text_transform(
    locations = cells_body(opponent2),
    fn = function(x) {
      # loop over the elements of the column
      map_chr(x, ~ local_image(
        filename = paste0(.x, ".jpg")
      ))
    }) %>% 
  cols_label("year2" = "Year",
             "opponent2" = "Opponent",
             "state2" = "State",
             "division2" = "Division",
             "result2" = "Result") %>% 
  tab_header(
    title = md("**Can the Biscuits Finally Beat the Jackson Generals?**")) %>% 
  tab_source_note(source_note = "The Mississippi Braves have an throwback alternate identity as the Jackson Generals and the Jackson Generals (TN) are former members of the Southern League (Now AA South)") %>% 
  tab_source_note(source_note = "Visualization by Billy Fryer") %>% 
  cols_align(align = "center")  %>% 
  gtsave("Biscuits Graph 2.png")


