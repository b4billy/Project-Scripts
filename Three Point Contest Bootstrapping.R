# Libraries
library(hoopR)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)

# Load 2022 Data
data_2022 <- hoopR::load_nba_pbp(2022)

player_2022 <- data_2022 %>% 
  # Only Shooting Plays
  filter(shooting_play) %>% 
  # Only Steph curry
  filter(participants_0_athlete_id == 3975) %>% 
  # Filter Down To Only 3s in a few steps
  ## Get Shot Distance Measurements from pbp text
  mutate(shot_distance = str_extract_all(text, "..-foot") %>% 
           substr(1, 2) %>% as.numeric() %>% suppressWarnings()) %>% 
  ## Only select where shot distance > 24 or score_value is 3 or 
  ## where pbp text has the words "three point"
  filter(shot_distance > 24 | score_value == 3 | str_detect(text, "three point")) %>% 
  # Add variable for what rack the shot belongs to and
  # variable for Make/Miss rather than TRUE/FALSE like scoring_play
  mutate(rack = case_when(coordinate_x <= 19 & coordinate_y <= 15 ~ 1,
                          coordinate_x >= 19 & coordinate_x <= 31 & coordinate_y >= 15 ~ 3,
                          coordinate_x >= 31 & coordinate_y <= 15 ~ 5,
                          coordinate_x <= 19 & coordinate_y >= 15 ~ 2,
                          coordinate_x >= 31 & coordinate_y >= 15 ~ 4),
         scoring_play = case_when(scoring_play ~ "Make",
                                  TRUE ~ "Miss")) %>% 
  # Only select the needed variables
  select(coordinate_x, coordinate_y, rack, scoring_play)

# Make rack a factor variable
player_2022$rack <- factor(player_2022$rack)

# Graph of All Shots
 ggplot(player_2022, aes(x = coordinate_x,
                        y = coordinate_y,
                        shape = rack,
                        color = scoring_play)) +
   # Jitter shot locations so all are shown
   geom_jitter() +
   # Choose specific shapes for shots
   scale_shape_manual(values = c(0, 1, 2, 3, 4)) +
   # Choose Specific colors for Make/Miss
   scale_color_manual(values = c("Make" = "#FF0000",
                                 "Miss" = "#303c6a")) +
   # FT Line
   geom_hline(yintercept = 15,
              color = "black") +
   # Lane Lines
   geom_vline(xintercept = 19,
              color = "black") +
   geom_vline(xintercept = 31,
              color = "black") +
   # Standardize the X Limits
   xlim(-0.5,50.5) +
   # Titles
   labs(title = "2022 Steph Curry 3 Pointers",
        subtitle = paste("Data as of", format(Sys.time(), "%b %d %Y")),
        color = "Make/Miss",
        shape = "Rack",
        caption = "Visualization by Billy Fryer ~ Data from hoopR package") +
   # Theme Changes
   theme_minimal() +
   theme(# Center Plot Title and Subtitle
         plot.title = element_text(hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         # Get rid of axis titles, ticks and labels
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         axis.text = element_blank(),
         # Make Plot Background the court color
         plot.background = element_rect(fill = "#eed6ab"),
         panel.grid = element_blank()) +
   # Add on Circle for Rim, I stole this code from somewhere
   annotate("path",
            x = 25+1*cos(seq(0,2*pi,length.out=100)),
            y = 0+1*sin(seq(0,2*pi,length.out=100)))
# Save the Image as a png
 ggsave("Steph Curry 3 Pointers.png",
        width = 4.5,
        height = 3.5)

### Bootstrap sampling
# Separate By Rack Function
sep_by_rack <- function(df, loc) {
  # Filter data frame down to shots by a certain rack location
 filter(df, rack == loc)
}

# Calculate Score per rack
calc_score <- function(rack_vec, moneyball) {
  # If it's not the moneyball rack
  if(!moneyball) {
    # One point for every ball and the last is worth double (moneyball)
    sum(rack_vec[-5] == "Make") + 2*sum(rack_vec[5] == "Make")
    # Otherwise (if it is the moneyball rack)
  } else {
    # Every shot is worth 2
    2*sum(rack_vec == "Make")
  }
}

# Funcion that performs the bootstrapping
# df argument is a dataframe
# moneyball_rack is the number of the moneyball rack
# B is the number of resamples (defaulted to 1 thousand)
# seed for reproducibility
bootstrap_score <- function(df, moneyball_rack, B = 1000, seed = 30) {
  # Separate Data Frames
  rack_1 <- sep_by_rack(df, 1)
  rack_2 <- sep_by_rack(df, 2)
  rack_3 <- sep_by_rack(df, 3)
  rack_4 <- sep_by_rack(df, 4)
  rack_5 <- sep_by_rack(df, 5)
  # Set aside space for rack results
  r1score <- rep(NA, B)
  r2score <- rep(NA, B)
  r3score <- rep(NA, B)
  r4score <- rep(NA, B)
  r5score <- rep(NA, B)
  totalscore <- rep(NA, B)
  
  # Set Seed For Reproducibility Like Curry's Jumper
  set.seed(seed)
  # For every resample
  for(i in 1:B){
    # Sample 5 shots From Each Rack
    r1_pts <- sample(rack_1$scoring_play, size = 5, replace=TRUE)
    r2_pts <- sample(rack_2$scoring_play, size = 5, replace=TRUE)
    r3_pts <- sample(rack_3$scoring_play, size = 5, replace=TRUE)
    r4_pts <- sample(rack_4$scoring_play, size = 5, replace=TRUE)
    r5_pts <- sample(rack_5$scoring_play, size = 5, replace=TRUE)
    # Calculate score for each rack (taking moneyball rack into account)
    if(moneyball_rack == 1) {
      r1score[i] <- calc_score(r1_pts, TRUE)
      r2score[i] <- calc_score(r2_pts, FALSE)
      r3score[i] <- calc_score(r3_pts, FALSE)
      r4score[i] <- calc_score(r4_pts, FALSE)
      r5score[i] <- calc_score(r5_pts, FALSE)
    } else if(moneyball_rack == 2) {
      r1score[i] <- calc_score(r1_pts, FALSE)
      r2score[i] <- calc_score(r2_pts, TRUE)
      r3score[i] <- calc_score(r3_pts, FALSE)
      r4score[i] <- calc_score(r4_pts, FALSE)
      r5score[i] <- calc_score(r5_pts, FALSE)
    } else if(moneyball_rack == 3) {
      r1score[i] <- calc_score(r1_pts, FALSE)
      r2score[i] <- calc_score(r2_pts, FALSE)
      r3score[i] <- calc_score(r3_pts, TRUE)
      r4score[i] <- calc_score(r4_pts, FALSE)
      r5score[i] <- calc_score(r5_pts, FALSE)
    } else if(moneyball_rack == 4) {
      r1score[i] <- calc_score(r1_pts, FALSE)
      r2score[i] <- calc_score(r2_pts, FALSE)
      r3score[i] <- calc_score(r3_pts, FALSE)
      r4score[i] <- calc_score(r4_pts, TRUE)
      r5score[i] <- calc_score(r5_pts, FALSE)
    } else if(moneyball_rack == 5) {
      r1score[i] <- calc_score(r1_pts, FALSE)
      r2score[i] <- calc_score(r2_pts, FALSE)
      r3score[i] <- calc_score(r3_pts, FALSE)
      r4score[i] <- calc_score(r4_pts, FALSE)
      r5score[i] <- calc_score(r5_pts, TRUE)
    }
    # Sum up each rack to a total score
    totalscore[i] <- sum(r1score[i], r2score[i], r3score[i], 
                         r4score[i], r5score[i])
  # And repeat
  }
  # Put all of the scores into a dataframe
  scores_df <- data.frame(r1score, r2score, r3score, 
                          r4score, r5score, totalscore)
  
  # Get median, mean, standard error and margin of error 
  # for each rack and overall score
  median <- apply(scores_df,
                  MARGIN = 2,
                  FUN = median)
  mean <- apply(scores_df,
                MARGIN = 2,
                FUN = mean)
  se <- apply(scores_df,
              MARGIN = 2,
              FUN = sd)
  me <- 2*se
  # Make all of this other info into a dataframe of its own
  other_df <- data.frame(rack = c(1:5, "Total"), 
                         median = median, 
                         mean = mean, 
                         se = se, 
                         me = me) %>% 
    # Mutate on a column that says which is the moneyball rack
    mutate(moneyball_rack = case_when(rack == "Total" ~ NA,
                                      rack == moneyball_rack ~ TRUE,
                                      TRUE ~ FALSE))
  # Prepare a list of 2 dataframes for output
  output <- list(scores = scores_df,
                 other_info = other_df)
  return(output)
}

# Produces each individual bootstrapped histogram plot.
# This is used as a helper function for the bootstrap_histograms function
# Inputs:
# info is an output from the bootstrap_score function
# rack is the specific rack that we want to graph
ind_bootstrap_histogram <- function(info, rack) {
  
  # Number of Bootstraps
  B <- nrow(info$scores)
  # Identify Moneyball Rack
  moneyball <- which(info$other_info$moneyball_rack)
  # Get scores data frame
  scores_df <- info$scores
  # Get Column name of relevant column
  column <- ifelse(is.na(names(scores_df)[rack]), "totalscore", names(scores_df)[rack])
  
  # Get Text for (sub)titles and other formatting
  rack_text <- ifelse(rack == "Total", "Total", paste("Rack", rack))
  # Prepare subtitle text
  ## Start with number of rounds of bootstrapping
  subtitle_text <- paste("n =", B, "Rounds of Bootstrapping")
  ## Clearly state if rack is the moneyball rack
  subtitle_text <- ifelse(moneyball == rack, paste0(subtitle_text, ", Moneyball Rack"), subtitle_text)
  # Find the min and max of the column for use in 
  # putting labels on the x axis of the histogram
  min_x <- eval(parse(text=paste0("min(scores_df$", column, ')')))
  max_x <- eval(parse(text=paste0("max(scores_df$", column, ')')))
  ifelse(rack == "Total", 
       x_lims <- seq(min_x,max_x, by = 2), 
       x_lims <- seq(min_x,max_x))
  # Green bar for moneyball rack histogram
  # Purple Bar for total score histogram
  # Royal Blue Bar Otherwise
  bar_color <- ifelse(moneyball == rack, "forestgreen", 
                      ifelse(rack == "Total", "purple", "royalblue"))

  plot <- ggplot(scores_df, aes(x = get(column))) +
    # Histogram Plot (with Density as first y axis)
    geom_histogram(aes(y = ..density..), 
                   binwidth = 1,
                   fill = bar_color) +
    # X Axis
    # This is where we use the min_x and max_x to get all labels
    # to appear. suppressWarnings is because it asks for the limits
    # to be a factor or scale_x_continuous which doesn't work
    suppressWarnings(scale_x_discrete(limits = x_lims)) +
    # Double Y Axis
    # First Axis is the Density and Second is Raw Number
    # trans is transformation (so multiply density by number of bootstraps)
    # name is name of secondary axis
    scale_y_continuous(
      "Density", 
      sec.axis = sec_axis(trans = ~ . * B,
                          name = "Raw Number")
    ) +
    # Titles
    labs(title = paste("Steph Curry Boot Strapped", rack_text, "Score"),
         subtitle = subtitle_text,
         x = "Score") +
    # Theme
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "grey"))
  

  return(plot)
}

# Grids all 6 bootstrap histograms together
# Input:
# info is an output from the bootstrap_score function
bootstrap_histograms <- function(info) {
  # Runs the ind_bootstrap_histogram for every rack and total
  hist1 <- ind_bootstrap_histogram(info = info, 1)
  hist2 <- ind_bootstrap_histogram(info = info, 2)
  hist3 <- ind_bootstrap_histogram(info = info, 3)
  hist4 <- ind_bootstrap_histogram(info = info, 4)
  hist5 <- ind_bootstrap_histogram(info = info, 5)
  histtotal <- ind_bootstrap_histogram(info = info, "Total")
  
  # Arranges in a grid and outputs
  output <- grid.arrange(hist1, hist2, hist3, hist4, hist5, histtotal, nrow = 2)

  return(output)
}

### Test Run

example <- bootstrap_score(df = player_2022, moneyball_rack = 2,
                           B = 1000, seed = 30)

grid_bootstrap <- bootstrap_histograms(example)

# ggsave("grid bootstrap.png",
#       plot = grid_bootstrap,
#       width = 12,
#       height = 6)