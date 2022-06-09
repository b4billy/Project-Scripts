# Call/Install Packages needed for analysis
# devtools::install_github("b4billy/FHSTR")
library(FHSTR)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(geomtextpath)
library(factoextra) # Plotting K Means stuff
library(plotly)
library(htmlwidgets)
library(gt)

### GOAL: Find times and shooting abilities for every athlete and then
### kmeans to cluster them together

###############################################################
### Data
###############################################################

# Find Biathlon Schedule
# Find Biathlon SportID
biathlon_id <- sport_list %>% 
  filter(c_Sport == "Biathlon") %>% 
  pull(n_SportID)

# Get Biathlon Schedule
biathlon_schedule <- load_olympic_sport_schedules(biathlon_id)

# All Mass Start Races
mass_start <- biathlon_schedule %>% 
  filter(str_detect(c_ContainerMatch, "Mass Start")) %>% 
  select(n_MatchID, c_ContainerMatch, GenderEvent.c_Name)

# Pull men and women IDs
women_id <- mass_start %>% 
  filter(str_detect(GenderEvent.c_Name, "Women's")) %>% 
  pull(n_MatchID)
men_id <- mass_start %>% 
  filter(!str_detect(GenderEvent.c_Name, "Women's")) %>% 
  pull(n_MatchID)

# Load CSV Data and add a column saying from which data set 
women_data <- load_olympic_csv_data(biathlon_id, women_id) %>% 
  mutate(Gender = "W")
men_data <- load_olympic_csv_data(biathlon_id, men_id) %>% 
  mutate(Gender = "M")

# Combine Data
full_data <- bind_rows(women_data, men_data) %>% 
  # Has to have a finishing time
  filter(!is.na(n_TimeAbs)) %>% 
  # Fastest to Slowest
  arrange(n_TimeAbs) %>%
  mutate(# Paste first and last name together
         NiceName = paste(c_ParticipantFirstName, c_ParticipantLastName),
         # Found Out Which is standing vs prone from here:
         # https://www.biathlonworld.com/inside-ibu/sports-and-event/biathlon-mass-start
         Prone1 = as.numeric(str_split_fixed(string = c_ResultInfo_1, 
                                          pattern = "\\+",
                                          n = 4)[,1]),
         Prone2 = as.numeric(str_split_fixed(string = c_ResultInfo_1, 
                                          pattern = "\\+",
                                          n = 4)[,2]),
         # Add for total missed shots from prone position
         Prone_Total = Prone1 + Prone2,
         Stand1 = as.numeric(str_split_fixed(string = c_ResultInfo_1, 
                                  pattern = "\\+",
                                  n = 4)[,3]),
         Stand2 = as.numeric(str_split_fixed(string = c_ResultInfo_1, 
                                  pattern = "\\+",
                                  n = 4)[,4]),
         # Add for total missed shots from standing position
         Stand_Total = Stand1 + Stand2,
         # n_TimeAbs gives time in milliseconds, so convert to minutes
         # for comprehension. This won't have any effect on modeling since
         # all transformations were linear
         Total_Time_Min = n_TimeAbs / 1000 / 60,
         )

###############################################################
### Kmeans Modeling
###############################################################

# Only data We want to use for clustering is time, prone and standing shooting
modeling_data <- full_data %>% 
  select(Total_Time_Min, Prone_Total, Stand_Total, Gender) %>% 
  # Scaling all variables to give them equal weight
  mutate(scaled_time = scale(Total_Time_Min),
        scaled_prone = scale(Prone_Total),
        scaled_stand = scale(Stand_Total)) %>% 
  # Select Scaled variables and convert to a matrix
  select(starts_with("scaled")) %>% 
  as.matrix()

# K-means Algorithm
# Assistance from here; https://uc-r.github.io/kmeans_clustering#optimal
# Seed for Reproducibility
set.seed(2022)

# Function to compute total within-cluster sum of square 
wss <- function(k, data = modeling_data) {
  kmeans(data, k, nstart = 20)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# Extract wss values for 1-15 clusters
wss_values <- map_dbl(k.values, wss)

# Make a dataframe of number of clusters and wss values
elbow_df <- data.frame(k = k.values,
                       wss = wss_values)

# Make Elbow Plot
ggplot(elbow_df, aes(x = k,
                     y = wss)) +
  # Points connected by a line
  geom_point() +
  geom_line() + 
  # Vertical Line at 4 Clusters
  geom_textvline(xintercept = 5, label = "5 Clusters") +
  # Title, Caption, and Axis Labels
  labs(title = "K-Means Elbow Plot",
       x = "Number of Clusters (K)",
       y = "Total Within-Clusters Sum of Squares",
       caption = "Viz by Billy Fryer ~ Data from FHSTR") +
  # Plot Theming
  theme_igray() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))

# Save Plot
# ggsave("Outputs/Biathlon K Means Output/Elbow-Plot.png",
#      width = 4,
#      height = 4)

# Final K Means Model with 5 Clusters
set.seed(60)
kmeans.model <- kmeans(modeling_data, 5, nstart = 20)

# Attach Clusters to full data
full_data2 <- full_data %>% 
  mutate(Cluster = factor(kmeans.model$cluster),
         ClusterLabel = paste("Cluster", Cluster))

###############################################################
### Data Visualization
###############################################################

# For Coloring of clusters
colors_vec <- c("aquamarine", "lightblue","gold", "pink", "thistle")

# Clustering Plot
fviz_cluster(kmeans.model,
             geom = "point",
             data = modeling_data) +
  # Color clusters with colors I want
  scale_fill_manual(values = colors_vec) +
  scale_color_manual(values = colors_vec) +
  # Labels and Title
  labs(title = "Cluster Plot",
       color = "Cluster",
       fill = "Cluster",
       shape = "Cluster",
       caption = "Data Viz from factoextra package with modifications") +
  # Plot Theming
  theme_igray() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1))

# Save Plot as png
# ggsave("Outputs/Biathlon K Means Output/Cluster-Plot.png",
#        width =  5,
#        height = 5,
#        units = "in")

# 3d Plot
plot_3d <- plot_ly(data = full_data2,
        # You must put the ~ before variables in the dataset
        # Otherwise it looks in your environment
        text = ~NiceName,
        x = ~Stand_Total, 
        y = ~Prone_Total, 
        z = ~Total_Time_Min, 
        # 3d Plot
        type = "scatter3d", 
        # With round balls for markers
        mode = "markers", 
        # Color points and specify colors
        color = ~ClusterLabel,
        colors = colors_vec) %>%
  # Title, Legends, and Axis Labels
  layout(scene = list(xaxis=list(title = "Prone Misses"),
                      yaxis=list(title = "Standing Misses"),
                      zaxis=list(title = "Time (Min)")),
         title = "Prone Misses vs Standing Misses vs Time",
         legend = list(title=list(text='')))

# Save 3D Plot
# saveWidget(plot_3d, 
#            "Outputs/Biathlon K Means Output/3D-Plot.html") 

# Filter Data for Visualizations
filtered_data <- full_data2 %>%
  # Select only necessary variables
  select(Cluster, ClusterLabel, NiceName, 
         NOC.c_Name, Gender, n_Rank, 
         c_ResultAbs, Prone_Total, Stand_Total) %>% 
  # Add Prone and Standing Misses to get Total Misses
  mutate(Misses_Total = Prone_Total + Stand_Total)

# Averages by cluster
by_cluster <- filtered_data %>% 
  # Group by Cluster
  group_by(Cluster) %>% 
  # Time Average Code stolen from https://stackoverflow.com/questions/42281134/average-time-in-a-column-in-hrminsec-format
  summarize(cluster_size = n(),
            avg_rank = mean(n_Rank, na.rm = TRUE),
            avg_time = format(mean(strptime(c_ResultAbs, "%M:%S")), "%M:%S"),
            avg_prone_miss = mean(Prone_Total, na.rm = TRUE),
            avg_stand_miss = mean(Stand_Total, na.rm = TRUE),
            avg_total_miss = mean(Misses_Total, na.rm = TRUE)
  ) %>% 
  ungroup()

by_cluster %>% 
  # GT Table
  gt() %>%
  # Title and Subtitle
  tab_header(
    title = md("**Cluster Averages**"),
  ) %>% 
  # Footnote
  tab_source_note(source_note = "Viz by Billy Fryer (@_b4billy_) | Data from FHSTR") %>%
  # Tab Options
  tab_options(
    # Table Font Color
    table.font.color = "#000000",
    # Bold Title
    heading.title.font.weight = "bold",
    # Change Subtitle Font Size
    heading.subtitle.font.size = 12,
    # Align Heading
    heading.align = "center",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.width = px(3),
  ) %>% 
  # Center Columns
  cols_align(align = "center") %>% 
  # Change Column Labels
  cols_label(
    "Cluster" = "Cluster",
    "cluster_size" = "Cluster Size",
    "avg_rank" = "Avg Rank",
    "avg_time" = "Avg Time",
    "avg_prone_miss" = "Prone",
    "avg_stand_miss" = "Standing",
    "avg_total_miss" = "Total") %>% 
  # Tab Spanners
  tab_spanner(columns = c(avg_prone_miss, avg_stand_miss, avg_total_miss),
              label = "Avg Shooting Misses") %>%
  # One Decimal Place for almost all numbers
  fmt_number(
    columns = c(avg_rank, avg_prone_miss, avg_stand_miss, avg_total_miss),
    decimals = 1
  ) %>% 
  gtsave(filename = "Outputs/Biathlon K Means Output/Cluster-Averages.png")


### All Biathletes
filtered_data %>% 
  # Arrange data.frame in order how I want table to appear
  select(NiceName:Misses_Total, ClusterLabel) %>% 
  # Arrange Rows by rank
  arrange(n_Rank) %>% 
  # Spell out Gender
  mutate(Gender = case_when(Gender == "M" ~ "Men",
                          Gender == "W" ~ "Women")) %>% 
  # GT respects grouping from dplyr
  group_by(Gender) %>% 
  gt(rowname_col = "Name") %>%  
  # Title and Subtitle
  tab_header(
    title = md("**All Biathletes**")
  ) %>% 
  # Footnote
  tab_source_note(source_note = "Viz by Billy Fryer (@_b4billy_) | Data from FHSTR") %>%
  # Tab Options
  tab_options(
    # Table Font Color
    table.font.color = "#000000",
    # Bold Title
    heading.title.font.weight = "bold",
    # Change Subtitle Font Size
    heading.subtitle.font.size = 12,
    # Align Heading
    heading.align = "center",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.width = px(3),
  ) %>% 
  # Center Columns
  cols_align(align = "center") %>% 
  # Change Column Labels
  cols_label(
    "NiceName" = "Name",
    "NOC.c_Name" = "Country",
    "n_Rank" = "Finish",
    "c_ResultAbs" = "Time",
    "Prone_Total" = "Prone",
    "Stand_Total" = "Standing",
    "Misses_Total" = "Total",
    "ClusterLabel" = "Cluster",) %>% 
  # Tab Spanners
  tab_spanner(columns = c(Prone_Total, Stand_Total, Misses_Total),
              label = "Shooting Misses") %>% 
  # Change Country Column to Flags
  text_transform(
    locations = cells_body(NOC.c_Name),
    fn = function(x) {
      # loop over the elements of the column
      map_chr(x, ~ local_image(
        # I had flag PNGs in the sub folder listed below 
        # where .x is the NOC.c_Name
        filename = paste0("Flags and Icons/Flags/", .x, ".png"),
        height = 30
      ))
    }) %>% # 1st Place Athletes in Gold
  tab_style(
    # What we did to the cell
    style = list(
      # Make Cell Gold
      cell_fill(color = "#D6AF36")
    ),
    # Which Cells to Apply it to
    locations = cells_body(
      # Apply to Rank Column
      columns = n_Rank,
      # 1st Place Athletes
      rows = n_Rank == 1)
  ) %>% # 2nd Place Athletes in Silver
  tab_style(
    # What we did to the cell
    style = list(
      # Make Cell Silver
      cell_fill(color = "#A7A7AD")
    ),
    # Which Cells to Apply it to
    locations = cells_body(
      # Apply to Rank Column
      columns = n_Rank,
      # 2nd Place Athletes
      rows = n_Rank == 2)
  ) %>% # 3rd Place Athletes in Bronze
  tab_style(
    # What we did to the cell
    style = list(
      # Make Cell Bronze
      cell_fill(color = "#824A0D")
    ),
    # Which Cells to Apply it to
    locations = cells_body(
      # Apply to Rank Column
      columns = n_Rank,
      # 2nd Place Athletes
      rows = n_Rank == 3)
  ) %>%
  gtsave(filename = "Outputs/Biathlon K Means Output/All-Biathletes.png")

### Biathletes in each cluster tables
# All of these tables are the same so I made a function!
cluster_table <- function(cluster, df = filtered_data){
  # Get Title of table based on cluster number inputted
  title_text <- paste("Biathletes from Cluster", cluster)
  
  df %>% 
    # Filter by cluster specified
    filter(Cluster == cluster) %>% 
    gt() %>%  
    # Title and Subtitle
    tab_header(
      title = md(title_text),
    ) %>% 
    # Footnote
    tab_source_note(source_note = "Viz by Billy Fryer (@_b4billy_) | Data from FHSTR") %>%
    # Tab Options
    tab_options(
      # Table Font Color
      table.font.color = "#000000",
      # Bold Title
      heading.title.font.weight = "bold",
      # Change Subtitle Font Size
      heading.subtitle.font.size = 12,
      # Align Heading
      heading.align = "center",
      column_labels.border.top.width = px(3),
      column_labels.border.bottom.width = px(3),
    ) %>% 
    # Center Columns
    cols_align(align = "center") %>% 
    # Change Column Labels
    cols_label(
      "Cluster" = "Cluster",
      "NiceName" = "Name",
      "NOC.c_Name" = "Country",
      "n_Rank" = "Finish",
      "c_ResultAbs" = "Time",
      "Prone_Total" = "Prone",
      "Stand_Total" = "Standing",
      "Misses_Total" = "Total") %>%
    # Don't need Cluster of ClusterLabel since 
    # there is only 1 cluster present per table
    cols_hide(c("Cluster", "ClusterLabel")) %>% 
    # Tab Spanners
    tab_spanner(columns = c(Prone_Total, Stand_Total, Misses_Total),
                label = "Shooting Misses") %>% 
    # Change Country Column to Flags
    text_transform(
      locations = cells_body(NOC.c_Name),
      fn = function(x) {
        # loop over the elements of the column
        map_chr(x, ~ local_image(
          # This is the same as before
          filename = paste0("Flags and Icons/Flags/", .x, ".png"),
          height = 30
        ))
      }) %>%
    # 1st Place Athletes in Gold
    tab_style(
      # What we did to the cell
      style = list(
        # Make Cell Gold
        cell_fill(color = "#D6AF36")
      ),
      # Which Cells to Apply it to
      locations = cells_body(
        # Apply to Rank Column
        columns = n_Rank,
        # 1st Place Athletes
        rows = n_Rank == 1)
    ) %>% # 2nd Place Athletes in Silver
    tab_style(
      # What we did to the cell
      style = list(
        # Make Cell Silver
        cell_fill(color = "#A7A7AD")
      ),
      # Which Cells to Apply it to
      locations = cells_body(
        # Apply to Rank Column
        columns = n_Rank,
        # 2nd Place Athletes
        rows = n_Rank == 2)
    ) %>% # 3rd Place Athletes in Bronze
    tab_style(
      # What we did to the cell
      style = list(
        # Make Cell Bronze
        cell_fill(color = "#824A0D")
      ),
      # Which Cells to Apply it to
      locations = cells_body(
        # Apply to Rank Column
        columns = n_Rank,
        # 2nd Place Athletes
        rows = n_Rank == 3)
    ) %>% return()

}

### Cluster Tables
cluster_table(1) %>%  gtsave(filename = "Outputs/Biathlon K Means Output/Cluster-1-Table.png")
cluster_table(2) %>%  gtsave(filename = "Outputs/Biathlon K Means Output/Cluster-2-Table.png")
cluster_table(3) %>%  gtsave(filename = "Outputs/Biathlon K Means Output/Cluster-3-Table.png")
cluster_table(4) %>%  gtsave(filename = "Outputs/Biathlon K Means Output/Cluster-4-Table.png")
cluster_table(5) %>%  gtsave(filename = "Outputs/Biathlon K Means Output/Cluster-5-Table.png")

