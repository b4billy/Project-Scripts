#' Non-MLB level codes
#' Here is a key for non-MLB levels and their corresponding code
#' 
#' 1 = MLB
#' 11 = Triple-A
#' 12 = Double-A
#' 13 = Class A Advanced
#' 14 = Class A
#' 15 = Class A Short Season
#' 5442 = Rookie Advanced
#' 16 = Rookie
#' 17 = Winter League

library(tidyverse)

#####################################################################
### Getting Game Packs
#####################################################################
get_game_pks <- function(date, level_ids = c(16,5442)) {
  
  # Print Date For Debugging
  print(date)
  
  api_call <- paste0("http://statsapi.mlb.com/api/v1/schedule?sportId=", paste(level_ids, collapse = ','), "&date=", date)
  
  payload <- jsonlite::fromJSON(api_call, flatten = TRUE)
  
  # This Line Added in By Billy Fryer.
  # Allows for scraping of a whole calendar year without messing up
  # Due to games not being played on a certain date
  if(length(payload$dates$games) == 0) {print(paste(date, "Not Availible")); return()}
  
  payload <- payload$dates$games %>%
    as.data.frame() %>%
    rename(game_pk = gamePk)
  
  # Throw error when scores aren't availible,
  # This would be triggered for postponements of all games on a date
  if(is.null(payload$teams.away.score)) {print(paste(date, "Not Availible")); return()}
  
  payload <- payload %>% 
    # Only Selected Columns
    select(officialDate, teams.away.team.name, teams.away.score,
           teams.home.team.name, teams.home.score, game_pk, scheduledInnings) %>% 
    # Rename some columns
    dplyr::rename("date" = "officialDate",
                  "away_team" = "teams.away.team.name",
                  "away_score" = "teams.away.score",
                  "home_team" = "teams.home.team.name",
                  "home_score" = "teams.home.score",
                  "gameid" = "game_pk")
  
  return(payload)
}


#' Acquire pitch-by-pitch data for Major and Minor League games via the MLB api 
#' \url{http://statsapi.mlb.com/api/}
#'
#' @param game_pk The date for which you want to find game_pk values for MLB games
#' @importFrom jsonlite fromJSON
#' @return Returns a data frame that includes over 100 columns of data provided
#' by the MLB Stats API at a pitch level.
#'
#' Some data will vary depending on the
#' park and the league level, as most sensor data is not availble in
#' minor league parks via this API. Note that the column names have mostly
#' been left as-is and there are likely duplicate columns in terms of the
#' information they provide. I plan to clean the output up down the road, but
#' for now I am leaving the majority as-is.
#'
#' Both major and minor league pitch-by-pitch data can be pulled with this
#'  function.
#' @keywords MLB, sabermetrics
#' @export
#'
#' @examples \dontrun{get_pbp_mlb(575156)}

get_pbp_mlb <- function(game_pk) {
  print(paste("Getting PBP for Game ID:", game_pk))
  
  api_call <- paste0("http://statsapi.mlb.com/api/v1.1/game/", game_pk, "/feed/live")
  payload <- jsonlite::fromJSON(api_call, flatten = TRUE)
  
  plays <- payload$liveData$plays$allPlays$playEvents %>% bind_rows()
  
  at_bats <- payload$liveData$plays$allPlays
  
  current <- payload$liveData$plays$currentPlay
  
  game_status <- payload$gameData$status$abstractGameState
  
  home_team <- payload$gameData$teams$home$name
  
  home_level <- payload$gameData$teams$home$sport
  
  home_league <- payload$gameData$teams$home$league
  
  away_team <- payload$gameData$teams$away$name
  
  away_level <- payload$gameData$teams$away$sport
  
  away_league <- payload$gameData$teams$away$league
  
  list_columns <- lapply(at_bats, function(x) class(x)) %>%
    dplyr::bind_rows(.id = "variable")
  
  if (length(list_columns) == 0) {return(NULL)}
  
  list_columns <- list_columns %>% 
    tidyr::gather(key, value) %>%
    dplyr::filter(value == "list") %>%
    dplyr::pull(key)
  
  at_bats <- at_bats %>%
    dplyr::select(-c(one_of(list_columns)))
  
  pbp <- plays %>%
    dplyr::left_join(at_bats, by = c("endTime" = "playEndTime"))
  
  pbp <- pbp %>%
    tidyr::fill(atBatIndex:matchup.splits.menOnBase, .direction = "up") %>%
    dplyr::mutate(game_pk = game_pk,
                  game_date = substr(payload$gameData$datetime$dateTime, 1, 10)) %>%
    dplyr::select(game_pk, game_date, everything())
  
  pbp <- pbp %>%
    dplyr::mutate(matchup.batter.fullName =
                    factor(matchup.batter.fullName),
                  matchup.pitcher.fullName =
                    factor(matchup.pitcher.fullName),
                  atBatIndex = factor(atBatIndex)
                  # batted.ball.result = case_when(!result.event %in% c(
                  #   "Single", "Double", "Triple", "Home Run") ~ "Out/Other",
                  #   TRUE ~ result.event),
                  # batted.ball.result = factor(batted.ball.result,
                  #                             levels = c("Single", "Double", "Triple", "Home Run", "Out/Other"))
    ) %>%
    dplyr::mutate(home_team = home_team,
                  home_level_id = home_level$id,
                  home_level_name = home_level$name,
                  home_parentOrg_id = payload$gameData$teams$home$parentOrgId,
                  home_parentOrg_name = payload$gameData$teams$home$parentOrgName,
                  home_league_id = home_league$id,
                  home_league_name = home_league$name,
                  away_team = away_team,
                  away_level_id = away_level$id,
                  away_level_name = away_level$name,
                  away_parentOrg_id = payload$gameData$teams$away$parentOrgId,
                  away_parentOrg_name = payload$gameData$teams$away$parentOrgName,
                  away_league_id = away_league$id,
                  away_league_name = away_league$name,
                  batting_team = factor(ifelse(about.halfInning == "bottom",
                                               home_team,
                                               away_team)),
                  fielding_team = factor(ifelse(about.halfInning == "bottom",
                                                away_team,
                                                home_team)))
  pbp <- pbp %>%
    dplyr::arrange(desc(atBatIndex), desc(pitchNumber))
  
  pbp <- pbp %>%
    dplyr::group_by(atBatIndex) %>%
    dplyr::mutate(last.pitch.of.ab =
                    ifelse(pitchNumber == max(pitchNumber), TRUE, FALSE)) %>%
    ungroup()
  
  check_home_level <- pbp %>%
    dplyr::distinct(home_level_id) %>%
    dplyr::pull()
  
  pbp <- pbp %>%
    dplyr::rename(count.balls.start = count.balls.x,
                  count.strikes.start = count.strikes.x,
                  count.outs.start = count.outs.x,
                  count.balls.end = count.balls.y,
                  count.strikes.end = count.strikes.y,
                  count.outs.end = count.outs.y)
  
  return(pbp)
}



#####################################################################
### Bulls Data
#####################################################################
bulls <- map_df(.x = seq.Date(as.Date("2021-01-01"), 
                                as.Date("2021-09-08"), 
                                'day'),
                  .f = get_game_pks,
                  level_ids = c(11)) %>% 
  filter(away_team == "Durham Bulls" | home_team == "Durham Bulls") %>% 
  pull(gameid)



pbp <- map_df(.x = bulls,
              .f = get_pbp_mlb)
 # Only Lowe
lowe <- pbp %>% 
  filter(matchup.batter.fullName == "Josh Lowe")



filtered_lowe <- lowe %>%
  # Gets Rid of Informational Events
  # Like Pitching Changes
  filter(isPitch) %>%
  # arrange by date and order of pitches
  arrange(game_date, about.inning, index) %>%
  # Select Useful Columns
  select(game_date, index, last.pitch.of.ab,
         details.call.description, result.event,
         batting_team, home_level_name, about.inning,
         pitchData.strikeZoneTop, pitchData.strikeZoneBottom,
         pitchData.coordinates.x, pitchData.coordinates.y,
         hitData.coordinates.coordX, hitData.coordinates.coordY
         )

# Flip y coordinate for hitting data
filtered_lowe$hitData.coordinates.coordY = -1 * filtered_lowe$hitData.coordinates.coordY


##############################################################
### Plotting
##############################################################
ggplot(data = filtered_lowe,
       aes(x = hitData.coordinates.coordX,
           y = hitData.coordinates.coordY)) +
  geom_hex(bins = 10) +
  geom_curve(x = 33, xend = 223, y = -108, yend = -108,
             curvature = -.65, color = "black") +
  geom_segment(x=33, xend = 128,
               y = -108, yend = -216,
               color = "black") +
  geom_segment(x=223, xend = 128,
               y= -108, yend = -216,
               color = "black") +
  # Infield Line
  geom_curve(x = 83, xend = 173, y = -161, yend = -161,
             curvature = -.65, linetype = "dotted", color = "black") +
  xlim(0, 250) +
  ylim(-250, 0) +
  theme_void() +
  scale_fill_gradient(low = "lightblue",
                      high = "red") +
  labs(title = "Josh Lowe Hitting",
       subtitle = "Results of At Bats at AAA Durham",
       caption = "Data Viz by Billy Fryer (@_b4billy_) | Data from MLB API via baseballr package",
       fill = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))

ggsave("Josh Lowe Callup.jpg")
