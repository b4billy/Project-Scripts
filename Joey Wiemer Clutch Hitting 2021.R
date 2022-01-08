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
  # at_bats <- at_bats %>% 
  #   filter(matchup.batter.fullName == "Joey Wiemer")
  
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
                    ifelse(pitchNumber == max(pitchNumber), "true", "false"),
                  last.pitch.of.ab = factor(last.pitch.of.ab)) %>%
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
                  count.outs.end = count.outs.y) %>% 
    filter(matchup.batter.fullName == "Joey Wiemer")
  
  return(pbp)
}



#####################################################################
### Joey Jacks Data
#####################################################################
mudcats <- map_df(.x = seq.Date(as.Date("2021-01-01"), 
                                as.Date("2021-08-09"), 
                                'day'),
                  .f = get_game_pks,
                  level_ids = 14) %>% 
  filter(away_team == "Carolina Mudcats" | home_team == "Carolina Mudcats") %>% 
  pull(gameid)

timber_rattlers <- map_df(.x = seq.Date(as.Date("2021-08-10"), 
                                        as.Date("2021-09-05"), 
                                        'day'),
                          .f = get_game_pks,
                          level_ids = 13)  %>% 
  filter(away_team == "Wisconsin Timber Rattlers" | 
          home_team == "Wisconsin Timber Rattlers") %>% 
  pull(gameid)

gameid_vec <- c(mudcats, timber_rattlers)

pbp <- map_df(.x = gameid_vec,
              .f = get_pbp_mlb)


clutch <- pbp %>% 
  # Gets Rid of Informational Events
  # Like Pitching Changes
  filter(isPitch) %>% 
  # 9th innning or later
  filter(about.inning >= 9) %>% 
  # arrange by date and order of pitches
  arrange(game_date, about.inning, index) %>%
  # Select Useful Columns
  select(game_date, index, last.pitch.of.ab,
         details.call.description, result.event,
         batting_team, home_level_name, about.inning,
         pitchData.strikeZoneTop, pitchData.strikeZoneBottom,
         pitchData.coordinates.x, pitchData.coordinates.y,
         hitData.coordinates.coordX, hitData.coordinates.coordY
         ) %>% 
  mutate(action.pitch = case_when(last.pitch.of.ab == "true" ~ TRUE,
                                  last.pitch.of.ab == "false" ~ FALSE,
                                  TRUE ~ NA)) %>% 
  # # Only Last Pitch of Atbat
  # filter(action.pitch) %>% 
  # Deselect last.pitch.of.ab
  select(game_date, index, action.pitch, about.inning,
         details.call.description:hitData.coordinates.coordY)

# Flip y coordinate for hitting data
clutch$hitData.coordinates.coordY = -1 * clutch$hitData.coordinates.coordY


temp <- clutch %>% filter(!is.na(hitData.coordinates.coordX))
##############################################################
### Plotting
##############################################################
ggplot(data = clutch,
       aes(x = hitData.coordinates.coordX,
           y = hitData.coordinates.coordY,
           color = result.event,
           shape = home_level_name
           )) +
  geom_point(stroke = 1,
             size = 5) +
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
  labs(title = "Joey Wiemer Clutch Hitting",
       subtitle = "Results of At Bats During or After the 9th Inning as of 9/6/2021",
       caption = "Data Viz by Billy Fryer | Data from MLB API via baseballr package",
       color = "Result",
       shape = "Classification") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))

ggsave("Joey Wiemer.jpg")
