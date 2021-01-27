# Setup different global parameters that will be called throughout
# additional scripts.
default_path <- "Data/"
select_cols <- c("time", "x", "y", "s",
                 "dis", "dir", "event",
                 "nflId", "displayName", "jerseyNumber",
                 "team", "frame.id", "gameId", "playId")
file_list <- list.files(default_path, pattern = "tracking_gameId_[0-9]+.csv")

if(new_age_tracking_data){
  default_path <- "Data_new/"
  select_cols <- c("time", "x", "y", "s",
                   "dis", "dir", "event",
                   "nflId", "displayName", "jerseyNumber",
                   "team", "frameId", "gameId", "playId")
  file_list <- paste0("week", 1:17, ".csv")
}

# Load the games, players, and list of plays in the data set
games <- read_csv(paste0(default_path, "games.csv"), col_types = cols()) %>%
  janitor::clean_names()
players <- read_csv(paste0(default_path, "players.csv"), col_types = cols()) %>%
  janitor::clean_names()
plays <- read_csv(paste0(default_path, "plays.csv"), col_types = cols()) %>%
  janitor::clean_names()

if(!new_age_tracking_data){
  # Change name scheme just to maintain consistency, even though relabeled again below
  players <- players %>%
    unite("display_name", c(first_name, last_name), sep = " ") %>%
    select(nfl_id, display_name, position = position_abbr)
}

players <- players %>%
  dplyr::select(nfl_id, display_name, position)

# Gather positions that I need for analysis, eligible receivers and defensive players
player_pos_id_key <- players %>%
  dplyr::select(nfl_id, position)

route_runners_pos_id_key <- player_pos_id_key %>%
  filter(position %in% c("RB", "FB", "WR", "TE", "OLB", "SS", "ILB", "DE", "CB", "NT",
                         "MLB", "FS", "DT", "LB", "DB"))

# For play standardization
reorient <- FALSE

# Also need the play_ids which are the plays that had passes
play_ids <- plays %>%
  filter(!is.na(pass_result)) %>% 
  select(game_id, play_id, pass_result)

# Generic event labels
pass_air_end <- 
  c(# "pass_arrived",
    "pass_outcome_caught",
    "pass_outcome_incomplete",
    # "pass_tipped",
    "touchdown",
    "pass_outcome_interception",
    "pass_outcome_touchdown"
  )

pass_air_start <-
  c("pass_forward",
    "pass_shovel"
  )

# Plays and games that had specific, unresolvable issues (generally broken tracking data for player/ball)
exempt_plays <- tibble(game_id = c(2017091700,2017091706, 2017091711, 2017092407, 2017092407, 2017091007), 
                       play_id = c(2288, 2126, 3386, 190, 270, 1702))
flog.info('Setup complete for general purpose variables. Defining cluster now.', name = 'par_obs')


games_reduced <- games %>%
  select(game_id, home_team_abbr, visitor_team_abbr)



flog.info(glue('Loading in standard nflscrapR data. Have updated to nflfastR
          and require just minimal changes to adjust. This uses the value
          passed to global parameters `seasons`. Valid options are
          2017 & 2018. Currently selected option is {seasons}'), name = 'comp_prob')

if(seasons == 2018){
  # 2018, or BDB 3, has different plays structure
  nfl_pbp <- plays %>%
    left_join(games_reduced, by = "game_id") %>%
    mutate(yardline_100 = if_else(is.na(absolute_yardline_number),
                                  yardline_number + 
                                    (50 - yardline_number) * as.numeric(yardline_side != possession_team),
                                  # For non-missing, just need to reduce by 10
                                  absolute_yardline_number - 10),
           # Just in case
           yardline_100 = if_else(is.na(yardline_100), 50, yardline_100),
           score_differential = if_else(possession_team == home_team_abbr,
                                        pre_snap_home_score - pre_snap_visitor_score,
                                        -1 * (pre_snap_home_score - pre_snap_visitor_score)),
           is_redzone = factor(yardline_100 < 20, levels = c(FALSE, TRUE))) %>%
    dplyr::select(game_id,
                  play_id, yardline_100, down, ydstogo = yards_to_go,
                  score_differential, is_redzone, number_of_pass_rushers)
} else {
  # 2017, or BDB 1, has some slight modifications needed to generate the above
  nfl_pbp <- plays %>%
    left_join(games_reduced, by = "game_id") %>%
    filter(!is.na(pass_result)) %>%
    mutate(yardline_100 = if_else(possession_team == yardline_side,
                                  100 - yardline_number,
                                  yardline_number),
           yardline_100 = if_else(is.na(yardline_100), 50, yardline_100),
           score_differential = if_else(home_team_abbr == possession_team,
                                        visitor_score_before_play - home_score_before_play,
                                        home_score_before_play - visitor_score_before_play),
           is_redzone = factor(yardline_100 < 20, levels = c(FALSE, TRUE))) %>%
    select(game_id,
           play_id, yardline_100, down, ydstogo = yards_to_go,
           score_differential, is_redzone, number_of_pass_rushers)
}

# Additional pertinent data/ep from nflfastR
nflfastr_stuff <- readRDS(
  url(
    glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{seasons}.rds")
  )
)
