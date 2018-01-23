
library(plyr)
library(tidyverse)
library(ggmap)
library(geosphere)
library(RcppRoll)
library(stattleshipR)
library(reshape2)
roll5=function(x){roll_mean(x, n=5, align="right", fill=0)}
stattleshipR::set_token('6f21357b4f89eec0deb10adfe0f813d5')
library(h2o)
h2o.init(nthreads=-1)


gl <- ss_get_result(sport = "basketball",
                    league = "nba",
                    ep = "game_logs",
                    query = list(season_id="nba-2017-2018", interval_type="regularseason",
                                 per_page=200, since="4 weeks ago"),
                    walk=TRUE)
#saveRDS(gl, "game_log_today.RDS")

#gl <- readRDS("game_log_today.RDS")


position_abbreviation <- c("C", "C-F", "F", "F-C", "F-G", "G", "G-F", "PF", "PG", "SF", "SG")
position <- c("c", "pf", "sf", "pf", "sf", "pg", "sg", "pf", "pg", "sf", "sg")
position_adj <- data.frame(position_abbreviation, position) %>% mutate_all(funs(as.character))

games_ <- ldply(gl, function(x) x$games)
teams_ <- ldply(gl, function(x) x$teams) %>%
  select(id, location, nickname, latitude, longitude) %>%
  distinct()
players <- ldply(gl,  function(x) x$players) %>%
  select(id, name, position_abbreviation) %>%
  left_join(position_adj) %>%
  distinct()
game_logs_ <- ldply(gl, function(x) x$game_logs) 


## Game and Opponent Lookup
games <- games_ %>%
  mutate(date=as.POSIXct(timestamp, origin="1970-01-01", tz="EST")) %>%
  left_join(teams_, by=c("home_team_id"="id")) %>%
  rename(gm_latitude=latitude, gm_longitude=longitude) %>%
  select(id, slug, home_team_id, away_team_id, date, gm_latitude, gm_longitude) %>%
  gather(home_team_id, away_team_id, key="home_away", value="team_id") %>%
  arrange(team_id, date) %>%
  left_join(teams_, by=c("team_id"="id")) %>%
  group_by(team_id) %>%
  mutate(hours_since_last=as.numeric(difftime(date, lag(date), units="hours")),
         dist_from_home=distHaversine(cbind(longitude, latitude),cbind(gm_longitude,gm_latitude))/1609.34,
         dist_from_last=distHaversine(cbind(gm_longitude, gm_latitude),cbind(lag(gm_longitude),lag(gm_latitude)))/1609.34) %>%
  filter(!is.na(gm_latitude)) 

games <- games %>%
  select(id, team_id) %>%
  rename(opponent_id=team_id) %>%
  right_join(games, by="id") %>%
  filter(team_id != opponent_id) %>%
  distinct() %>%
  ungroup()


game_context <- games %>%
  select(id, team_id, date, hours_since_last, dist_from_home, dist_from_last) %>%
  distinct()

# Modeling Individual Performance
plyr_pts <- game_logs_ %>%
  mutate(dk_pts=1*points + .5*three_pointers_made + 1.25*rebounds_total + 1.5*assists +
           2*steals + 2*blocks -.5*turnovers + 1.5*double_double + 3*triple_double,
         fd_pts=1*points + 1.2*rebounds_total + 1.5*assists +
           3*steals + 3*blocks -1*turnovers) %>%
  distinct() %>%
  left_join(players, by=c("player_id"="id")) %>%
  left_join(games, by=c("game_id"="id", "team_id", "opponent_id")) %>%
  arrange(player_id, date) %>%
  group_by(player_id) %>%
  mutate(dk_lag1=dk_pts, dk_lag2=lag(dk_lag1), dk_lag3=lag(dk_lag2),
         dk_lag4=lag(dk_lag3), dk_lag5=lag(dk_lag4),
         fd_lag1=fd_pts, fd_lag2=lag(fd_lag1), fd_lag3=lag(fd_lag2),
         fd_lag4=lag(fd_lag3), fd_lag5=lag(fd_lag4),
         game_id_next=lead(game_id)) %>%
  select(player_id, team_id, game_id_next, position, dk_lag1:fd_lag5) %>%
  distinct() %>%
  filter(is.na(game_id_next)) # Gets the current state



# Team Total Points by Position
tm_pts <- game_logs_ %>%
  mutate(dk_pts=1*points + .5*three_pointers_made + 1.25*rebounds_total + 1.5*assists +
           2*steals + 2*blocks -.5*turnovers + 1.5*double_double + 3*triple_double,
         fd_pts=1*points + 1.2*rebounds_total + 1.5*assists +
           3*steals + 3*blocks -1*turnovers) %>%
  distinct() %>% 
  left_join(players, by=c("player_id"="id")) %>%
  left_join(games, by=c("game_id"="id", "team_id", "opponent_id")) %>%
  group_by(team_id, game_id, date, position) %>%
  summarize(dk_pts=sum(dk_pts), fd_pts=sum(fd_pts)) %>%
  ungroup() %>%
  arrange(team_id, date) %>%
  group_by(team_id, position) %>%
  mutate(dk_lag1=dk_pts, dk_lag2=lag(dk_lag1), dk_lag3=lag(dk_lag2),
         dk_lag4=lag(dk_lag3), dk_lag5=lag(dk_lag4),
         fd_lag1=fd_pts, fd_lag2=lag(fd_lag1), fd_lag3=lag(fd_lag2),
         fd_lag4=lag(fd_lag3), fd_lag5=lag(fd_lag4),
         game_id_next=lead(game_id)) %>%
  ungroup() %>%
  select(game_id_next, team_id, position, dk_lag1:fd_lag5) %>%
  #filter(complete.cases(.)) %>%
  gather(key="lag",value="points", dk_lag1:fd_lag5) %>%
  mutate(position=tolower(position)) %>%
  unite(pos_site_lag, position, lag) %>%
  spread(pos_site_lag, points, fill=0)  %>%
  rename_at(vars(c_dk_lag1:sg_fd_lag5),  funs(paste("tm",.,sep="_"))) %>%
  filter(is.na(game_id_next)) # Gets the current state

# Team Allowed Merging
# Lookup File
tm_allow_lookup <- game_logs_ %>%
  select(game_id, team_id, opponent_id) %>%
  distinct()

# Team Performance
tm_allow_pts <- game_logs_ %>%
  mutate(dk_pts=1*points + .5*three_pointers_made + 1.25*rebounds_total + 1.5*assists +
           2*steals + 2*blocks -.5*turnovers + 1.5*double_double + 3*triple_double,
         fd_pts=1*points + 1.2*rebounds_total + 1.5*assists +
           3*steals + 3*blocks -1*turnovers) %>%
  distinct() %>% 
  left_join(players, by=c("player_id"="id")) %>%
  left_join(games, by=c("game_id"="id", "team_id", "opponent_id")) %>%
  group_by(team_id, game_id, date, position) %>%
  summarize(dk_pts=sum(dk_pts), fd_pts=sum(fd_pts)) %>%
  right_join(tm_allow_lookup, by=c("team_id"="opponent_id", "game_id")) %>%
  ungroup() %>%
  arrange(team_id, date) %>%
  group_by(team_id, position) %>%
  mutate(dk_lag1=dk_pts, dk_lag2=lag(dk_lag1), dk_lag3=lag(dk_lag2),
         dk_lag4=lag(dk_lag3), dk_lag5=lag(dk_lag4),
         fd_lag1=fd_pts, fd_lag2=lag(fd_lag1), fd_lag3=lag(fd_lag2),
         fd_lag4=lag(fd_lag3), fd_lag5=lag(fd_lag4),
         game_id_next=lead(game_id)) %>%
  ungroup() %>%
  select(game_id_next, team_id, position, dk_lag1:fd_lag5) %>%
  #filter(complete.cases(.)) %>%
  gather(key="lag",value="points", dk_lag1:fd_lag5) %>%
  mutate(position=tolower(position)) %>%
  unite(pos_site_lag, position, lag) %>%
  spread(pos_site_lag, points, fill=0) %>%
  rename_at(vars(c_dk_lag1:sg_fd_lag5),  funs(paste("allow",.,sep="_"))) %>%
  filter(is.na(game_id_next)) # Gets the current state


## Get Todays Games
gm_today_ls <- ss_get_result(sport="basketball",
                             league="nba",
                             ep="games",
                             query=list(on="today"))

games_today <- ldply(gm_today_ls, function(x) x$games)

games <- games_ %>%
  bind_rows(games_today) %>%
  mutate(date=as.POSIXct(timestamp, origin="1970-01-01", tz="EST")) %>%
  left_join(teams_, by=c("home_team_id"="id")) %>%
  rename(gm_latitude=latitude, gm_longitude=longitude) %>%
  select(id, slug, home_team_id, away_team_id, date, gm_latitude, gm_longitude) %>%
  gather(home_team_id, away_team_id, key="home_away", value="team_id") %>%
  arrange(team_id, date) %>%
  left_join(teams_, by=c("team_id"="id")) %>%
  group_by(team_id) %>%
  mutate(hours_since_last=as.numeric(difftime(date, lag(date), units="hours")),
         dist_from_home=distHaversine(cbind(longitude, latitude),cbind(gm_longitude,gm_latitude))/1609.34,
         dist_from_last=distHaversine(cbind(gm_longitude, gm_latitude),cbind(lag(gm_longitude),lag(gm_latitude)))/1609.34) %>%
  filter(!is.na(gm_latitude)) 

games <- games %>%
  select(id, team_id) %>%
  rename(opponent_id=team_id) %>%
  right_join(games, by="id") %>%
  filter(team_id != opponent_id) %>%
  distinct() %>%
  ungroup()

game_id_today <- games_today %>% select(id)

game_context_today <- games %>%
  select(id, team_id, opponent_id, date, hours_since_last, dist_from_home, dist_from_last) %>%
  distinct() %>%
  inner_join(game_id_today)

## Join Together
game_log_today <- game_context_today %>%
  left_join(plyr_pts, by=c("team_id")) %>%
  left_join(tm_pts, by=c("team_id")) %>%
  left_join(tm_allow_pts, by=c("opponent_id"="team_id")) %>%
  select(-date)

dk_lead <- h2o.loadModel(path="./models/StackedEnsemble_AllModels_0_AutoML_20180122_184910")
fd_lead <- h2o.loadModel(path="./models/StackedEnsemble_AllModels_0_AutoML_20180122_192336")

glt.hex <- as.h2o(game_log_today)

glt.hex$dk_pred <- h2o.predict(dk_lead, newdata=glt.hex)
glt.hex$fd_pred <- h2o.predict(fd_lead, newdata=glt.hex)

pred_today <- as.data.frame(glt.hex) %>%
  left_join(players, by=c("player_id"="id")) %>%
  select(name, dk_pred, fd_pred)

## Plot Salaries
pred_today %>%
  filter(dk_pred>10) %>%
  ggplot(aes(x=dk_pred, y=fd_pred, label=name)) +
  geom_label()

dk_sal <- read_csv("dk_salaries_20180122.csv") %>%
  select(Name, Salary, Position) %>%
  rename(name=Name, dk_salary=Salary, dk_position=Position)
fd_sal <- read_csv("fd_salaries_20180122.csv") %>%
  select(Nickname, Salary, Position) %>%
  rename(name=Nickname, fd_salary=Salary, fd_position=Position)


pred_today_1 <- pred_today %>%
  left_join(dk_sal, by="name") %>%
  left_join(fd_sal, by="name") %>%
  mutate(dk_effic=dk_pred/dk_salary*1000,
         fd_effic=fd_pred/fd_salary*1000)

a <- pred_today_1 %>%
  filter(fd_sal>4)
  filter(grepl("Warren", name))



