
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

# Read in Game Logs
game_log17 <- readRDS("game_log_1617.RDS")
game_log16 <- readRDS("game_log_1516.RDS")


# Merge Game Logs
game_log <- bind_rows(game_log17, game_log16)

# Specify variables
names <- names(game_log)
dk <- names[!grepl("fd", names)]
dk_dat <- game_log[,dk]
dk_y <- "dk_pts"
dk_x <- dk_dat %>% select(position:dist_from_last) %>% mutate(position=as.factor(position)) %>% names()


fd <- names[!grepl("dk", names)]
fd_dat <- game_log[,fd]
fd_y <- "fd_pts"
fd_x <- fd_dat %>% select(position:dist_from_last) %>% mutate(position=as.factor(position)) %>% names()



## Model
# Load Data Frame to H2O
gl.hex <- as.h2o(game_log)

# Split into Training, Test, and Validation Frames
split <- h2o.splitFrame(gl.hex,ratios=c(.7,.15))
gl.train <- split[[1]]
gl.test <- split[[2]]
gl.valid <- split[[3]]

# DraftKings Model
dk_auto <- h2o.automl(x=dk_x, y=dk_y,
                      training_frame = gl.train,
                      validation_frame = gl.valid,
                      leaderboard_frame = gl.test,
                      max_runtime_secs = 2000,
                      project_name = "dk_model")
dk_leader <- dk_auto@leader
#dk_leader@model_id <- "dk_leader"
#dk_board <- dk_auto@leaderboard %>% as.data.frame()

fd_auto <- h2o.automl(x=fd_x, y=fd_y,
                      training_frame = gl.train,
                      validation_frame = gl.valid,
                      leaderboard_frame = gl.test,
                      max_runtime_secs = 2000,
                      project_name = "fd_model")

fd_leader <- fd_auto@leader
#fd_leader@model_id <- "fd_leader"
#fd_board <- fd_auto@leaderboard %>% as.data.frame()


## Save Models
h2o.saveModel(dk_leader, path="models", force=TRUE)
h2o.saveModel(fd_leader, path="models", force=TRUE)




# Score & Plot Validation Frame
gl.valid$dk_pts_pred <- h2o.predict(dk_leader, newdata=gl.valid)
gl.valid$fd_pts_pred <- h2o.predict(fd_leader, newdata=gl.valid)

gl_valid <- as.data.frame(gl.valid)
saveRDS(gl_valid, "game_log_validation_frame.RDS")
ggplot(gl_valid, aes(x=dk_pts, y=dk_pts_pred)) +
  geom_point()

library(caret)
postResample(gl_valid$dk_pts_pred, gl_valid$dk_pts)
postResample(gl_valid$fd_pts_pred, gl_valid$fd_pts)


ggplot(gl_valid, aes(x=dk_pts, y=dk_pts_pred)) +
  geom_point()

gl_valid %>%
  sample_n(2000) %>%
  filter(dk_pts>0) %>%
  ggplot(aes(x=fd_pts_pred, y=dk_pts_pred)) +
  geom_point()

