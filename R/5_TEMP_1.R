# CreateTimeStepsBAEA <- function(step_interval = step_interval,
#                                 agent = agent,
#                                 sim = sim) {
#   time_step_period = sim$pars$global$time_step_period
#   options(lubridate.verbose=FALSE)
#
#   start_x <- as.numeric(agent$states$start_x)  #CONVERT TO DECIMAL DEGREES!!!
#   start_y <- as.numeric(agent$states$start_y)
#   start_xy_utm<- data.frame(x = agent$states$start_x, y = agent$states$start_y)
#   crs_utm <- "+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#
#   start_pt <- sp::SpatialPoints(start_xy_utm,
#     bbox=NULL, proj4string=sp::CRS("+proj=utm +zone=19 +datum=WGS84"))
#   xy_coords <- sp::spTransform(start_pt, sp::CRS("+proj=longlat +datum=WGS84"))
#   #sp::coordinates(xy_coords)
#   #xy_coords <- matrix(c(-67.7778, 44.8012), nrow = 1)
#   interval_start_East <- force_tz(lubridate::int_start(step_interval),
#     tzone = "US/Eastern")
#   interval_end_East <- force_tz(lubridate::int_end(step_interval)-minutes(1),
#     tzone = "US/Eastern") #subtracting one minute to make it the same day
#   sunrise <- maptools::sunriset(xy_coords, interval_start_East,
#     direction = "sunrise", POSIXct.out = TRUE)[1,2]
#   sunset <- maptools::sunriset(xy_coords, interval_end_East,
#     direction = "sunset", POSIXct.out = TRUE)[1,2]
#   step_interval_start <- round_date(sunrise, "minute") - hours(1)
#   step_interval_end <- round_date(sunset, "minute") + hours(1)
#   end_int <- lubridate::int_end(lubridate::as.interval(time_step_period,
#     step_interval_start))
#   steps <- append(step_interval_start, end_int)
#   while (end_int < step_interval_end) {
#     end_int <- end_int + time_step_period
#     steps <- append(steps, end_int)
#   }
#   if (tail(steps, 1) > step_interval_end) {
#     steps[length(steps)] <- step_interval_end
#   }
#   time_step_list <- list()
#   for (i in 1:(length(steps)-1)) {
#     interval <- lubridate::as.interval(steps[i], steps[i+1])
#     interval@tzone  <- "US/Eastern"
#     time_step_list[[length(time_step_list)+1]] <- interval
#   }
#   return(time_step_list)
# }

# CreateTimeStepsInStepIntervalBAEA <- function(step_interval = step_interval,
#                                           sim = sim) {
#   time_step_period = sim$pars$global$time_step_period
#   options(lubridate.verbose=FALSE)
#   xy_coords <- matrix(c(-67.7778, 44.8012), nrow = 1)
#   interval_start_East <- force_tz(lubridate::int_start(step_interval),
#     tzone = "US/Eastern")
#   interval_end_East <- force_tz(lubridate::int_end(step_interval)-minutes(1),
#     tzone = "US/Eastern") #subtracting one minute to make it the same day
#   sunrise <- maptools::sunriset(xy_coords, interval_start_East,
#     proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunrise",
#     POSIXct.out= TRUE)[1,2]
#   sunset <- maptools::sunriset(xy_coords, interval_end_East,
#     proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunset",
#     POSIXct.out= TRUE)[1,2]
#   step_interval_start <- round_date(sunrise, "minute") - hours(2)
#   step_interval_end <- round_date(sunset, "minute") + hours(2)
#   end_int <- lubridate::int_end(lubridate::as.interval(time_step_period,
#     step_interval_start))
#   steps <- append(step_interval_start, end_int)
#   while (end_int < step_interval_end) {
#     end_int <- end_int + time_step_period
#     steps <- append(steps, end_int)
#   }
#   if (tail(steps, 1) > step_interval_end) {
#     steps[length(steps)] <- step_interval_end
#   }
#   time_step_list <- list()
#   for (i in 1:(length(steps)-1)) {
#     interval <- lubridate::as.interval(steps[i], steps[i+1])
#     interval@tzone  <- "US/Eastern"
#     time_step_list[[length(time_step_list)+1]] <- interval
#   }
#   return(time_step_list)
# }


# BehaviorSubModelBAEA2 <- function(sim = sim,
#                                  agent_states = agent_states,
#                                  step_data = step_data,
#                                  step = step) {
#   sex <- agent_states$sex
#   beta <- as.matrix(sim$pars$classes[[sex]]$constant$fixed$behavior_betas)
#   step_row <- which(step_data$datetime == step)
#   current_behavior <- as.numeric(step_data[step_row, "behavior"])
#   current_time_prop <- as.numeric(step_data[step_row, "time_proportion"])
#   step_data <- as.data.frame(step_data)
#   gamma <- diag(5)
#   g <- beta[1, ]  #  g = state transition probabilities intercepts
#   g <- g +
#     beta[2, ] * cos(2*pi * (step_data[step_row, "julian"]/365)) +
#     beta[3, ] * sin(2*pi * (step_data[step_row, "julian"]/365)) +
#     beta[4, ] * cos(2*pi * (step_data[step_row, "time_proportion"])) +
#     beta[5, ] * sin(2*pi * (step_data[step_row, "time_proportion"]))
#   exp(g)
#   gamma[!gamma] <- exp(g) # Beta values in non-diagonal locations in matrix
#   gamma2 <- t(gamma) # probabilities for state transitions are now in rows
#   gamma3 <- gamma2/apply(gamma2, 1, sum) # rows sum to 1
#   if(current_time_prop <= .5 & current_behavior != 5){
#     gamma3[, 5] <- 0
#     gamma3 <- gamma3/apply(gamma3, 1, sum)
#     gamma3[, 5] <- 0
#   }
#   if(current_time_prop > .5 & current_behavior == 5){
#     gamma3[, 1:4] <- 0
#     gamma3[, 5] <- 1
#     #gamma3 <- gamma3/apply(gamma3, 1, sum)
#   }
#   step_data[step_row + 1, "behavior"] <- sample(1:5, size = 1,
#     prob = gamma3[current_behavior, ])  # trans prob. given behavior
#
#   if(current_time_prop == 1){
#     if (current_behavior %in% c(3,5)){
#       step_data[step_row + 1, "behavior"] <- current_behavior
#     } else {
#       overnight_behavior <- sample(c(3,5), 1)
#       step_data[step_row, "behavior"] <- overnight_behavior
#       step_data[step_row + 1, "behavior"] <- overnight_behavior
#     }
#   }
#   return(step_data)
# }

# LastDailySubModelBAEA <- function(sim = sim,
#                                   agent_states = agent_states,
#                                   step_data = step_data,
#                                   step = step) {
#   step_row <- which(step_data$datetime == step)
#   current_behavior <- as.numeric(step_data[step_row, "behavior"])
#   if (current_behavior %in% c(3,5)){
#     step_data[step_row + 1, "behavior"] <- current_behavior
#   } else {
#     overnight_behavior <- sample(c(3,5), 1)
#     step_data[step_row, "behavior"] <- overnight_behavior
#     step_data[step_row + 1, "behavior"] <- overnight_behavior
#   }
#   return(step_data)
# }

# UpdateAgentStepDataBAEA <- function(step_data = NULL,
#                                     sim = sim,
#                                     init = FALSE,
#                                     rep_intervals = rep_intervals) {
#   if (init == TRUE) {
#     sim_start <- sim$pars$global$sim_start
#     all <- sim$agents$all
#     for (i in 1:length(all)) {
#       agent <- all[[i]]
#       print(paste("Creating initial 'step_data' dataframe for", i, "of",
#         length(all)))
#       all_time_steps <- as.POSIXct(NA)
#       for (j in 1:length(rep_intervals)){
#         #j <- 1
#         step_intervals <- CreateStepIntervals(rep_intervals[[j]])
#         for (k in 1:length(step_intervals)){
#           #k <- 1
#           time_steps <- CreateTimeStepsBAEA(step_intervals[[k]], agent=agent,
#             sim=sim)
#           for (m in 1:length(time_steps)){
#             all_time_steps <- append(all_time_steps, int_start(time_steps[[m]]))
#             if (m == length(time_steps)) all_time_steps <- append(all_time_steps,
#               int_end(time_steps[[m]]))
#           }
#         }
#       }   # this is all about getting 'all_time_steps'
#       if(is.na(all_time_steps[1])) all_time_steps <- all_time_steps[-1]
#       time_steps_df <- data.frame(datetime = all_time_steps) %>%
#         mutate(julian = yday(datetime)) %>%
#         group_by(julian) %>%
#         mutate(day_start = min(datetime),
#           day_end = max(datetime),
#           day_minutes = as.integer(difftime(day_end,day_start,units="mins"))) %>%
#         ungroup() %>%
#         mutate(time_after_start = as.integer(difftime(datetime, day_start,
#           units="mins"))) %>%
#         mutate(time_proportion = time_after_start/day_minutes) %>%
#         dplyr::select(datetime, julian, time_proportion)
#       step_data <- time_steps_df %>%
#         mutate(id=agent$states$id,
#           behavior = NA,
#           x = NA,
#           y = NA,
#           exp_angle = NA,
#           abs_angle = NA) %>%
#         dplyr::select(id, datetime, behavior, x, y, exp_angle, abs_angle,
#           julian, time_proportion) %>%
#         as.data.frame() # when saved as a tibble, broke BehaviorSubModel
#       step_data[1, "behavior"] <- 3
#       step_data[1, "x"] <- agent$states$start_x
#       step_data[1, "y"] <- agent$states$start_y
#       agent  <- append(agent, NamedList(step_data))
#       all[[i]] <- agent
#     }
#     sim$agents$all <- all
#     return(sim)
#   } else {
#     step_data <- step_data
#     return(step_data)
#   }
# }

