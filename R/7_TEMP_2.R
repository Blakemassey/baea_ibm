UpdateAgentStates <- function(agent_states = NULL,
                              sim = sim,
                              init = FALSE) {
  if (init == TRUE) {
    input <- sim$agents$input
    input <- CreateBirthDate(input)
    input_columns <- colnames(input)
    na_columns <- c("start_datetime", "died")
    all <- list()
    for (i in 1:nrow(input)) {
      states <- list()
      for (j in input_columns) states <- append(states, input[i, j])
      for (k in 1:length(na_columns)) states <- append(states, NA)
      states <- setNames(states, c(input_columns, na_columns))
      agent <- NamedList(states)
      all <- append(all, NamedList(agent))
    }
    sim$agents <- append(sim$agents, NamedList(all))
    return(sim)
  } else {
    agent_states <- agent_states
    return(agent_states)
  }
}


library(raster)
x <- raster(matrix(1:(15*25), nrow = 15), xmn = -1000, xmx = 1000,
ymn = -1000, ymx = 1000)
crs(x) <-
plot(x, main="Original")
 plot(RotateRaster(x, 30, 10), main = paste("Rotated by 30 degrees"))
 plot(RotateRaster(x, 75, 10), main = paste("Rotated by 75 degrees"))
 plot(RotateRaster(x, 180, 10), main = paste("Rotated by 180 degrees"))
 plot(RotateRaster(x, 300, 10), main = paste("Rotated by 300 degrees"))
