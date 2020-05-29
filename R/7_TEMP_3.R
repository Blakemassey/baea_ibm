CreateBirthDate <- function(input = sim$agents$input){
  # Only proceed if there is no birth_date column
  if(!"birth_date" %in% colnames(input)){
    # Loop through each row in the input
    input_age_period <- sim$pars$global$input_age_period
    birth_day <- sim$pars$global$birth_day
    input <- input %>% add_column(birth_date = NA)
    for(a in 1:nrow(input)){
      # Is the age_period a year?
      if(input_age_period == "year" || input_age_period == "years") {
        # Determine the first sim_start date after the birth_day
        one_year <- as.period(1, "year")
        s0 <- as.Date(sim$pars$global$sim_start - (one_year*input$age[a]))
        # Set the format of the birth_day
        birth_day_format <- tail(guess_formats(birth_day, orders ="dm"), 1)
        birth_day_format <- paste(birth_day_format,"%Y",sep="")
        # Determine the first birth_day after s0
        s1 <- as.Date(paste(birth_day,year(s0),sep=""), format=birth_day_format)
        if(s0 >= s1) {
          input$birth_date[a] <- as.character(s1)
        } else {
          input$birth_date[a] <- as.character(s1-one_year)
        }
      } else {
        # If age period is not a year
        age_period_unit <- as.period(1, input_age_period)
        input$birth_date[a] <- as.character(sim$pars$global$sim_start -
          (age_period_unit*input$age[a]))
      }
    }
  }
  return(input)
}
