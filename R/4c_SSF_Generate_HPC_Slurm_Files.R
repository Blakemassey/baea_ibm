library(tidyverse)

# Determine the size and structure of the models list (based on 12 covars)
covars <- c("developed", "forest","open_water",
      "eastness", "northness", "wind_class", "tpi", "tri", "roughness",
      "developed_dist", "hydro_dist", "turbine_dist")
list_of_models <- lapply(seq_along((covars)), function(n) {
  preds <- apply(X = combn(covars, n), MARGIN = 2, paste, collapse = " + ")
  paste("case", preds, sep = " ~ ")})
tbl_of_models <- tibble(models = list_of_models)  %>%
  mutate(model_grp = 1:n()) %>%
  mutate(model_n = map_int(models, length)) %>%
  mutate(range_end = cumsum(model_n)) %>%
  mutate(range_start = lag(range_end + 1, default = 1)) %>%
  select(models, model_grp, model_n, range_start, range_end)

est_sec <- c(
  "1" = 1,
  "2" = 50,
  "3" = 400,  # good estimate
  "4" = 2000, # good estimate
  "5" = 7000,
  "6" = 8000,
  "7" = 9000,
  "8" = 10000,
  "9" = 11000,
  "10" = 12000,
  "11" = 13000,
  "12" = 14000
)

desired_hours <- 24

SplitRange <- function(range_start, range_end, length_out){
  splits <- round(seq(from = range_start, to = range_end,
    length.out = length_out))
  return(splits)
}

tbl_of_models_est <- tbl_of_models %>%
  mutate(est_sec = est_sec) %>%
  mutate(total_hours = (model_n * est_sec/60)/60) %>%
  mutate(grp_splits_n = ceiling(total_hours/desired_hours)) %>%
  mutate(grp_hours = ceiling(total_hours/desired_hours)) %>%
  mutate(range_splits = pmap(.l = list(range_start, range_end,
    grp_splits_n), .f = SplitRange))

range_splits_vec <- flatten_dbl(tbl_of_models_est$range_splits) %>%
  as.integer(.)

range_groups <- tibble(range_start = range_splits_vec) %>%
  mutate(range_end = lead(range_start, default =
    max(range_splits_vec) + 1) - 1) %>%
  mutate(range_group = 1:n()) %>%
  select(range_group, range_start, range_end)

tail(range_groups)

range_groups_file <- "E:/Assets/range_groups.rds"
if(!exists(range_groups_file)) saveRDS(range_groups, range_groups_file)

# Create step-type tibble

step_full <- c("cruise_cruise", "cruise_flight", "cruise_perch",
  "flight_cruise", "flight_flight", "flight_perch","flight_roost",
  "nest_cruise", "nest_flight", "nest_perch", "nest_roost", "perch_cruise",
  "perch_flight", "perch_perch", "perch_roost", "roost_flight", "roost_perch")
steps <- tibble(step_full, step_short = NA)

for (i in 1:nrow(steps)){
  str_split_steps <- str_split(steps %>% slice(i) %>% pull(step_full), "_") %>%
    unlist(.)
  steps[i, "step_short"] <- paste0(str_sub(str_split_steps, 1, 1), collapse ="")
}

# Create array slurm files

for (i in 1:nrow(steps)){
  step_type <- steps %>% slice(i) %>% pull(step_short)
  step_data <- steps %>% slice(i) %>% pull(step_full)
  outfile <- paste0("E:/Slurm_Scripts/ssf_", step_type, "_array", ".slurm")
  sink(outfile)
  cat("#!/bin/sh\n")
  cat(paste0("#SBATCH --job-name=ssf_", step_type, "\n"))
  cat(paste0("#SBATCH --output=/home/bmassey/Output/ssf-", step_type ,
    "-%a.out\n"))
  cat("#SBATCH -N 1\n")
  cat("#SBATCH -p normal\n")
  cat("#SBATCH -t 48:00:00\n")
  cat("#SBATCH --account=sciapps\n")
  cat("#SBATCH --array=1-350%100\n")
  cat("#SBATCH --mail-type=FAIL\n")
  cat("#SBATCH --mail-user=blake_massey@fws.gov\n\n")
  cat('echo "SLURM_JOBID: " $SLURM_JOBID\n')
  cat('echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID\n')
  cat('echo "SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID\n\n')
  cat("module purge\n")
  cat("module load openmpi/1.10.2-gcc7.1.0 R/3.6.1-intel2018\n\n")
  cat("srun --mpi=pmi2 Rscript /home/bmassey/R_Scripts/ssf_optimize.R ")
  cat(paste0("/home/bmassey/Data/ua_steps_", step_data, ".rds ",
    "$SLURM_ARRAY_TASK_ID\n"))
  sink()
  Sys.sleep(1)
  txt <- readLines(outfile)
  f <- file(outfile, open = "wb")
  cat(txt, file = f, sep = "\n")
  Sys.sleep(1)
  close(f)
}

# Create individual job slurm files (DEPRECATED APPROACH)

for (j in seq_along(1:nrow(steps))){
  step_type <- steps %>% slice(j) %>% pull(step_short)
  step_data <- steps %>% slice(j) %>% pull(step_full)
  if(!dir.exists(paste0("E:/Slurm_Scripts/", step_type))){
     dir.create(paste0("E:/Slurm_Scripts/", step_type))
    }
  for (i in seq_along(1:nrow(range_groups))){
    range_group_i <- range_groups %>% slice(i) %>% pull(range_group)
    range_group_i_pad <- str_pad(range_group_i, width = 3, side = "left",
      pad = "0")
    outfile <- paste0("E:/Slurm_Scripts/", step_type, "/ssf_", step_type, "_",
      range_group_i_pad, ".slurm")
    sink(outfile)
    cat("#!/bin/sh\n")
    cat("\n")
    cat(paste0("#SBATCH --job-name=", step_type, "_", range_group_i_pad, "\n"))
    cat(paste0("#SBATCH --output=/home/bmassey/Output/ssf-", step_type ,"-",
      range_group_i_pad, "-%j.out\n"))
    cat("#SBATCH -N 1\n")
    cat("#SBATCH -p normal\n")
    cat("#SBATCH -t 48:00:00\n")
    cat("#SBATCH --account=sciapps\n")
    cat("#SBATCH --mail-type=FAIL	# BEGIN, END, FAIL, REQUEUE, and ALL\n")
    cat("#SBATCH --mail-user=blake_massey@fws.gov\n")
    cat("\n")
    cat("module purge\n")
    cat("module load openmpi/1.10.2-gcc7.1.0 R/3.6.1-intel2018\n")
    cat("\n")
    cat("srun --mpi=pmi2 Rscript /home/bmassey/R_Scripts/ssf_optimize.R ")
    cat(paste0("/home/bmassey/Data/ua_steps_", step_data, ".rds ",
      range_group_i, "\n"))
    sink()
    Sys.sleep(1)
    txt <- readLines(outfile)
    f <- file(outfile, open = "wb")
    cat(txt, file = f, sep = "\n")
    Sys.sleep(1)
    close(f)
  }
}


