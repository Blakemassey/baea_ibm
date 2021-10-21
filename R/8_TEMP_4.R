library(tidyverse)

sim_log <- tibble(
  sim = NA_character_,
  notes = NA_character_) %>%
  slice(0) %>%
  add_row(sim = "20210725-01") %>%
  add_row(notes = "A -> A = 3:1:2 n=50") %>%
  add_row(notes = "S -> A = 3:1:2 n=50") %>%
  add_row(notes = "A -> S = 3:2:1 n=50") %>%
  add_row(notes = "S -> S = 3:2:1 n=50") %>%
  add_row(sim = "20210725-02") %>%
  add_row(notes = "A -> A = 3:1:2 n=100") %>%
  add_row(notes = "S -> A = 3:1:2 n=100") %>%
  add_row(notes = "A -> S = 3:2:1 n=1000") %>%
  add_row(notes = "S -> S = 3:2:1 n=1000") %>%
  add_row(sim = "20210725-03") %>%
  add_row(notes = "A -> A = 3:1:2 n=25") %>%
  add_row(notes = "S -> A = 3:1:2 n=25") %>%
  add_row(notes = "A -> S = 3:2:1 n=250") %>%
  add_row(notes = "S -> S = 3:2:1 n=250") %>%
  add_row(sim = "20210725-04") %>%
  add_row(notes = "A -> A = 2:2:1 n=25") %>%
  add_row(notes = "S -> A = 2:2:1 n=25") %>%
  add_row(notes = "A -> S = 4:2:1 n=250") %>%
  add_row(notes = "S -> S = 4:2:1 n=250") %>%
  add_row(sim = "20210725-05") %>%
  add_row(notes = "A -> A = 4:2:1 n=25") %>%
  add_row(notes = "S -> A = 4:2:1 n=25") %>%
  add_row(notes = "A -> S = 4:2:1 n=250") %>%
  add_row(notes = "S -> S = 4:2:1 n=250") %>%
  add_row(sim = "20210725-06") %>%
  add_row(notes = "A -> A = 6:2:1 n=25") %>%
  add_row(notes = "S -> A = 6:2:1 n=25") %>%
  add_row(notes = "A -> S = 6:2:1 n=250") %>%
  add_row(notes = "S -> S = 6:2:1 n=250") %>%
  add_row(sim = "20210725-07") %>%
  add_row(notes = "A -> A = 3:1:1 n=25") %>%
  add_row(notes = "S -> A = 3:1:1 n=25") %>%
  add_row(notes = "A -> S = 4:1:1 n=250") %>%
  add_row(notes = "S -> S = 4:1:1 n=250") %>%
  add_row(sim = "20210725-08") %>%
  add_row(notes = "A -> A = 2:1:1 n=25") %>%
  add_row(notes = "S -> A = 2:1:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:1 n=250") %>%
  add_row(notes = "S -> S = 5:1:1 n=250") %>%
  add_row(sim = "20210725-09") %>%
  add_row(notes = "A -> A = 2:1:1 n=50") %>%
  add_row(notes = "S -> A = 2:1:1 n=50") %>%
  add_row(notes = "A -> S = 5:1:1 n=500") %>%
  add_row(notes = "S -> S = 5:1:1 n=500") %>%
  add_row(sim = "20210725-10") %>%
  add_row(notes = "A -> A = 2:1:1 n=100") %>%
  add_row(notes = "S -> A = 2:1:1 n=100") %>%
  add_row(notes = "A -> S = 5:1:1 n=1000") %>%
  add_row(notes = "S -> S = 5:1:1 n=1000") %>%
  add_row(sim = "20210725-11") %>%
  add_row(notes = "A -> A = 2:1:1 n=200") %>%
  add_row(notes = "S -> A = 2:1:1 n=200") %>%
  add_row(notes = "A -> S = 5:1:1 n=2000") %>%
  add_row(notes = "S -> S = 5:1:1 n=2000") %>%
  add_row(sim = "20210725-12") %>%
  add_row(notes = "A -> A = 2:1:1 n=300") %>%
  add_row(notes = "S -> A = 2:1:1 n=300") %>%
  add_row(notes = "A -> S = 5:1:1 n=3000") %>%
  add_row(notes = "S -> S = 5:1:1 n=3000") %>%
  add_row(sim = "20210725-13") %>%
  add_row(notes = "A -> A = 3:1:2 n=25") %>%
  add_row(notes = "S -> A = 3:1:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:1 n=1000") %>%
  add_row(notes = "S -> S = 5:1:1 n=1000") %>%
  add_row(sim = "20210725-14") %>%
  add_row(notes = "A -> A = 4:1:2 n=25") %>%
  add_row(notes = "S -> A = 4:1:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:1 n=1000") %>%
  add_row(notes = "S -> S = 5:1:1 n=1000") %>%
  add_row(sim = "20210725-15") %>%
  add_row(notes = "A -> A = 4:1:2 n=25") %>%
  add_row(notes = "S -> A = 4:1:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:1 n=500") %>%
  add_row(notes = "S -> S = 5:1:1 n=500") %>%
  add_row(sim = "20210725-16") %>%
  add_row(notes = "A -> A = 4:1:2 n=50") %>%
  add_row(notes = "S -> A = 4:1:2 n=50") %>%
  add_row(notes = "A -> S = 5:1:1 n=1000") %>%
  add_row(notes = "S -> S = 5:1:1 n=1000") %>%
  add_row(sim = "20210725-17") %>%
  add_row(notes = "A -> A = 4:1:2 n=100") %>%
  add_row(notes = "S -> A = 4:1:2 n=100") %>%
  add_row(notes = "A -> S = 5:1:1 n=1000") %>%
  add_row(notes = "S -> S = 5:1:1 n=1000") %>%
  add_row(sim = "20210725-18") %>%
  add_row(notes = "A -> A = 3:1:2 n=50") %>%
  add_row(notes = "S -> A = 3:1:2 n=50") %>%
  add_row(notes = "A -> S = 5:1:1 n=1000") %>%
  add_row(notes = "S -> S = 5:1:1 n=1000") %>%
  add_row(sim = "20210725-19") %>%
  add_row(notes = "A -> A = 3:1:2 n=50") %>%
  add_row(notes = "S -> A = 3:1:2 n=50") %>%
  add_row(notes = "A -> S = 4:1:1 n=1000") %>%
  add_row(notes = "S -> S = 4:1:1 n=1000") %>%
  add_row(sim = "20210725-20") %>%
  add_row(notes = "A -> A = 3:1:2 n=50") %>%
  add_row(notes = "S -> A = 3:1:2 n=50") %>%
  add_row(notes = "A -> S = 3:1:1 n=1000") %>%
  add_row(notes = "S -> S = 3:1:1 n=1000") %>%
  add_row(sim = "20210725-21") %>%
  add_row(notes = "A -> A = 3:1:2 n=50") %>%
  add_row(notes = "S -> A = 3:1:2 n=50") %>%
  add_row(notes = "A -> S = 3:2:1 n=1000") %>%
  add_row(notes = "S -> S = 3:2:1 n=1000") %>%
  add_row(sim = "20210725-22") %>%
  add_row(notes = "A -> A = 3:2:1 n=25") %>%
  add_row(notes = "S -> A = 3:1:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:1 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-23") %>%
  add_row(notes = "A -> A = 3:3:1 n=25") %>%
  add_row(notes = "S -> A = 3:1:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:1 n=1000") %>%
  add_row(notes = "S -> S = 5:1:3 n=1000") %>%
  add_row(sim = "20210725-24") %>%
  add_row(notes = "A -> A = 3:4:1 n=25") %>%
  add_row(notes = "S -> A = 3:1:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:1 n=1000") %>%
  add_row(notes = "S -> S = 5:1:3 n=1000") %>%
  add_row(sim = "20210725-25") %>%
  add_row(notes = "A -> A = 2:3:1 n=25") %>%
  add_row(notes = "S -> A = 3:1:1 n=25") %>%
  add_row(notes = "A -> S = 4:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:1:3 n=1000") %>%
  add_row(sim = "20210725-26") %>%
  add_row(notes = "A -> A = 3:3:1 n=25") %>%
  add_row(notes = "S -> A = 3:1:1 n=25") %>%
  add_row(notes = "A -> S = 4:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:2:1 n=1000") %>%
  add_row(sim = "20210725-27") %>%
  add_row(notes = "A -> A = 3:1:1 n=10") %>%
  add_row(notes = "S -> A = 3:1:1 n=10") %>%
  add_row(notes = "A -> S = 4:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:2:1 n=1000") %>%
  add_row(sim = "20210725-28") %>%
  add_row(notes = "A -> A = 1:1:1 n=25") %>%
  add_row(notes = "S -> A = 3:2:1 n=25") %>%
  add_row(notes = "A -> S = 4:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:1:1 n=1000") %>%
  add_row(sim = "20210725-29") %>%
  add_row(notes = "A -> A = 1:1:1 n=10") %>%
  add_row(notes = "S -> A = 3:2:1 n=10") %>%
  add_row(notes = "A -> S = 4:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:1:1 n=1000") %>%
  add_row(sim = "20210725-30") %>%
  add_row(notes = "A -> A = 1:2:1 n=10") %>%
  add_row(notes = "S -> A = 3:2:1 n=10") %>%
  add_row(notes = "A -> S = 4:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:1:1 n=1000") %>%
  add_row(sim = "20210725-31") %>%
  add_row(notes = "A -> A = 1:3:1 n=10") %>%
  add_row(notes = "S -> A = 3:2:1 n=10") %>%
  add_row(notes = "A -> S = 4:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:1:1 n=1000") %>%
  add_row(sim = "20210725-32") %>%
  add_row(notes = "A -> A = 1:3:1 n=25") %>%
  add_row(notes = "S -> A = 3:2:1 n=25") %>%
  add_row(notes = "A -> S = 4:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:1:1 n=1000") %>%
  add_row(sim = "20210725-33") %>%
  add_row(notes = "A -> A = 1:3:1 n=1000-1") %>%
  add_row(notes = "S -> A = 3:2:1 n=1000-1") %>%
  add_row(notes = "A -> S = 4:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:1:1 n=1000") %>%
  add_row(sim = "20210725-34") %>%
  add_row(notes = "A -> A = 5:2:1 n=1000-1") %>%
  add_row(notes = "S -> A = 5:2:1 n=1000-1") %>%
  add_row(notes = "A -> S = 4:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:1:1 n=1000") %>%
  add_row(sim = "20210725-35") %>%
  add_row(notes = "A -> A = 2:2:1 n=1000-1") %>%
  add_row(notes = "S -> A = 2:2:1 n=1000-1") %>%
  add_row(notes = "A -> S = 4:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:2:1 n=1000") %>%
  add_row(sim = "20210725-36") %>%
  add_row(notes = "A -> A = 5:2:1 n=1000-100-1") %>%
  add_row(notes = "S -> A = 5:2:1 n=1000-100-1") %>%
  add_row(notes = "A -> S = 4:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:2:1 n=1000") %>%
  add_row(sim = "20210725-37") %>%
  add_row(notes = "A -> A = 5:2:1 n=1000-50-1") %>%
  add_row(notes = "S -> A = 5:2:1 n=1000-50-1") %>%
  add_row(notes = "A -> S = 4:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:2:1 n=1000") %>%
  add_row(sim = "20210725-38") %>%
  add_row(notes = "A -> A = 5:2:1 n=1000-25-1") %>%
  add_row(notes = "S -> A = 5:2:1 n=1000-25-1") %>%
  add_row(notes = "A -> S = 4:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:2:1 n=1000") %>%
  add_row(sim = "20210725-39") %>%
  add_row(notes = "A -> A = 1:5:3 n=25") %>%
  add_row(notes = "S -> A = 3:2:3 n=25") %>%
  add_row(notes = "A -> S = 4:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:1:1 n=1000") %>%
  add_row(sim = "20210725-40") %>%
  add_row(notes = "A -> A = 3:1:2 n=25") %>%
  add_row(notes = "S -> A = 3:1:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:1:1 n=1000") %>%
  add_row(sim = "20210725-41") %>%
  add_row(notes = "A -> C = 1:3:2 n=25") %>%
  add_row(notes = "A -> F = 4:2:1 n=25") %>%
  add_row(notes = "S -> A = 4:2:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:2:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:1 n=1000") %>%
  add_row(sim = "20210725-42") %>%
  add_row(notes = "A -> C = 5:1:1 n=25") %>%
  add_row(notes = "A -> F = 5:1:1 n=25") %>%
  add_row(notes = "S -> A = 5:1:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:1 n=1000") %>%
  add_row(notes = "N -> S = 5:3:1 n=1000") %>%
  add_row(notes = "S -> S = 5:3:1 n=1000")

sim_log <- sim_log %>%
  fill(sim, .direction = "down") %>%
  filter(!is.na(notes))

