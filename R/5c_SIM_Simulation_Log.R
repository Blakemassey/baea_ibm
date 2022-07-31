# Sim Run Parameters -----------------------------------------------------------

pacman::p_load(tidyverse)

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
  add_row(notes = "S -> S = 5:3:1 n=1000") %>%
  add_row(sim = "20210725-43") %>%
  add_row(notes = "A -> C = 3:3:1 n=25") %>%
  add_row(notes = "A -> F = 3:3:1 n=25") %>%
  add_row(notes = "S -> A = 3:3:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:1:3 n=1000") %>%
  add_row(sim = "20210725-44") %>%
  add_row(notes = "A -> C = 2:2:1 n=25") %>%
  add_row(notes = "A -> F = 2:2:1 n=25") %>%
  add_row(notes = "S -> A = 2:2:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:1:3 n=1000") %>%
  add_row(sim = "20210725-45") %>%
  add_row(notes = "A -> C = 2:1:1 n=25") %>%
  add_row(notes = "A -> F = 2:1:1 n=25") %>%
  add_row(notes = "S -> A = 2:1:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:1:3 n=1000") %>%
  add_row(sim = "20210725-46") %>%
  add_row(notes = "A -> C = 3:2:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "S -> A = 3:2:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:1:3 n=1000") %>%
  add_row(sim = "20210725-47") %>%
  add_row(notes = "A -> C = 3:2:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "S -> A = 3:2:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:2 n=1000") %>%
  add_row(notes = "N -> S = 5:1:2 n=1000") %>%
  add_row(notes = "S -> S = 5:1:2 n=1000") %>%
  add_row(sim = "20210725-48") %>%
  add_row(notes = "A -> C = 3:2:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "S -> A = 3:2:1 n=25") %>%
  add_row(notes = "A -> S = 5:2:1 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:1:3 n=1000") %>%
  add_row(sim = "20210725-49") %>%
  add_row(notes = "A -> C = 3:2:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "S -> A = 3:2:1 n=25") %>%
  add_row(notes = "A -> S = 5:2:1 n=1000") %>%
  add_row(notes = "N -> S = 5:3:1 n=1000") %>%
  add_row(notes = "S -> S = 5:1:3 n=1000") %>%
  add_row(sim = "20210725-50") %>%
  add_row(notes = "A -> C = 3:2:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "S -> A = 3:2:1 n=25") %>%
  add_row(notes = "A -> S = 5:2:1 n=1000") %>%
  add_row(notes = "N -> S = 5:2:1 n=1000") %>%
  add_row(notes = "S -> S = 5:1:3 n=1000") %>%
  add_row(sim = "20210725-51") %>%
  add_row(notes = "A -> C = 3:3:1 n=25") %>%
  add_row(notes = "A -> F = 3:3:1 n=25") %>%
  add_row(notes = "S -> A = 3:3:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1500") %>%
  add_row(notes = "N -> S = 5:1:3 n=1500") %>%
  add_row(notes = "S -> S = 5:1:3 n=1500") %>%
  add_row(sim = "20210725-52") %>%
  add_row(notes = "A -> C = 3:3:1 n=25") %>%
  add_row(notes = "A -> F = 3:3:1 n=25") %>%
  add_row(notes = "S -> A = 3:3:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1500") %>%
  add_row(notes = "N -> S = 5:1:3 n=1500") %>%
  add_row(notes = "S -> S = 5:2:3 n=1500") %>%
  add_row(sim = "20210725-53") %>%
  add_row(notes = "A -> C = 3:3:1 n=25") %>%
  add_row(notes = "A -> F = 3:3:1 n=25") %>%
  add_row(notes = "S -> A = 3:3:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1500") %>%
  add_row(notes = "N -> S = 5:2:3 n=1500") %>%
  add_row(notes = "S -> S = 5:2:3 n=1500") %>%
  add_row(sim = "20210725-54") %>%
  add_row(notes = "A -> C = 3:2:1 n=25") %>%
  add_row(notes = "A -> F = 3:3:1 n=25") %>%
  add_row(notes = "S -> A = 3:3:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1500") %>%
  add_row(notes = "N -> S = 5:2:3 n=1500") %>%
  add_row(notes = "S -> S = 5:2:3 n=1500") %>%
  add_row(sim = "20210725-55") %>%
  add_row(notes = "A -> C = 2:2:1 n=25") %>%
  add_row(notes = "A -> F = 3:3:1 n=25") %>%
  add_row(notes = "S -> A = 3:3:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1500") %>%
  add_row(notes = "N -> S = 5:2:3 n=1500") %>%
  add_row(notes = "S -> S = 5:2:3 n=1500") %>%
  add_row(sim = "20210725-56") %>%
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:3:1 n=25") %>%
  add_row(notes = "S -> A = 3:3:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1500") %>%
  add_row(notes = "N -> S = 5:2:3 n=1500") %>%
  add_row(notes = "S -> S = 5:2:3 n=1500") %>%
  add_row(sim = "20210725-57") %>%
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:3:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:4:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1500") %>%
  add_row(notes = "N -> S = 5:2:3 n=1500") %>%
  add_row(notes = "S -> S = 5:2:3 n=1500") %>%
  add_row(sim = "20210725-58") %>%
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:4:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1500") %>%
  add_row(notes = "N -> S = 5:2:3 n=1500") %>%
  add_row(notes = "S -> S = 5:2:3 n=1500") %>%
  add_row(sim = "20210725-59") %>%
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:3 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:4:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1500") %>%
  add_row(notes = "N -> S = 5:2:3 n=1500") %>%
  add_row(notes = "S -> S = 5:2:3 n=1500") %>%
  add_row(sim = "20210725-60") %>%
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:3:2 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:4:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1500") %>%
  add_row(notes = "N -> S = 5:2:3 n=1500") %>%
  add_row(notes = "S -> S = 5:2:3 n=1500") %>%
  add_row(sim = "20210725-61") %>%
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:2 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:4:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1500") %>%
  add_row(notes = "N -> S = 5:2:3 n=1500") %>%
  add_row(notes = "S -> S = 5:2:3 n=1500") %>%
  add_row(sim = "20210725-62") %>%
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:3:2 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:4:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1500") %>%
  add_row(sim = "20210725-63") %>%
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:3:2 n=100") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:4:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1500") %>%
  add_row(sim = "20210725-64") %>%
  add_row(notes = "A -> C = 3:4:1 n=100") %>%
  add_row(notes = "A -> F = 3:3:2 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:4:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1500") %>%
  add_row(sim = "20210725-65") %>%
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:3:2 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=100") %>%
  add_row(notes = "N -> A = 3:4:2 n=100") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1500") %>%
  add_row(sim = "20210725-66") %>%
  add_row(notes = "A -> C = 3:3:1 n=25") %>%
  add_row(notes = "A -> F = 3:3:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:4:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1500") %>%
  add_row(sim = "20210725-67") %>%
  add_row(notes = "A -> C = 3:4:2 n=25") %>%
  add_row(notes = "A -> F = 3:3:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:4:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1500") %>%
  add_row(sim = "20210725-68") %>%
  add_row(notes = "A -> C = 3:4:2 n=25") %>%
  add_row(notes = "A -> F = 3:3:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:4:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-69") %>%
  add_row(notes = "A -> C = 3:4:2 n=25") %>%
  add_row(notes = "A -> F = 3:3:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:4:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-70") %>%
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:4:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-71") %>%
  add_row(notes = "A -> C = 3:4:1 n=50") %>%
  add_row(notes = "A -> F = 3:2:1 n=50") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=50") %>%
  add_row(notes = "N -> A = 3:4:2 n=50") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-72") %>%
  add_row(notes = "A -> C = 3:4:1 n=100") %>%
  add_row(notes = "A -> F = 3:2:1 n=100") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=100") %>%
  add_row(notes = "N -> A = 3:4:2 n=100") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-73") %>%
  add_row(notes = "A -> C = 3:4:1 n=100") %>%
  add_row(notes = "A -> F = 3:2:1 n=100") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:4:2 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-74") %>%
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=100") %>%
  add_row(notes = "N -> A = 3:4:2 n=100") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-75") %>%
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:4:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-76") %>%
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 2:4:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-77") %>%
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 2:3:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-78") %>%
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:3:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-79") %>%
  add_row(notes = "A -> C = 3:4:1 n=10") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:3:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-80") %>%
  add_row(notes = "A -> C = 3:4:1 n=50") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:3:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-81") %>%  # REPEAT OF 78
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:3:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-82") %>%  # REPEAT OF 78
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:3:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-83") %>%  # REPEAT OF 78
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:3:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  tidyr::fill(sim, .direction = "down") %>%
  add_row(sim = "20210725-84") %>%  # REPEAT OF 78
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 3:3:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-85") %>%  # REPEAT OF 77
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 2:3:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-86") %>%  # REPEAT OF 77
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 2:3:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-87") %>%  # REPEAT OF 77
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 2:3:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-88") %>%  # REPEAT OF 77
  add_row(notes = "A -> C = 3:4:1 n=25") %>%
  add_row(notes = "A -> F = 3:2:1 n=25") %>%
  add_row(notes = "P/R -> A = 3:3:2 n=25") %>%
  add_row(notes = "N -> A = 2:3:1 n=25") %>%
  add_row(notes = "A -> S = 5:1:3 n=1000") %>%
  add_row(notes = "N -> S = 5:1:3 n=1000") %>%
  add_row(notes = "S -> S = 5:2:3 n=1000") %>%
  add_row(sim = "20210725-89") %>%  # REPEAT OF 77 - NEW SAMPLE METHOD
  add_row(notes = "A -> C = 3:4:1") %>%
  add_row(notes = "A -> F = 3:2:1") %>%
  add_row(notes = "P/R -> A = 3:3:2") %>%
  add_row(notes = "N -> A = 2:3:1") %>%
  add_row(notes = "A -> S = 5:1:3") %>%
  add_row(notes = "N -> S = 5:1:3") %>%
  add_row(notes = "S -> S = 5:2:3") %>%
  add_row(notes = "Prob = c(1:10/10)") %>%
  add_row(sim = "20210725-90") %>%  # REPEAT OF 77 - NEW SAMPLE METHOD
  add_row(notes = "A -> C = 3:4:1") %>%
  add_row(notes = "A -> F = 3:2:1") %>%
  add_row(notes = "P/R -> A = 3:3:2") %>%
  add_row(notes = "N -> A = 2:3:1") %>%
  add_row(notes = "A -> S = 5:1:3") %>%
  add_row(notes = "N -> S = 5:1:3") %>%
  add_row(notes = "S -> S = 5:2:3") %>%
  add_row(notes = "Prob = c(1:10/10)") %>%
  add_row(sim = "20210725-91") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 3:4:1") %>%
  add_row(notes = "A -> F = 3:2:1") %>%
  add_row(notes = "P/R -> A = 3:3:2") %>%
  add_row(notes = "N -> A = 2:3:1") %>%
  add_row(notes = "A -> S = 10:1:3") %>%
  add_row(notes = "N -> S = 10:1:3") %>%
  add_row(notes = "S -> S = 10:2:3") %>%
  add_row(notes = "Prob = c(1:10/10)") %>%
  add_row(sim = "20210725-92") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 3:4:1") %>%
  add_row(notes = "A -> F = 3:2:1") %>%
  add_row(notes = "P/R -> A = 3:3:2") %>%
  add_row(notes = "N -> A = 2:3:1") %>%
  add_row(notes = "A -> S = 5:1:10") %>%
  add_row(notes = "N -> S = 5:1:10") %>%
  add_row(notes = "S -> S = 5:2:10") %>%
  add_row(notes = "Prob = c(1:10/10)") %>%
  add_row(sim = "20210725-93") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 3:4:5") %>%
  add_row(notes = "A -> F = 3:2:5") %>%
  add_row(notes = "P/R -> A = 3:3:5") %>%
  add_row(notes = "N -> A = 2:3:5") %>%
  add_row(notes = "A -> S = 5:1:10") %>%
  add_row(notes = "N -> S = 5:1:10") %>%
  add_row(notes = "S -> S = 5:2:10") %>%
  add_row(notes = "Prob = c(1:10/10)") %>%
  add_row(sim = "20210725-94") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 2:1:1") %>%
  add_row(notes = "A -> F = 2:1:1") %>%
  add_row(notes = "P/R -> A = 2:1:1") %>%
  add_row(notes = "N -> A = 2:1:1") %>%
  add_row(notes = "A -> S = 2:1:1") %>%
  add_row(notes = "N -> S = 2:1:1") %>%
  add_row(notes = "S -> S = 2:1:1") %>%
  add_row(notes = "Prob = c(1:10/10)") %>%
  add_row(sim = "20210725-95") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 2:1:1") %>%
  add_row(notes = "A -> F = 2:1:1") %>%
  add_row(notes = "P/R -> A = 2:1:1") %>%
  add_row(notes = "N -> A = 2:1:1") %>%
  add_row(notes = "A -> S = 2:1:1") %>%
  add_row(notes = "N -> S = 2:1:1") %>%
  add_row(notes = "S -> S = 2:1:1") %>%
  add_row(notes = "Prob = c(1:10)^2/100") %>%
  add_row(sim = "20210725-96") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 2:1:1") %>%
  add_row(notes = "A -> F = 2:1:1") %>%
  add_row(notes = "P/R -> A = 2:1:1") %>%
  add_row(notes = "N -> A = 2:1:1") %>%
  add_row(notes = "A -> S = 5:1:3") %>%
  add_row(notes = "N -> S = 5:1:3") %>%
  add_row(notes = "S -> S = 5:1:3") %>%
  add_row(notes = "Prob = c(1:10)^2/100") %>%
  add_row(sim = "20210725-97") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 2:1:1") %>%
  add_row(notes = "A -> F = 2:1:1") %>%
  add_row(notes = "P/R -> A = 2:1:1") %>%
  add_row(notes = "N -> A = 2:1:1") %>%
  add_row(notes = "A -> S = 5:1:3") %>%
  add_row(notes = "N -> S = 5:1:3") %>%
  add_row(notes = "S -> S = 5:1:3") %>%
  add_row(notes = "Prob = c(2^(1:10)/100))") %>%
  add_row(sim = "20210725-98") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 3:4:1") %>%
  add_row(notes = "A -> F = 3:2:1") %>%
  add_row(notes = "P/R -> A = 3:3:2") %>%
  add_row(notes = "N -> A = 2:3:1") %>%
  add_row(notes = "A -> S = 5:1:3") %>%
  add_row(notes = "N -> S = 5:1:3") %>%
  add_row(notes = "S -> S = 5:1:3") %>%
  add_row(notes = "Prob = c(2^(1:10)/100))") %>%
  add_row(sim = "20210725-99") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 3:4:1") %>%
  add_row(notes = "A -> F = 3:2:1") %>%
  add_row(notes = "P/R -> A = 3:3:2") %>%
  add_row(notes = "N -> A = 2:3:1") %>%
  add_row(notes = "A -> S = 10:1:3") %>%
  add_row(notes = "N -> S = 10:1:3") %>%
  add_row(notes = "S -> S = 10:1:3") %>%
  add_row(notes = "Prob = c(2^(1:10)/100))") %>%
  add_row(sim = "20210725-100") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 3:4:1") %>%
  add_row(notes = "A -> F = 3:2:1") %>%
  add_row(notes = "P/R -> A = 3:3:2") %>%
  add_row(notes = "N -> A = 2:3:1") %>%
  add_row(notes = "A -> S = 4:1:1") %>%
  add_row(notes = "N -> S = 4:1:1") %>%
  add_row(notes = "S -> S = 4:1:1") %>%
  add_row(notes = "Prob = c(2^(1:10)/100))") %>%
  add_row(sim = "20210725-101") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 3:4:1") %>%
  add_row(notes = "A -> F = 3:2:1") %>%
  add_row(notes = "P/R -> A = 3:3:2") %>%
  add_row(notes = "N -> A = 2:3:1") %>%
  add_row(notes = "A -> S = 8:1:1") %>%
  add_row(notes = "N -> S = 8:1:1") %>%
  add_row(notes = "S -> S = 8:1:1") %>%
  add_row(notes = "Prob = c(2^(1:10)/100))") %>%
  add_row(sim = "20210725-102") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 3:4:1") %>%
  add_row(notes = "A -> F = 3:2:1") %>%
  add_row(notes = "P/R -> A = 3:3:2") %>%
  add_row(notes = "N -> A = 2:3:1") %>%
  add_row(notes = "A -> S = 8:1:3") %>%
  add_row(notes = "N -> S = 8:1:3") %>%
  add_row(notes = "S -> S = 8:1:3") %>%
  add_row(notes = "Prob = c(2^(1:10)/100))") %>%
  add_row(sim = "20210725-103") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 3:4:1") %>%
  add_row(notes = "A -> F = 3:2:1") %>%
  add_row(notes = "P/R -> A = 3:3:2") %>%
  add_row(notes = "N -> A = 2:3:1") %>%
  add_row(notes = "A -> S = 2:1:1") %>%
  add_row(notes = "N -> S = 2:1:1") %>%
  add_row(notes = "S -> S = 2:1:1") %>%
  add_row(notes = "Prob = c(2^(1:10)/100))") %>%
  add_row(sim = "20210725-104") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 3:4:1") %>%
  add_row(notes = "A -> F = 3:2:1") %>%
  add_row(notes = "P/R -> A = 3:3:2") %>%
  add_row(notes = "N -> A = 2:3:1") %>%
  add_row(notes = "A -> S = 2:1:2") %>%
  add_row(notes = "N -> S = 2:1:2") %>%
  add_row(notes = "S -> S = 2:1:2") %>%
  add_row(notes = "Prob = c(2^(1:10)/100))") %>%
  add_row(sim = "20210725-105") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 3:4:1") %>%
  add_row(notes = "A -> F = 3:2:1") %>%
  add_row(notes = "P/R -> A = 3:3:2") %>%
  add_row(notes = "N -> A = 2:3:1") %>%
  add_row(notes = "A -> S = 3:1:1") %>%
  add_row(notes = "N -> S = 3:1:1") %>%
  add_row(notes = "S -> S = 3:1:1") %>%
  add_row(notes = "Prob = c(2^(1:10)/100))") %>%
  add_row(sim = "20210725-106") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 3:4:1") %>%
  add_row(notes = "A -> F = 3:2:1") %>%
  add_row(notes = "P/R -> A = 3:3:2") %>%
  add_row(notes = "N -> A = 2:3:1") %>%
  add_row(notes = "A -> S = 10:1:1") %>%
  add_row(notes = "N -> S = 10:1:1") %>%
  add_row(notes = "S -> S = 10:1:1") %>%
  add_row(notes = "Prob = c(2^(1:10)/100))") %>%
  add_row(sim = "20210725-107") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 3:4:1") %>%
  add_row(notes = "A -> F = 3:2:1") %>%
  add_row(notes = "P/R -> A = 3:3:2") %>%
  add_row(notes = "N -> A = 2:3:1") %>%
  add_row(notes = "A -> S = 5:1:1") %>%
  add_row(notes = "N -> S = 5:1:1") %>%
  add_row(notes = "S -> S = 5:1:1") %>%
  add_row(notes = "Prob = c(2^(1:10)/100))") %>%
  add_row(sim = "20210725-108") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 1:1:1") %>%
  add_row(notes = "A -> F = 1:1:1") %>%
  add_row(notes = "P/R -> A = 1:1:1") %>%
  add_row(notes = "N -> A = 1:1:1") %>%
  add_row(notes = "A -> S = 3:1:1") %>%
  add_row(notes = "N -> S = 3:1:1") %>%
  add_row(notes = "S -> S = 3:1:1") %>%
  add_row(notes = "Prob = c(2^(1:10)/100))") %>%
  add_row(sim = "20210725-109") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 2:1:1") %>%
  add_row(notes = "A -> F = 2:1:1") %>%
  add_row(notes = "P/R -> A = 2:1:1") %>%
  add_row(notes = "N -> A = 2:1:1") %>%
  add_row(notes = "A -> S = 3:1:1") %>%
  add_row(notes = "N -> S = 3:1:1") %>%
  add_row(notes = "S -> S = 3:1:1") %>%
  add_row(notes = "Prob = c(2^(1:10)/100))") %>%
  add_row(sim = "20210725-110") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 3:1:1") %>%
  add_row(notes = "A -> F = 3:1:1") %>%
  add_row(notes = "P/R -> A = 3:1:1") %>%
  add_row(notes = "N -> A = 3:1:1") %>%
  add_row(notes = "A -> S = 3:1:1") %>%
  add_row(notes = "N -> S = 3:1:1") %>%
  add_row(notes = "S -> S = 3:1:1") %>%
  add_row(notes = "Prob = c(2^(1:10)/100))") %>%
  add_row(sim = "20210725-111") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 4:1:1") %>%
  add_row(notes = "A -> F = 4:1:1") %>%
  add_row(notes = "P/R -> A = 4:1:1") %>%
  add_row(notes = "N -> A = 4:1:1") %>%
  add_row(notes = "A -> S = 3:1:1") %>%
  add_row(notes = "N -> S = 3:1:1") %>%
  add_row(notes = "S -> S = 3:1:1") %>%
  add_row(notes = "Prob = c(2^(1:10)/100))") %>%
  add_row(sim = "20210725-112") %>%  # NEW SAMPLE METHOD
  add_row(notes = "A -> C = 4:2:1") %>%
  add_row(notes = "A -> F = 4:2:1") %>%
  add_row(notes = "P/R -> A = 4:2:1") %>%
  add_row(notes = "N -> A = 4:2:1") %>%
  add_row(notes = "A -> S = 3:1:1") %>%
  add_row(notes = "N -> S = 3:1:1") %>%
  add_row(notes = "S -> S = 3:1:1") %>%
  add_row(notes = "Prob = c(2^(1:10)/100))") %>%
  tidyr::fill(sim, .direction = "down") %>%
  filter(!is.na(notes))

# Need for Slidy file creation (must have full path to source() inside .Rmd)
saveRDS(sim_log, file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Assets/simulation_log.rds"))


################################ OLD CODE ######################################

# 20210615-01 = Reduce nesting by 20%, Dest. cell n=500
# 20210615-02 = Reduce nesting by 20%, Increase cruise(btwn .25-.85) by 50%
#               Increase flight(btwn .35-.75) by 50%, Dest. cell n=1000
# 20210616-02 = Increase cruise(btwn .25-.85) by 50%
#               Increase flight(btwn .35-.75) by 50%, Dest. cell n=500
#               Removed nest_dist in hmm
# 20210616-03 = Increase cruise(btwn .25-.85) by 50%
#               Increase flight(btwn .35-.75) by 50%, Dest. cell n=100
#               Removed nest_dist in hmm
#               No con_nest_kernel_log in redist_kernel
# 20210616-04 = Increase cruise(btwn .25-.85) by 50%
#               Increase flight(btwn .35-.75) by 50%, Dest. cell n=500
#               Removed nest_dist in hmm
#               No con_nest_kernel_log in redist_kernel
# 20210616-05 = Increase cruise(btwn .25-.85) by 50%
#               Increase flight(btwn .35-.75) by 50%, Dest. cell n=1000
#               Removed nest_dist in hmm
#               No con_nest_kernel_log in redist_kernel
# 20210616-06 = Increase cruise(btwn .25-.85) by 50%
#               Increase flight(btwn .35-.75) by 50%, Dest. cell n=50
#               Removed nest_dist in hmm
#               No con_nest_kernel_log in redist_kernel
# 20210616-07 = Reduce perch (btwn .25-.85) by 25%, Dest. cell n=100
#               Removed nest_dist in hmm
#               No con_nest_kernel_log in redist_kernel
# 20210616-08 = Reduce perch (btwn .25-.85) by 25%, Dest. cell n=100
#               Removed nest_dist in hmm
#               No con_nest_kernel_log in redist_kernel
#               Included ssf_kernel_log twice in kernel_stack
# 20210616-09 = Reduce perch (btwn .25-.85) by 25%, Dest. cell n=100
#               Removed nest_dist in hmm
#               No con_nest_kernel_log in redist_kernel
#               Included move_kernel_log twice in kernel_stack
# 20210616-10 = Reduce perch (btwn .25-.85) by 25%, Dest. cell n=100
#               Removed nest_dist in hmm
#               No con_nest_kernel_log OR move_kernel_log in redist_kernel
# 20210616-11 = Reduce perch (btwn .25-.85) by 25%, Dest. cell n=100
#               Increase cruise(btwn .25-.85) by 25%
#               Increase flight(btwn .25-.75) by 25%
#               Removed nest_dist in hmm
#               No con_nest_kernel_log OR move_kernel_log in redist_kernel
# 20210616-12 = Reduce perch (btwn .25-.85) by 25%, Dest. cell n=500
#               Increase cruise(btwn .25-.85) by 25%
#               Increase flight(btwn .25-.75) by 25%
#               Removed nest_dist in hmm
#               No con_nest_kernel_log OR move_kernel_log in redist_kernel
# 20210616-13 = Reduce perch (btwn .25-.85) by 25%, Dest. cell n=1000
#               Increase cruise(btwn .25-.85) by 25%
#               Increase flight(btwn .25-.75) by 25%
#               Removed nest_dist in hmm
#               No con_nest_kernel_log OR move_kernel_log in redist_kernel
# 20210616-14 = Reduce perch (btwn .25-.85) by 25%, Dest. cell n=1000
#               Increase cruise(btwn .25-.85) by 25%
#               Increase flight(btwn .25-.75) by 25%
#               Removed nest_dist in hmm
#               move_kernel weight .25 (others are 1)
# 20210616-15 = Reduce perch (btwn .25-.85) by 25%, Dest. cell n=100
#               Increase cruise(btwn .25-.85) by 25%
#               Increase flight(btwn .25-.75) by 25%
#               Removed nest_dist in hmm
#               move_kernel weight 1; ssf_kernel weight 4
# 20210616-16 = Reduce perch (btwn .25-.85) by 25%, Dest. cell n=100
#               Increase cruise(btwn .25-.85) by 25%
#               Increase flight(btwn .25-.75) by 25%
#               Removed nest_dist in hmm
#               move_kernel, con_nest_kernel weight 1; ssf_kernel weight 4
# 20210616-17 = Reduce perch (btwn .25-.85) by 25%, Dest. cell n=100
#               Increase cruise(btwn .25-.85) by 25%
#               Increase flight(btwn .25-.75) by 25%
#               Removed nest_dist in hmm
#               move_kernel, con_nest_kernel weight 1; ssf_kernel weight 10
# 20210616-18 = Reduce perch (btwn .25-.85) by 25%, Dest. cell n=100
#               Increase cruise(btwn .25-.85) by 25%
#               Increase flight(btwn .25-.75) by 25%
#               Removed nest_dist in hmm
#               move_kernel, con_nest_kernel weight 1; ssf_kernel weight 7
# 20210616-19 = Reduce perch (btwn .25-.85) by 25%,
#               Increase cruise(btwn .25-.85) by 25%
#               Increase flight(btwn .25-.75) by 25%
#               Only daily_proportion in hmm
#               A -> A = 2:1:1 n=50
#               S -> A = 2:1:1 n=50
#               A -> S = 5:1:1 n=500
#               S -> S = 2:1:1 n=500
# 20210616-20 = Increase cruise -> cruise probability (btwn .1-.9) by 50%
#               Increase flight -> flight probability (btwn .1-.9) by 50%
#               Only daily_proportion in hmm
#               A -> A = 1:2:1 n=50
#               S -> A = 1:2:1 n=50
#               A -> S = 5:1:1 n=500
#               S -> S = 3:3:1 n=500
# 20210616-21 = Increase cruise -> cruise probability (btwn .1-.9) by 25%
#               Increase flight -> flight probability (btwn .1-.9) by 25%
#               Only daily_proportion in hmm
#               A -> A = 1:2:1 n=50
#               S -> A = 1:2:1 n=50
#               A -> S = 5:1:1 n=500
#               S -> S = 3:3:1 n=500
# 20210616-22 = Increase cruise -> cruise probability (btwn .1-.9) by 25%
#               Increase flight -> flight probability (btwn .1-.9) by 25%
#               Only daily_proportion in hmm
#               A -> A = 1:2:1 n=50
#               S -> A = 1:2:1 n=50
#               A -> S = 3:2:1 n=500
#               S -> S = 3:2:1 n=500
# 20210616-23 = Reduce perch (btwn .2-.8) by 15%,
#               Increase cruise -> cruise probability (btwn .1-.9) by 50%
#               Increase flight -> flight probability (btwn .1-.9) by 50%
#               Only daily_proportion in hmm
#               A -> A = 3:1:2 n=50
#               S -> A = 3:1:2 n=50
#               A -> S = 3:2:1 n=500
#               S -> S = 3:2:1 n=500
# 20210725-01 = HMM = julian and daily_proportion
#               A -> A = 3:1:2 n=50
#               S -> A = 3:1:2 n=50
#               A -> S = 3:2:1 n=500
#               S -> S = 3:2:1 n=500
# 20210725-02 = HMM = julian and daily_proportion
#               A -> A = 3:1:2 n=100
#               S -> A = 3:1:2 n=100
#               A -> S = 3:2:1 n=1000
#               S -> S = 3:2:1 n=1000
# 20210725-03 = HMM = julian and daily_proportion
#               A -> A = 3:1:2 n=25
#               S -> A = 3:1:2 n=25
#               A -> S = 3:2:1 n=250
#               S -> S = 3:2:1 n=250
# 20210725-04 = HMM = julian and daily_proportion
#               A -> A = 2:2:1 n=25
#               S -> A = 2:2:1 n=25
#               A -> S = 4:2:1 n=250
#               S -> S = 4:2:1 n=250
# 20210725-05 = HMM = julian and daily_proportion
#               A -> A = 4:2:1 n=25
#               S -> A = 4:2:1 n=25
#               A -> S = 4:2:1 n=250
#               S -> S = 4:2:1 n=250
# 20210725-06 = HMM = julian and daily_proportion
#               A -> A = 6:2:1 n=25
#               S -> A = 6:2:1 n=25
#               A -> S = 6:2:1 n=250
#               S -> S = 6:2:1 n=250
# 20210725-07 = HMM = julian and daily_proportion
#               A -> A = 3:1:1 n=25
#               S -> A = 3:1:1 n=25
#               A -> S = 4:1:1 n=250
#               S -> S = 4:1:1 n=250
# 20210725-08 = HMM = julian and daily_proportion
#               A -> A = 2:1:1 n=25
#               S -> A = 2:1:1 n=25
#               A -> S = 5:1:1 n=250
#               S -> S = 5:1:1 n=250
# 20210725-09 = HMM = julian and daily_proportion
#               A -> A = 2:1:1 n=50
#               S -> A = 2:1:1 n=50
#               A -> S = 5:1:1 n=500
#               S -> S = 5:1:1 n=500
# 20210725-10 = HMM = julian and daily_proportion
#               A -> A = 2:1:1 n=100
#               S -> A = 2:1:1 n=100
#               A -> S = 5:1:1 n=1000
#               S -> S = 5:1:1 n=1000
# 20210725-11 = HMM = julian and daily_proportion
#               A -> A = 2:1:1 n=200
#               S -> A = 2:1:1 n=200
#               A -> S = 5:1:1 n=2000
#               S -> S = 5:1:1 n=2000
# 20210725-12 = HMM = julian and daily_proportion
#               A -> A = 2:1:1 n=300
#               S -> A = 2:1:1 n=300
#               A -> S = 5:1:1 n=3000
#               S -> S = 5:1:1 n=3000
# 20210725-13 = HMM = julian and daily_proportion
#               A -> A = 3:1:2 n=25
#               S -> A = 3:1:2 n=25
#               A -> S = 5:1:1 n=1000
#               S -> S = 5:1:1 n=1000
# 20210725-14 = HMM = julian and daily_proportion
#               A -> A = 4:1:2 n=25
#               S -> A = 4:1:2 n=25
#               A -> S = 5:1:1 n=1000
#               S -> S = 5:1:1 n=1000
# 20210725-15 = HMM = julian and daily_proportion
#               A -> A = 4:1:2 n=25
#               S -> A = 4:1:2 n=25
#               A -> S = 5:1:1 n=500
#               S -> S = 5:1:1 n=500
# 20210725-16 = HMM = julian and daily_proportion
#               A -> A = 4:1:2 n=50
#               S -> A = 4:1:2 n=50
#               A -> S = 5:1:1 n=1000
#               S -> S = 5:1:1 n=1000
# 20210725-17 = HMM = julian and daily_proportion
#               A -> A = 4:1:2 n=100
#               S -> A = 4:1:2 n=100
#               A -> S = 5:1:1 n=1000
#               S -> S = 5:1:1 n=1000
# 20210725-18 = HMM = julian and daily_proportion
#               A -> A = 3:1:2 n=50
#               S -> A = 3:1:2 n=50
#               A -> S = 5:1:1 n=1000
#               S -> S = 5:1:1 n=1000
# 20210725-19 = HMM = julian and daily_proportion
#               A -> A = 3:1:2 n=50
#               S -> A = 3:1:2 n=50
#               A -> S = 4:1:1 n=1000
#               S -> S = 4:1:1 n=1000
# 20210725-20 = HMM = julian and daily_proportion
#               A -> A = 3:1:2 n=50
#               S -> A = 3:1:2 n=50
#               A -> S = 3:1:1 n=1000
#               S -> S = 3:1:1 n=1000
# 20210725-21 = HMM = julian and daily_proportion
#               A -> A = 3:1:2 n=50
#               S -> A = 3:1:2 n=50
#               A -> S = 3:2:1 n=1000
#               S -> S = 3:2:1 n=1000
# 20210725-22 = HMM = julian and daily_proportion
#               A -> A = 3:2:1 n=25
#               S -> A = 3:1:1 n=25
#               A -> S = 5:1:1 n=1000
#               S -> S = 5:2:3 n=1000
# 20210725-23 = HMM = julian and daily_proportion
#               A -> A = 3:3:1 n=25
#               S -> A = 3:1:1 n=25
#               A -> S = 5:1:1 n=1000
#               S -> S = 5:1:3 n=1000
# 20210725-24 = HMM = julian and daily_proportion
#               A -> A = 3:4:1 n=25
#               S -> A = 3:1:1 n=25
#               A -> S = 5:1:1 n=1000
#               S -> S = 5:1:3 n=1000
# 20210725-25 = HMM = julian and daily_proportion
#               A -> A = 2:3:1 n=25
#               S -> A = 3:1:1 n=25
#               A -> S = 4:1:2 n=1000
#               S -> S = 5:1:3 n=1000
# 20210725-26 = HMM = julian and daily_proportion
#               A -> A = 3:3:1 n=25
#               S -> A = 3:1:1 n=25
#               A -> S = 4:1:2 n=1000
#               S -> S = 5:2:1 n=1000
# 20210725-27 = HMM = julian and daily_proportion
#               A -> A = 3:1:1 n=10
#               S -> A = 3:1:1 n=10
#               A -> S = 4:1:2 n=1000
#               S -> S = 5:2:1 n=1000
# 20210725-28 = HMM = julian and daily_proportion
#               A -> A = 1:1:1 n=25
#               S -> A = 3:2:1 n=25
#               A -> S = 4:1:2 n=1000
#               S -> S = 5:1:1 n=1000
# 20210725-29 = HMM = julian and daily_proportion
#               A -> A = 1:1:1 n=10
#               S -> A = 3:2:1 n=10
#               A -> S = 4:1:2 n=1000
#               S -> S = 5:1:1 n=1000
# 20210725-30 = HMM = julian and daily_proportion
#               A -> A = 1:2:1 n=10
#               S -> A = 3:2:1 n=10
#               A -> S = 4:1:2 n=1000
#               S -> S = 5:1:1 n=1000
# 20210725-31 = HMM = julian and daily_proportion
#               A -> A = 1:3:1 n=10
#               S -> A = 3:2:1 n=10
#               A -> S = 4:1:2 n=1000
#               S -> S = 5:1:1 n=1000
# 20210725-32 = HMM = julian and daily_proportion
#               A -> A = 1:3:1 n=25
#               S -> A = 3:2:1 n=25
#               A -> S = 4:1:2 n=1000
#               S -> S = 5:1:1 n=1000
# 20210725-33 = HMM = julian and daily_proportion
#               A -> A = 1:3:1 n=1000-1
#               S -> A = 3:2:1 n=1000-1
#               A -> S = 4:1:2 n=1000
#               S -> S = 5:1:1 n=1000
# 20210725-34 = HMM = julian and daily_proportion
#               A -> A = 5:2:1 n=1000-1
#               S -> A = 5:2:1 n=1000-1
#               A -> S = 4:1:2 n=1000
#               S -> S = 5:1:1 n=1000
# 20210725-35 = HMM = julian and daily_proportion
#               A -> A = 2:2:1 n=1000-1
#               S -> A = 2:2:1 n=1000-1
#               A -> S = 4:1:2 n=1000
#               S -> S = 5:2:1 n=1000
# 20210725-36 = HMM = julian and daily_proportion
#               A -> A = 5:2:1 n=1000-100-1
#               S -> A = 5:2:1 n=1000-100-1
#               A -> S = 4:1:2 n=1000
#               S -> S = 5:2:1 n=1000
# 20210725-37 = HMM = julian and daily_proportion
#               A -> A = 5:2:1 n=1000-50-1
#               S -> A = 5:2:1 n=1000-50-1
#               A -> S = 4:1:2 n=1000
#               S -> S = 5:2:1 n=1000
# 20210725-38 = HMM = julian and daily_proportion
#               A -> A = 5:2:1 n=1000-25-1
#               S -> A = 5:2:1 n=1000-25-1
#               A -> S = 4:1:2 n=1000
#               S -> S = 5:2:1 n=1000
# 20210725-39 = HMM = julian and daily_proportion
#               A -> A = 1:5:3 n=25
#               S -> A = 3:2:3 n=25
#               A -> S = 4:1:2 n=1000
#               S -> S = 5:1:1 n=1000
# 20210725-40 = HMM = julian and daily_proportion
#               A -> A = 3:1:2 n=25
#               S -> A = 3:1:2 n=25
#               A -> S = 5:1:2 n=1000
#               S -> S = 5:1:1 n=1000
# 20210725-41 = HMM = julian and daily_proportion
#               A -> C = 1:3:2 n=25
#               A -> F = 4:2:1 n=25
#               S -> A = 4:2:1 n=25
#               A -> S = 5:1:3 n=1000
#               N -> S = 5:2:3 n=1000
#               S -> S = 5:2:1 n=1000
# 20210725-42 = HMM = julian and daily_proportion




