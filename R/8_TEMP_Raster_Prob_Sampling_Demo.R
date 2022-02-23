pacman::p_load(tidyverse, ggthemes)
sample_n <- 10000
sample_vec <- sample(10, size = sample_n, replace = T, 1:10/10)
sample_vec <- sample(10, size = sample_n, replace = T, c(1:10)^2/100)
sample_vec <- sample(10, size = sample_n, replace = T, c(2^(1:10)/100))

prob_sample = as_tibble_col(sample_vec,
  column_name = "x") %>%
  mutate(x = as.factor(x))
ggplot(prob_sample) +
  geom_bar(aes(x), fill = "#69b3a2", color = "#e9ecef") +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 10), name = "Count",
      labels = scales::comma) +
    scale_x_discrete(name = "Reclassed", breaks = 1:100, labels = 1:100) +
    theme_clean()

prob_sample_freq_table <- prob_sample %>%
  count(x) %>%
  mutate(prop = n/sum(n))
prob_sample_freq_table




