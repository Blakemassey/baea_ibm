
::::

## Three Column Layout
Below is a Div containing three child Divs side by side. The Div
in the middle is empty, just to add more space between the left
and right Divs.

:::: {style="display: flex;"}
::: {.column width=30%}

Here is the **first** Div.

```{r}
str(iris)
```
:::

::: {.column width=60%}
And this block will be put on the right:

```{r}
plot(iris[, -5])
```

:::

::: {.column width=10%}
Far right
:::

::::


  ```{r out.width = "50%"}
n <- "40"
include_graphics(paste0(sim_dir, n, "/Calibration/gg_combine_ridge.png"))
include_graphics(paste0(sim_dir, n, "/Calibration/gg_combine_hydro_dist.png"))
sim_log_i <- sim_log %>%
  filter(sim == "20210725-01") %>%
  select(notes)
kable(sim_log_i)
include_graphics(paste0(sim_dir, n, "/AKDEs/", sim_id, "-", n, "_01-01.png"))
include_graphics(paste0(sim_dir, n, "/AKDEs/", sim_id, "-", n, "_01-02.png"))
include_graphics(paste0(sim_dir, n, "/AKDEs/", sim_id, "-", n, "_01-03.png"))
include_graphics(paste0(sim_dir, n, "/AKDEs/", sim_id, "-", n, "_01-04.png"))
include_graphics(paste0(sim_dir, n, "/AKDEs/", sim_id, "-", n, "_01-05.png"))
include_graphics(paste0(sim_dir, n, "/AKDEs/", sim_id, "-", n, "_01-06.png"))
include_graphics(paste0(sim_dir, n, "/AKDEs/", sim_id, "-", n, "_01-07.png"))
include_graphics(paste0(sim_dir, n, "/AKDEs/", sim_id, "-", n, "_01-08.png"))
```
