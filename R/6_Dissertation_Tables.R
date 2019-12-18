library(pacman)
p_load(DT, lubridate, tidyverse, ggplot2, readr, xtable)
suppressMessages(extrafont::loadfonts(device="win"))
options(stringsAsFactors = FALSE)

pkg_dir <- "C:/Users/Blake/OneDrive/Work/R/Projects/multiscale_optim"
tex_dir <- "C:/Users/Blake/OneDrive/Work/LaTeX/BMassey_Dissertation"

BoldText <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}

# Theme (for LaTeX font)
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 14))


############################################################################# ##
#### -------------------------- CHAPTER 2 --------------------------------- ####
############################################################################# ##

## GPS Deployment and Territoriality -------------------------------------

baea_hr_org <- readRDS("Data/BAEA/baea_homerange.rds")
baea_hr <- table(baea_hr_org$id, baea_hr_org$year) %>%
  as.data.frame.matrix(.)%>%
  rownames_to_column(.) %>%
  as_tibble(.) %>%
  rename("id" = "rowname") %>%
  na_if(0) %>%
  mutate(row=row_number()) %>%
  pivot_longer(., cols = `2015`:`2019`, names_to = "years",
    values_drop_na = TRUE) %>%
  group_by(id) %>%
  summarize(terr_years = paste(years, collapse = ", ")) %>%
  mutate(terr_years = str_replace_all(terr_years,"2015, 2016, 2017, 2018, 2019",
    "2015 -- 2019")) %>%
  mutate(terr_years = str_replace_all(terr_years, "2015, 2016, 2017, 2018",
    "2015 -- 2018")) %>%
  mutate(terr_years = str_replace_all(terr_years, "2015, 2016, 2017",
    "2015 -- 2017"))


gps_deploys_org <- read_csv("Data/GPS/GPS_Deployments.csv")
gps_deploys <- gps_deploys_org %>%
  filter(!is.na(deploy_seq) & !is.na(trap_site)) %>%
  arrange(deploy_seq) %>%
  select(serial, deployed, sex, deploy_location, trap_site, county) %>%
  mutate(id = deploy_location) %>%
  mutate(deployed = lubridate::ymd(deployed))

gps_deploys

deployments <- gps_deploys %>% left_join(., baea_hr, by = "id") %>%
  transmute(`Bird ID` = id,
            `Trap Site` = trap_site,
            `County` = county,
            `Trap Date` = format(deployed, format="%Y-%m-%d"),
            `Sex` = str_to_title(sex),
            `Territorial Years`= terr_years)

# Check for proper handling of symbols
print(xtable(deployments), sanitize.text.function = identity)

# Covert to an xtable and adjust alignment
deployments_xtable <- xtable(deployments)
ncol(deployments_xtable)
align(deployments_xtable)  <- c("L{0}", "L{.42}", "C{.835}", "C{.415}",
                                "C{.45}", "C{.38}", "C{.5}")
str_replace_all(align(deployments_xtable), "[^[//.||0-9]]", "") %>%
  as.numeric(.) %>% sum()
# translates to relative column widths (first value, rownames, is ignored)
# should sum to 3 for the 3 columns

# For LaTeX Folder
print(deployments_xtable,
  floating = FALSE, width = "\\textwidth",
  tabular.environment = "tabularx",
  booktabs = TRUE, # thick top/bottom line, Premable add "\usepackage{booktabs}"
  include.rownames=FALSE,
  size="\\fontsize{11pt}{12pt}\\selectfont",
  sanitize.colnames.function=BoldText,
  file = file.path("C:/Users/blake/OneDrive/Work/LaTeX/BMassey_Dissertation",
                   "Tables/Ch2/Deployments.tex"))

## Homerange Metrics -----------------------------------------------------------

hr_metrics_org <- readRDS(file.path("Output/Analysis/Homerange",
  "hr_all_metrics.rds")) #%>% filter(!id %in% c("Eskutassis", "Sheepscot"))
unique(hr_metrics_org$id)
colnames(hr_metrics_org)

hr_metrics <- hr_metrics_org %>%
  as_tibble(.) %>%
  mutate(yr = as.integer(year)) %>%
  transmute('Eagle ID' = id,
    Year = yr,
    'GPS Points\\newline(n)' = locs,
    '95\\% UD Area\\newline(km\\textsuperscript{2})' = ud_95_total,
    '50\\% UD Area\\newline(km\\textsuperscript{2})' = ud_50_total,
    '50\\% UD Waterbody Area\\newline(km\\textsuperscript{2})' =
      ud_50_waterbody_area,
    '95\\% UD Waterbody Area\\newline(km\\textsuperscript{2})' =
      ud_95_waterbody_area) %>%
  map_if(is.factor, as.character) %>%
  as_tibble(.)

# Check for proper handling of symbols
print(xtable(hr_metrics), sanitize.text.function = identity)

# Covert to an xtable and adjust alignment
hr_metrics_xtable <- xtable(hr_metrics)
ncol(hr_metrics_xtable)
align(hr_metrics_xtable)  <- c("L{0}", "L{.35}", "C{.35}", "C{.3}",
  "C{.5}", "C{.5}", "C{.5}", "C{.5}")

# translates to relative column widths (first value, rownames, is ignored)
# should sum to 3 for the 3 columns
str_replace_all(align(hr_metrics_xtable), "[^[//.||0-9]]", " ") %>%
  as.numeric(.) %>% sum()

# For LaTeX Folder
print(hr_metrics_xtable,
  floating = FALSE, width = "\\textwidth",
  tabular.environment = "tabularx",
  booktabs = TRUE, # thick top/bottom line, Premable add "\usepackage{booktabs}"
  include.rownames=FALSE,
  size="\\fontsize{11pt}{12pt}\\selectfont",
  sanitize.colnames.function=BoldText,
  file = file.path("C:/Users/blake/OneDrive/Work/LaTeX/BMassey_Dissertation",
                   "Tables/Ch2/Homerange_Metrics.tex"))

## SSF Analysis Covariates -----------------------------------------------------

ssf_land_covar_org <- read_csv("Data/Assets/ssf_landscape_covariates.csv")

ssf_land_covar <- ssf_land_covar_org %>%
  filter(!is.na(Type)) %>%
  dplyr::select(Class, Covariate, Description, Source)

# modify for use as table in dissertation
#  DT::datatable(head(ssf_land_covar), editable = TRUE)

ssf_land_covar_xtable <- xtable(ssf_land_covar,
  only.contents = TRUE, floating = FALSE,
  caption = "My caption\\label{tab:SSF_Landscape_Covariates}",
  label="tab:SSF_Landscape_Covariates")

align(ssf_land_covar_xtable) <- c("L{0}", "L{.7}", "L{.8}", "L{1.6}", "L{.9}")
# translates to relative column widths (first value, rownames, is ignored)
# should sum to 4 for the 4 columns
# "L" indicates left-aligned, ragged-right, no hypenation (check LaTeX preamble)
# "H" indicates left-aligned, ragged-right, hypenation (check LaTeX preamble)

print(ssf_land_covar_xtable,
  floating = FALSE, width = "\\textwidth",
  tabular.environment = "tabularx",
  booktabs = TRUE, # thick top/bottom line, Premable add "\usepackage{booktabs}"
  include.rownames=FALSE,
  size="\\fontsize{11pt}{12pt}\\selectfont",
  sanitize.colnames.function=BoldText,
  file = "Products/Tables/SSF_Landscape_Covariates.tex")

# For LaTeX Folder
print(ssf_land_covar_xtable,
  floating = FALSE, width = "\\textwidth",
  tabular.environment = "tabularx",
  booktabs = TRUE, # thick top/bottom line, Premable add "\usepackage{booktabs}"
  include.rownames=FALSE,
  size="\\fontsize{11pt}{12pt}\\selectfont",
  sanitize.colnames.function=BoldText,
  file = file.path("C:/Users/blake/OneDrive/Work/LaTeX/BMassey_Dissertation",
                   "Tables/Ch2/SSF_Landscape_Covariates.tex"))

## ConNest Distribution Fits ---------------------------------------------------

fits_baea_dist_org <- readRDS("Output/Analysis/Territorial/fits_baea_dist.rds")
fits_baea_dist_df <- SummarizeFitDist(fits_baea_dist_org) %>%
  dplyr::select(Distribution, LogLik, AIC, BIC, Parameter, Estimate, SD) %>%
  mutate(Distribution = str_replace(Distribution, "Halfnorm", "Half Normal"))

print(xtable(fits_baea_dist_df, digits = c(0, 0, 0, 0, 0, 0, 3, 4)),
  latex.environments = "", include.rownames = F)

fits_baea_dist_xtable <- xtable(fits_baea_dist_df,
  digits = c(0, 0, 0, 0, 0, 0, 3, 4), only.contents = TRUE, floating = FALSE)

#& exponential & halfnorm & gamma &  & pareto &  & weibull &

align(fits_baea_dist_xtable) <- c("L{0}",
"L{.65}", "R{.35}", "R{.35}", "R{.35}", "R{.5}", "R{.4}", "R{.4}")

str_replace_all(align(fits_baea_dist_xtable), "[^[//.||0-9]]", "") %>%
  as.numeric(.) %>% sum()
# translates to relative column widths (first value, rownames, is ignored)
# should sum to 4 for the 4 columns
# "L" indicates left-aligned, ragged-right, no hypenation (check LaTeX preamble)
# "H" indicates left-aligned, ragged-right, hypenation (check LaTeX preamble)

# For LaTeX Folder
print(fits_baea_dist_xtable,
  floating = FALSE, width = "\\textwidth",
  tabular.environment = "tabularx",
  booktabs = TRUE, # thick top/bottom line, Premable add "\usepackage{booktabs}"
  include.rownames=FALSE,
  size="\\fontsize{11pt}{12pt}\\selectfont",
  sanitize.colnames.function=BoldText,
  file = file.path("C:/Users/blake/OneDrive/Work/LaTeX/BMassey_Dissertation",
                   "Tables/Ch2/BAEA_Dist_Fits.tex"))

## Movement Parameter Fits -----------------------------------------------------

fits_move_pars_org <- readRDS("Output/Analysis/Movements/move_pars.rds")
fits_move_pars_df  <- fits_move_pars_org %>%
  dplyr::select(behavior_behavior, min_step:mvm_prop) %>%
  mutate(behavior_behavior = str_replace_all(behavior_behavior, "->",
    "$\\\\rightarrow$"))

print(xtable(fits_move_pars_df, digits = c(0, 0, 0, 0, 2, 0, 2, 2, 2, 2, 2)),
  sanitize.text.function=identity, latex.environments = "", include.rownames =F)

fits_move_pars_xtable <- xtable(fits_move_pars_df,
  digits = c(0, 0, 0, 0, 2, 0, 2, 2, 2, 2, 2), only.contents = TRUE,
  floating = FALSE)

# test the column width sum (should be 10, the number of columns)
test <- "L{2.55}C{1.3}C{1.35}R{.5}R{.9}R{.555}R{.555}R{.92}R{.92}R{.45}"
str_replace_all(str_split(test, "\\{", simplify = TRUE), "[^[//.||0-9]]", "")%>%
  str_subset(., "[0-9]") %>% as.numeric(.) %>% sum()

align(fits_move_pars_xtable) <- c("L{0}",
  "L{2.55}", "C{1.3}", "C{1.35}", "R{.5}", "R{.9}", "R{.555}", "R{.555}",
  "R{.92}", "R{.92}", "R{.45}")

str_replace_all(align(fits_move_pars_xtable), "[^[//.||0-9]]", "") %>%
  str_subset(., "[0-9]") %>% as.numeric(.) %>% sum()
# translates to relative column widths (first value, rownames, is ignored)
# should sum to 10 for the 10 columns
# "L" indicates left-aligned, ragged-right, no hypenation (check LaTeX preamble)
# "H" indicates left-aligned, ragged-right, hypenation (check LaTeX preamble)

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- c(paste0(
  "\\multirow{2}{=}{\\centering \\textbf{\\hfil Movement\\newline Step Type}} ",
  "& \\multicolumn{2}{c}{\\textbf{Step Length}} ",
  "& \\multicolumn{2}{c}{\\textbf{Weibull}} ",
  "& \\multicolumn{5}{c}{\\textbf{Mixed Von Mises}} \\\\ ",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-10} ",
  "& {Min (m)} & {Max (m)} & {$\\kappa$} ",
  "& \\multicolumn{1}{c}{$\\lambda$} ",
  "& \\multicolumn{1}{c}{$\\mu\\textsubscript{1}$} ",
  "& \\multicolumn{1}{c}{$\\mu\\textsubscript{2}$} ",
  "& \\multicolumn{1}{c}{$\\kappa\\textsubscript{1}$} ",
  "& \\multicolumn{1}{c}{$\\kappa\\textsubscript{2}$} & {Mix} \\\\"))

# For LaTeX Folder
print(fits_move_pars_xtable,
  add.to.row = addtorow,
  floating = FALSE, width = "\\textwidth",
  tabular.environment = "tabularx",
  booktabs = TRUE, # thick top/bottom line, Premable add "\usepackage{booktabs}"
  include.rownames = FALSE,
  include.colnames = FALSE,
  size="\\fontsize{11pt}{12pt}\\selectfont",
  sanitize.colnames.function=BoldText,
  sanitize.text.function=identity,
  file = file.path("C:/Users/blake/OneDrive/Work/LaTeX/BMassey_Dissertation",
                   "Tables/Ch2/Movement_Pars.tex"))


# ---------------------------------------------------------------------------- #
################################ OLD CODE ######################################
# ---------------------------------------------------------------------------- #
