library(pacman)
p_load(DT, dplyr, ggplot2, readr, stringr, xtable)
options(stringsAsFactors = FALSE)

############################################################################# ##
#### -------------------------- CHAPTER 2 --------------------------------- ####
############################################################################# ##

bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}

# HOMERANGE METRICS ------------------------------------------------------------
homerange_metrics_org <- readRDS(file.path("Output/Analysis/Homerange",
  "homerange_metrics.rds")) #%>% filter(!id %in% c("Eskutassis", "Sheepscot"))

unique(homerange_metrics_org$id)

homerange_metrics <- homerange_metrics_org %>%
  as_tibble(.) %>%
  mutate(yr = as.integer(year)) %>%
  dplyr::select(id, yr, locs, ud_95_total, ud_50_total) %>%
  rename('Eagle ID' = id) %>%
  rename(Year = yr) %>%
  rename('GPS Points\\newline(\\#)' = locs) %>%
  rename('95\\% UD Area\\newline(km\\textsuperscript{2})' = ud_95_total) %>%
  rename('50\\% UD Area\\newline(km\\textsuperscript{2})' = ud_50_total) %>%
  map_if(is.factor, as.character) %>%
  as_tibble(.)

bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}

# Check for proper handling of symbols
print(xtable(homerange_metrics), sanitize.text.function = identity)

# Covert to an xtable and adjust alignment
homerange_metrics_xtable <- xtable(homerange_metrics)
ncol(homerange_metrics_xtable)
align(homerange_metrics_xtable)  <- c("L{0}", "L{.5}", "C{.25}", "C{.5}",
  "C{.75}", "C{.75}")
# translates to relative column widths (first value, rownames, is ignored)
# should sum to 3 for the 3 columns

# For LaTeX Folder
print(homerange_metrics_xtable,
  floating = FALSE, width = "\\textwidth",
  tabular.environment = "tabularx",
  booktabs = TRUE, # thick top/bottom line, Premable add "\usepackage{booktabs}"
  include.rownames=FALSE,
  size="\\fontsize{11pt}{12pt}\\selectfont",
  sanitize.colnames.function=bold,
  file = file.path("C:/Users/blake/OneDrive/Work/LaTeX/BMassey_Dissertation",
                   "Tables/Homerange_Metrics.tex"))



# SSF ANALYSIS LANDSCAPE COVARIATES --------------------------------------------
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
  sanitize.colnames.function=bold,
  file = "Products/Tables/SSF_Landscape_Covariates.tex")

# For LaTeX Folder
print(ssf_land_covar_xtable,
  floating = FALSE, width = "\\textwidth",
  tabular.environment = "tabularx",
  booktabs = TRUE, # thick top/bottom line, Premable add "\usepackage{booktabs}"
  include.rownames=FALSE,
  size="\\fontsize{11pt}{12pt}\\selectfont",
  sanitize.colnames.function=bold,
  file = file.path("C:/Users/blake/OneDrive/Work/LaTeX/BMassey_Dissertation",
                   "Tables/SSF_Landscape_Covariates.tex"))


# HOMERANGE METRICS ------------------------------------------------------------

homerange_metrics <- readRDS("Output/Analysis/Homerange/homerange_metrics.rds")
colnames(homerange_metrics)

ggplot(homerange_metrics) +
  geom_boxplot(aes(y = ud_50_total))






