library(pacman)
p_load(DT, dplyr, readr, stringr, xtable)
options(stringsAsFactors = FALSE)

############################## CHAPTER 1 #######################################

# SSF Analysis Parameter Sets --------------------------------------------------
optim_parm_sets_org <- read_csv("Data/Assets/opt_parm_set_examples.csv")

optim_parm_sets <- optim_parm_sets_org %>%
  rename(values_range = "Values Range") %>%
  mutate("Values Range" = str_replace_all(values_range, '"', "")) %>%
  select(-c(values_range))

# modify for use as table in dissertation
#DT::datatable(head(optim_parm_sets), editable = TRUE)

bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}

optim_parm_sets_xtable <- xtable(optim_parm_sets,
  only.contents = TRUE, floating = FALSE,
  caption = "My caption\\label{tab:Optim_Parm_Sets_Example}",
  label = "tab:Optim_Parm_Sets_Example")

align(optim_parm_sets_xtable)  <- c("L{0}", "L{.5}", "L{1.75}", "L{.75}")
# translates to relative column widths (first value, rownames, is ignored)
# should sum to 3 for the 3 columns
# "L" indicates left-aligned, ragged-right, no hypenation (check LaTeX preamble)
# "H" indicates left-aligned, ragged-right, hypenation (check LaTeX preamble)


print(optim_parm_sets_xtable,
  floating = FALSE, width = "\\textwidth",
  tabular.environment = "tabularx",
  booktabs = TRUE, # thick top/bottom line, Premable add "\usepackage{booktabs}"
  include.rownames = FALSE,
  sanitize.colnames.function = bold,
  size = "\\fontsize{11pt}{12pt}\\selectfont",
  file = "Products/Tables/Optim_Parm_Sets.tex")


# SSF Analysis Landscape Covariates --------------------------------------------
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

# SSF Analysis Landscape Covariates --------------------------------------------
ssf_land_covar_org <- read_csv("Data/Assets/ssf_landscape_covariates.csv")

ssf_land_covar <- ssf_land_covar_org %>%
  filter(!is.na(Type)) %>%
  dplyr::select(Class, Covariate, Description, Source)
