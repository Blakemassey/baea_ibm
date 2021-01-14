library(pacman)
p_load(DT, lubridate, tibble, tidyverse, ggplot2, readr, xtable)
suppressMessages(extrafont::loadfonts(device="win"))
options(stringsAsFactors = FALSE)

pkg_dir <- "C:/Users/Blake/OneDrive/Work/R/Projects/multiscale_optim"
tex_dir <- "C:/Users/Blake/OneDrive/Work/LaTeX/BMassey_Dissertation"

BoldText <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
BoldTextCentered <- function(x) {paste('{c}{\\textbf{',x,'}}', sep ='')}

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
  dplyr::select(behavior, behavior_behavior, min_step:mvm_prop) %>%
  mutate(behavior_behavior = str_replace_all(behavior_behavior, "->",
    "$\\\\rightarrow$")) %>%
  mutate(mvm_mu1 = ifelse(behavior %in% c("Nest", "Perch", "Roost"), NA,
    mvm_mu1)) %>%
  mutate(mvm_mu2 = ifelse(behavior %in% c("Nest", "Perch", "Roost"), NA,
    mvm_mu2)) %>%
  mutate(mvm_kappa1 = ifelse(behavior %in% c("Nest", "Perch", "Roost"), NA,
    mvm_kappa1)) %>%
  mutate(mvm_kappa2 = ifelse(behavior %in% c("Nest", "Perch", "Roost"), NA,
    mvm_kappa2)) %>%
  mutate(mvm_prop = ifelse(behavior %in% c("Nest", "Perch", "Roost"), NA,
    mvm_prop)) %>%
  dplyr::select(-behavior)


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


## SSF Models Compiled Refits --------------------------------------------------

model_fits_compiled_refit_org <- readRDS(file.path("Output/Analysis/SSF/Models",
  "model_fits_compiled_refit", "model_fits_compiled_refit.rds"))

model_fits_compiled_refit_df  <- model_fits_compiled_refit_org %>%
  dplyr::select(step_type, fit_aicc, delta_aicc, preds) %>%
  mutate(step_type_cap = str_replace_all(step_type, "_", " ")) %>%
  mutate(step_type_cap = str_to_title(step_type_cap)) %>%
  mutate(step_type_cap = str_replace_all(step_type_cap, " ", "_")) %>%
  mutate(preds = str_replace_all(preds, "_", " ")) %>%
  mutate(preds = str_replace_all(str_to_title(preds),
    "(?<=[:alpha:]) (?=[:alpha:])", "")) %>%
  mutate(preds = str_replace_all(preds, "DevelopedDist0", "DevelopedDist")) %>%
  mutate(preds = str_replace_all(preds, "HydroDist0", "HydroDist")) %>%
  mutate(preds = str_replace_all(preds, "TurbineDist0", "TurbineDist")) %>%
  mutate(preds = str_replace_all(preds, "RoadDist0", "RoadDist")) %>%
  group_by(step_type_cap) %>%
  mutate(mod_rank = 1:n()) %>%
  ungroup(.) %>%
  select(step_type_cap, mod_rank, fit_aicc, delta_aicc, preds)

colnames(model_fits_compiled_refit_df) <-
  colnames(model_fits_compiled_refit_df) %>%
  str_replace(., "mod_rank", "Rank") %>%
  str_replace(., "fit_aicc", "AICc") %>%
  str_replace(., "delta_aicc", "$\\\\Delta$ AICc") %>%
  str_replace(., "preds",
    "Model Covariates (with $\\\\sigma$ bandwidth values)")

for (i in unique(model_fits_compiled_refit_df$step_type_cap)){
  model_fits_compiled_refit_df_i <- model_fits_compiled_refit_df %>%
    filter(step_type_cap == i) %>%
    select(-c(step_type_cap))
  model_fits_compiled_xtable_i <- xtable(model_fits_compiled_refit_df_i,
    digits = c(0, 0, 2, 2, 0))
  align(model_fits_compiled_xtable_i) <- c("L{0}", "C{.3}", "C{.3}",
    "C{.45}", "L{2.95}")
  str_replace_all(align(model_fits_compiled_xtable_i), "[^[//.||0-9]]", "") %>%
    str_subset(., "[0-9]") %>% as.numeric(.) %>% sum()
  # should sum to 4 for the 4 columns
  print(model_fits_compiled_xtable_i,
    floating = FALSE, width = "\\textwidth",
    tabular.environment = "tabularx",
    booktabs = TRUE, # thick top/bottom line, Preamble: "\usepackage{booktabs}"
    include.rownames = FALSE,
    size = "\\fontsize{11pt}{12pt}\\selectfont",
    sanitize.colnames.function = BoldText,
    sanitize.text.function = identity,
    file = file.path("C:/Users/blake/OneDrive/Work/LaTeX/BMassey_Dissertation",
      "Tables/Ch2", paste0("SSF_Fits_", i,".tex")))
}

## SSF Models Compiled Best ----------------------------------------------------

model_fits_best_org <- readRDS(file.path("Output/Analysis/SSF/Models",
  "model_fits_compiled_refit_best", "model_fits_compiled_refit_best.rds"))

model_fits_best <- model_fits_best_org %>%
  mutate(start_behavior = word(step_type, 1, sep = "_")) %>%
  mutate(end_behavior = word(step_type, 2, sep = "_"))

start_step_types <- model_fits_best %>% pull(start_behavior) %>% unique(.)

i <- start_step_types[1]; j <- 1  # for testing
for (i in start_step_types){
  model_fits_best_i <- model_fits_best %>% filter(start_behavior == i)
  xtable_list <- vector(mode = "list", length = nrow(model_fits_best_i))
  for (j in seq_len(nrow(model_fits_best_i))){
    fit_ij <- model_fits_best_i %>% slice(j) %>% pull(clogit_fit) %>% pluck(1)
    start_ij <- model_fits_best_i %>% slice(j) %>% pull(start_behavior)
    end_ij <- model_fits_best_i %>% slice(j) %>% pull(end_behavior)
    xtable_fit_ij <- as.data.frame(xtable(fit_ij)) %>%
      rownames_to_column(., var = "term") %>%
      mutate(step_type = NA)
    xtable_fit_ij[1, "step_type"] <- paste(str_to_title(start_ij),
      "$\\rightarrow$", str_to_title(end_ij))
    xtable_list[[j]] <- xtable_fit_ij
  }
  model_fits_best_xtable <- xtable_list %>%
    reduce(bind_rows) %>%
    mutate(term = str_replace_all(term, "_", " ")) %>%
    mutate(term = str_replace_all(str_to_title(term),
      "(?<=[:alpha:]) (?=[:alpha:])", "")) %>%
    mutate(term = str_replace_all(term, "DevelopedDist0", "DevelopedDist"))%>%
    mutate(term = str_replace_all(term, "HydroDist0", "HydroDist")) %>%
    mutate(term = str_replace_all(term, "TurbineDist0", "TurbineDist")) %>%
    rename("Step Type" = step_type,
           "Term" = term,
           "Coefficient " = coef,
           "Exp(Coef)" = "exp(coef)",
           "SE(Coef)" = "se(coef)",
           "Z Statistic" = "z",
           "p-value" = "p") %>%
    dplyr::select("Step Type", "Term", "Coefficient ", "Exp(Coef)",
      "SE(Coef)", "Z Statistic", "p-value")

  model_fits_best_xtable <- xtable(model_fits_best_xtable,
    digits = c(0, 0, 2, 3, 2, 2, 2, 2))
  display(model_fits_best_xtable) = c("s", "s", "s", "g", "g", "g", "g", "g")

  align(model_fits_best_xtable) <- c("L{0}", "L{1.8}", "H{1.25}", "R{1}",
    "R{.9}", "R{.8}", "R{.45}", "R{.8}")
  str_replace_all(align(model_fits_best_xtable), "[^[//.||0-9]]", "") %>%
    str_subset(., "[0-9]") %>% as.numeric(.) %>% sum()

  hline <- (which(str_detect(model_fits_best_xtable$`Step Type`,
   "[:alpha:]"))-1)[-1]
  htype <- c(rep("\\midrule ", times = length(hline)))

  model_fits_best_tex <- print(model_fits_best_xtable,
    add.to.row = list(pos = as.list(hline), command = htype),
    floating = FALSE, width = "\\textwidth",
    tabular.environment = "xltabular",
    caption.placement = "top",
    booktabs = TRUE,
    include.rownames = FALSE,
    rotate.colnames = TRUE,
    size = "\\fontsize{11pt}{12pt}\\selectfont",
    sanitize.colnames.function = BoldText,
    sanitize.text.function = identity,
    print.results = FALSE)

  caption_label_tex <- paste0("\\\\caption[Step-selection Model Fits for ",
    "Step-types with ", str_to_title(i) ," Start Behavior]\n",
    "{\\\\label{tab:SSF_Fits_Terms_", str_to_title(i),
    "} Step-selection model terms and metrics for ",
    "step-types starting with ", str_to_title(i),
    " behavior for Bald Eagles in Maine.}\\\\\\\\ \n ")

  # Make column header ("Step Type") horizontal, italicize 'p' in p-value
  model_fits_best_tex_update <- model_fits_best_tex %>%
    str_replace(., "\\\\toprule", paste0(caption_label_tex, "\\\\toprule")) %>%
    str_replace_all(., paste0("\\\\begin\\{sideways\\} ",
      "\\{\\\\textbf\\{Step Type\\}\\} \\\\end\\{sideways\\}"),
      "\\{\\\\textbf\\{Step Type\\}\\}") %>%
    str_replace_all(., "\\{\\\\textbf\\{p-value\\}\\}",
      "\\{\\\\textit\\{\\\\textbf\\{p\\}\\}\\\\textbf\\{-value\\}\\}")

  # Add column headers when table is split across pages
  column_headers <- model_fits_best_tex_update %>%
    str_match(., "(?s)toprule(.*?)\\\\midrule(?s)") %>% pluck(2) %>%
    str_replace_all(., "\\\\", "\\\\\\\\")
  model_fits_best_tex_final <- model_fits_best_tex_update %>%
    str_replace(., "\\\\midrule",
    paste0("\\\\endfirsthead\n \\\\\\hline", column_headers,
      "\\\\\\hline\n \\\\endhead\n \\\\\\hline\n \\\\endfoot\n \\\\\\hline\n ",
      "\\\\endlastfoot\n \\\\\\hline"))
  write_lines(model_fits_best_tex_final, file = file.path("C:/Users/blake",
    "OneDrive/Work/LaTeX/BMassey_Dissertation/Tables/Ch2",
    paste0("SSF_Fits_Terms_", str_to_title(i), ".tex")))
}

# ---------------------------------------------------------------------------- #
################################ OLD CODE ######################################
# ---------------------------------------------------------------------------- #


## SSF Fits ----------------------------------------------------------------- ##

# best_ssf_fit_all_org <- readRDS(file.path("Output/Analysis/SSF/Models",
#   "best_ssf_fit_all.rds"))
#
# ssf_fits_df  <- best_ssf_fit_all_org %>%
#   dplyr::select(step_type, fit_aicc, preds) %>%
#   mutate(step_type = str_replace_all(step_type, "_", " _ ")) %>%
#   mutate(step_type = str_to_title(step_type)) %>%
#   mutate(step_type = str_replace_all(step_type, " _ ", " $\\\\rightarrow$ "))%>%
#   mutate(preds = str_replace_all(preds, "_", " ")) %>%
#   mutate(preds = str_to_title(preds)) %>%
#   mutate(preds = str_replace_all(preds, "(?<=[:alpha:]) (?=[:alpha:])", ""))
#
# colnames(ssf_fits_df) <- colnames(ssf_fits_df) %>%
#   str_replace(., "step_type", "Step Type") %>%
#   str_replace(., "fit_aicc", "AICc") %>%
#   str_replace(., "preds", "Predictor Variables with Sigma")
#
#
# print(xtable(ssf_fits_df, digits = c(0, 0, 2, 0)),
#   sanitize.text.function = identity, latex.environments = "",
#   include.rownames = FALSE)
#
# ssf_fits_xtable <- xtable(ssf_fits_df, digits = c(0, 0, 2, 0))
# #  only.contents = FALSE, floating = FALSE)
#
# align(ssf_fits_xtable) <- c("L{0}", "L{.6}", "C{.25}", "L{2.15}")
#
# str_replace_all(align(ssf_fits_xtable), "[^[//.||0-9]]", "") %>%
#   str_subset(., "[0-9]") %>% as.numeric(.) %>% sum()
# # translates to relative column widths (first value, rownames, is ignored)
# # should sum to 3 for the 3 columns
# # "L" indicates left-aligned, ragged-right, no hypenation (check LaTeX preamble)
# # "H" indicates left-aligned, ragged-right, hypenation (check LaTeX preamble)
#
# # For LaTeX Folder
# print(ssf_fits_xtable,
#   floating = FALSE, width = "\\textwidth",
#   tabular.environment = "tabularx",
#   booktabs = TRUE, # thick top/bottom line, Preamble add "\usepackage{booktabs}"
#   include.rownames = FALSE,
#   size="\\fontsize{11pt}{12pt}\\selectfont",
#   sanitize.colnames.function=BoldText,
#   sanitize.text.function=identity,
#   file = file.path("C:/Users/blake/OneDrive/Work/LaTeX/BMassey_Dissertation",
#                    "Tables/Ch2/SSF_Fits.tex"))
