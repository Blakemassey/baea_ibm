library(stringr)
library(xtable)
options(stringsAsFactors = FALSE)

# SSF Analysis Landscape Covariates for
ssf_land_covar <- read.csv("Data/Assets/ssf_landscape_covariates.csv",
  row.names = NULL)
colnames(ssf_land_covar) <- str_replace_all(colnames(ssf_land_covar), "\\.",
  " ")
ssf_land_covar_xtable <- xtable(ssf_land_covar,
  only.contents = TRUE, floating = FALSE,
  caption = "My caption\\label{tab:SSF_Landscape_Covariates}",
  label="tab:SSF_Landscape_Covariates")
print(ssf_land_covar_xtable, floating = FALSE, include.rownames=FALSE,
  file = "Products/Tables/SSF_Landscape_Covariates.tex")


