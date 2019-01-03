library(stringr)
library(xtable)
library(DT)
options(stringsAsFactors = FALSE)

# SSF Analysis Landscape Covariates for

ssf_land_covar <- read.csv("Data/Assets/ssf_landscape_covariates.csv",
  row.names = NULL)
DT::datatable(head(ssf_land_covar), editable = TRUE)
colnames(ssf_land_covar) <- str_replace_all(colnames(ssf_land_covar), "\\.",
  " ")
write.csv(ssf_land_covar, "Data/Assets/ssf_landscape_covariates.csv",
  row.names = NULL)

ssf_land_covar_xtable <- xtable(ssf_land_covar,
  only.contents = TRUE, floating = FALSE,
  caption = "My caption\\label{tab:SSF_Landscape_Covariates}",
  label="tab:SSF_Landscape_Covariates")
print(ssf_land_covar_xtable, floating = FALSE, include.rownames=FALSE,
  file = "Products/Tables/SSF_Landscape_Covariates.tex")


