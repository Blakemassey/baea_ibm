library(pacman)
p_load(DT, lubridate, tidyverse, ggplot2, readr, xtable)
suppressMessages(extrafont::loadfonts(device="win"))
options(stringsAsFactors = FALSE)

pkg_dir <- "C:/Users/Blake/OneDrive/Work/R/Projects/multiscale_optim"
tex_dir <- "C:/Users/Blake/OneDrive/Work/LaTeX/BMassey_Dissertation"

BoldText <- function(x) {paste0('{\\textbf{',x,'}}')}
BoldTextCentered <- function(x) {paste0('\\multicolumn{1}{c}{\\textbf{', x,
  '}}')}

# Theme (for LaTeX font)
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 14))

for (i in start_step_types){
  best_fit_i <- best_fit_all %>% filter(start_behavior == i)
  xtable_list <- vector(mode = "list", length = nrow(best_fit_i))
  for (j in seq_len(nrow(best_fit_i))){
    fit_ij <- best_fit_i %>% slice(j) %>% pull(clogit_fit) %>% pluck(1)
    start_ij <- best_fit_i %>% slice(j) %>% pull(start_behavior)
    end_ij <- best_fit_i %>% slice(j) %>% pull(end_behavior)
    xtable_fit_ij <- as.data.frame(xtable(fit_ij)) %>%
      rownames_to_column(., var = "term") %>%
      mutate(step_type = NA)
    xtable_fit_ij[1, "step_type"] <- paste(str_to_title(start_ij),
      "$\\rightarrow$", str_to_title(end_ij))
    xtable_list[[j]] <- xtable_fit_ij
  }
  ssf_best_fits_xtable <- xtable_list %>%
    reduce(bind_rows) %>%
    mutate(term = str_replace_all(term, "_", " ")) %>%
    mutate(term = str_replace_all(str_to_title(term),
      "(?<=[:alpha:]) (?=[:alpha:])", "")) %>%
    mutate(term = str_replace_all(term, "DevelopedDist0", "DevelopedDist"))%>%
    mutate(term = str_replace_all(term, "HydroDist0", "HydroDist")) %>%
    mutate(term = str_replace_all(term, "TurbineDist0", "TurbineDist")) %>%
    rename("Step Type" = step_type,
           "Term" = term,
           "Coefficient" = coef,
           "Exp(Coef)" = "exp(coef)",
           "SE(Coef)" = "se(coef)",
           "Z Statistic" = "z",
           "p-value" = "p") %>%
    dplyr::select("Step Type", "Term", "Coefficient", "Exp(Coef)",
      "SE(Coef)", "Z Statistic", "p-value")

  ssf_best_fits_xtable <- xtable(ssf_best_fits_xtable,
    digits = c(0, 0, 2, 3, 2, 2, 2, 2))
  display(ssf_best_fits_xtable) = c("s", "s", "s", "g", "g", "g", "g", "g")

  #L{1.6}L{1.25}R{1}R{1}R{1}R{.4}R{.75} # Without vertical col name
  #L{1.8}H{1.25}R{1}R{.9}R{.8}R{.45}R{.8} # With veritcal col name
  align(ssf_best_fits_xtable) <- c("L{0}", "L{1.8}", "H{1.25}", "R{1}",
    "R{.9}", "R{.8}", "R{.45}", "R{.8}")
  str_replace_all(align(ssf_best_fits_xtable), "[^[//.||0-9]]", "") %>%
    str_subset(., "[0-9]") %>% as.numeric(.) %>% sum()

  hline <- (which(str_detect(ssf_best_fits_xtable$`Step Type`,
   "[:alpha:]"))-1)[-1]
  htype <- c(rep("\\midrule ", times = length(hline)))

  ssf_xtable_list_tex <- print(ssf_best_fits_xtable,
    add.to.row = list(pos = as.list(hline), command = htype),
    floating = FALSE, width = "\\textwidth",
    tabular.environment = "tabularx",
    booktabs = TRUE, # thick top/bottom line, Preamble: "\usepackage{booktabs}"
    include.rownames = FALSE,
    rotate.colnames = TRUE,
    size = "\\fontsize{11pt}{12pt}\\selectfont",
    sanitize.colnames.function = BoldText,
    sanitize.text.function = identity,
    print.results = FALSE)

  # Make column header ("Step Type") horizontal, italices 'p' in p-value
  ssf_xtable_list_tex_final <- ssf_xtable_list_tex %>%
    str_replace_all(., paste0("\\\\begin\\{sideways\\} ",
      "\\{\\\\textbf\\{Step Type\\}\\} \\\\end\\{sideways\\}"),
      "\\{\\\\textbf\\{Step Type\\}\\}") %>%
    str_replace_all(., "\\{\\\\textbf\\{p-value\\}\\}",
    "\\{\\\\textit\\{\\\\textbf\\{p\\}\\}\\\\textbf\\{-value\\}\\}")

  write_lines(ssf_xtable_list_tex_final, path = file.path("C:/Users/blake",
    "OneDrive/Work/LaTeX/BMassey_Dissertation/Tables/Ch2",
    paste0("SSF_Fits_Terms_", str_to_title(i),".tex")))

}

# Test align vector equals 7
align_vec <- "L{1.8}R{1.25}R{1}R{.9}R{.8}R{.45}R{.8}"

str_split(align_vec, "\\}", simplify = TRUE) %>%
  str_replace_all(., "[^[//.||0-9]]", "") %>%
  str_subset(., "[0-9]") %>% as.numeric(.) %>% sum()

















#
# xtable_i <- as.data.frame(xtable(fit_i)) %>%
#     rownames_to_column(.) %>%
#     mutate(term = str_replace_all(rowname, "_", " ")) %>%
#     mutate(term = str_replace_all(str_to_title(term),
#       "(?<=[:alpha:]) (?=[:alpha:])", "")) %>%
#     mutate(term = str_replace_all(term, "DevelopedDist0", "DevelopedDist"))%>%
#     mutate(term = str_replace_all(term, "HydroDist0", "HydroDist")) %>%
#     mutate(term = str_replace_all(term, "TurbineDist0", "TurbineDist")) %>%
#     mutate("Step Type" = c(step_type_i, rep(NA, n()-1))) %>%
#     rename("Term" = "term",
#            "Coefficient" = coef,
#            "Exp(Coef)" = "exp(coef)",
#            "SE(Coef)" = "se(coef)",
#            "Z" = "z",
#            "P" = "p") %>%
#     dplyr::select("Step Type", "Term", "Coefficient", "exp(Coef)",
#       "Std. Err. (Coef)", "Z", "P")
#   xtable_list[[i]] <- xtable_i
#
#     }
# }
#   step_type_i <- best_fit_all %>% slice(i) %>% pull(step_type) %>%
#     str_replace(., "_", " ") %>%
#     str_to_title(.) %>%
#     str_replace_all(., " ", " $\\\\rightarrow$ ")
#   fit_i <- best_fit_all %>% slice(i) %>% pull(clogit_fit) %>% pluck(1)
#   xtable_i <- as.data.frame(xtable(fit_i)) %>%
#     rownames_to_column(.) %>%
#     mutate(term = str_replace_all(rowname, "_", " ")) %>%
#     mutate(term = str_replace_all(str_to_title(term),
#       "(?<=[:alpha:]) (?=[:alpha:])", "")) %>%
#     mutate(term = str_replace_all(term, "DevelopedDist0", "DevelopedDist"))%>%
#     mutate(term = str_replace_all(term, "HydroDist0", "HydroDist")) %>%
#     mutate(term = str_replace_all(term, "TurbineDist0", "TurbineDist")) %>%
#     mutate("Step Type" = c(step_type_i, rep(NA, n()-1))) %>%
#     rename("Term" = "term",
#            "Coefficient" = coef,
#            "Exp(Coef)" = "exp(coef)",
#            "SE(Coef)" = "se(coef)",
#            "Z" = "z",
#            "P" = "p") %>%
#     dplyr::select("Step Type", "Term", "Coefficient", "exp(Coef)",
#       "Std. Err. (Coef)", "Z", "P")
#   xtable_list[[i]] <- xtable_i
# }
#
# ssf_best_fits_xtable <- xtable_list %>%
#   reduce(bind_rows)
#
#
# ssf_best_fits_xtable <- xtable(ssf_best_fits_xtable,
#   digits = c(0, 0, 2, 2, 2, 2, 2, 2))
# display(ssf_best_fits_xtable) = c("s", "s", "s", "g", "g", "g", "g", "g")
# align(ssf_best_fits_xtable) <- c("L{0}", "C{2}", "C{1.5}", "C{.75}",
#   "L{.75}", "L{1}", "L{.5}", "L{.5}")
# str_replace_all(align(ssf_best_fits_xtable), "[^[//.||0-9]]", "") %>%
#   str_subset(., "[0-9]") %>% as.numeric(.) %>% sum()
# # translates to relative column widths (first value, rownames, is ignored)
# # should sum to 7 for the 7 columns
# # "L" indicates left-aligned, ragged-right, no hypenation (see LaTeX preamble)
# # "H" indicates left-aligned, ragged-right, hypenation (see LaTeX preamble)
# # For LaTeX Folder
# print(ssf_best_fits_xtable,
#   floating = FALSE, width = "\\textwidth",
#   tabular.environment = "tabularx",
#   booktabs = TRUE, # thick top/bottom line, Preamble: "\usepackage{booktabs}"
#   include.rownames = FALSE,
#   size = "\\fontsize{11pt}{12pt}\\selectfont",
#   sanitize.colnames.function = BoldTextCentered,
#   sanitize.text.function = identity,
#   file = file.path("C:/Users/blake/OneDrive/Work/LaTeX/BMassey_Dissertation",
#     "Tables/Ch2/SSF_Fits.tex")
# )
#


# require(broom)
#
# for (i in seq_len(nrow(best_fit_all))){
#   fit_i <- best_fit_all %>% slice(i) %>% pull(clogit_fit) %>% pluck(1)
#   xtable_list[[i]] <- tidy(fit_i)
# }
#
# attr(xtable_list, "subheadings") <- best_fit_all$step_type %>%
#     str_replace_all(., "_", " ") %>%
#     str_to_title(.) %>%
#     str_replace_all(., " ", " $\\\\rightarrow$ ")
#
# ssf_xtable_list <- xtableList(xtable_list)
#
# xtable_list_align <- c("L{0} C{1} C{1} C{1} C{1} C{1} C{1} C{1}")
# str_replace_all(xtable_list_align, "[^[//.||0-9]]", "") %>%
#   str_subset(., "[0-9]") %>% as.numeric(.) %>% sum()
#
# # For LaTeX Folder
# ssf_xtable_list_tex <- print.xtableList(ssf_xtable_list,
#   align = xtable_list_align,
#   floating = FALSE, width = "\\textwidth",
#   tabular.environment = "tabularx",
#   booktabs = TRUE, # thick top/bottom line, Preamble add "\usepackage{booktabs}"
#   include.rownames = FALSE,
#   size = "\\fontsize{11pt}{12pt}\\selectfont",
#   sanitize.colnames.function = BoldTextCentered,
#   sanitize.text.function = identity,
#   file = file.path("C:/Users/blake/OneDrive/Work/LaTeX/BMassey_Dissertation",
#                     "Tables/Ch2/SSF_Fits.tex")
#   )




# write_lines(ssf_xtable_list_tex, path = file.path("C:/Users/blake/OneDrive",
#   "Work/LaTeX/BMassey_Dissertation", "Tables/Ch2/SSF_Fits.tex"))
#
# ssf_fits_df  <- best_ssf_fit_all_org %>%
#   dplyr::select(step_type, fit_aicc, preds) %>%
#   mutate(step_type = str_replace_all(step_type, "_", "  ")) %>%
#   mutate(step_type = str_to_title(step_type)) %>%
#   mutate(step_type = str_replace_all(step_type, "  ", " $\\\\rightarrow$ ")) %>%
#   mutate(preds = str_replace_all(preds, "_", " ")) %>%
#   mutate(preds = str_to_title(preds)) %>%
#   mutate(preds = str_replace_all(preds, "(?<=[:alpha:]) (?=[:alpha:])", "")) %>%
#   mutate(preds = str_replace_all(preds, "DevelopedDist0", "DevelopedDist")) %>%
#   mutate(preds = str_replace_all(preds, "HydroDist0", "HydroDist")) %>%
#   mutate(preds = str_replace_all(preds, "TurbineDist0", "TurbineDist"))
#
# colnames(ssf_fits_df) <- colnames(ssf_fits_df) %>%
#   str_replace(., "step_type", "Step Type") %>%
#   str_replace(., "fit_aicc", "Fit AICc") %>%
#   str_replace(., "preds", "Predictor Variables with Sigma")
#
# print(xtable(ssf_fits_df, digits = c(0, 0, 2, 0)),
#   sanitize.text.function = identity, latex.environments = "",
#   include.rownames = FALSE)
#
# ssf_fits_xtable <- xtable(ssf_fits_df, digits = c(0, 0, 2, 0))
# #  only.contents = FALSE, floating = FALSE)
#
# align(ssf_fits_xtable) <- c("L{0}", "L{.75}", "C{.25}", "L{2}")
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
#   sanitize.colnames.function=BoldTextCentered,
#   sanitize.text.function=identity,
#   file = file.path("C:/Users/blake/OneDrive/Work/LaTeX/BMassey_Dissertation",
#                    "Tables/Ch2/SSF_Fits.tex"))
