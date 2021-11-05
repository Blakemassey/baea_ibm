library(prettyunits)
x <- "-0.001812*DistHydro"

char_string <- "-0.03842*TPI7 + 11.2*Wetland100 + -0.001757*DistHydro + 0.0002934*DistTurbine"
r1 <- as.numeric(str_extract_all(x, "-*\\d+\\.*\\d*(?=[:punct:])")[[1]])
r1 <- signif(r1, 2)
r2 <- str_split(x, "-*\\d+\\.*\\d*(?=[:punct:])")[[1]]
paste0(r2, r1, collapse = "")

UpdateSignifDigits <- function(char_string, sig_digits = 2){
  str1 <- as.numeric(str_extract_all(char_string, "-*\\d+\\.*\\d*") %>% unlist(.))
  str1 <- as.character(c(signif(str1, sig_digits), ""))
  str2 <- str_split(char_string, "-*\\d+\\.*\\d*") %>% unlist(.)
  str_final <- paste0(str2, str1, collapse = "")
  return(str_final)
}

UpdateSignifDigits(x)

length(r1)
length(r2)

r1 <- as.numeric(str_extract_all(x, "-*\\d+\\.*\\d*")[[1]])

str_match(a, "STR1\\s*(.*?)\\s*STR2")

r2 <- str_extract_all(x, "\\*(.*?)\\s") %>% unlist(.)

r2 <- strsplit(gsub("\\d", "", x),"\\.")[[1]]
# [1] "Hello" "World"
paste0(r2, format(r1, digits = 3, trim=T), collapse = "")

