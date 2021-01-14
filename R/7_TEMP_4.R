################## Getting the Quadratic Components Incorporated ###############
library(tidyverse)

# Functions
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

################## Very Simple Model with Quadratic Term #######################

# Terms and data table
intercept <- -5
beta1 <- 35
beta1a <- -40
data_tbl <- tibble(
  var1 = seq(0, 1, by = .01),
  result = intercept + (beta1*var1) + (beta1a*var1^2),
  prob = plogis(result),
  response = rbinom(length(prob), 1, prob))

# Plot result, prob, and response
ggplot(data_tbl) +
  geom_point(aes(x = var1, y = result))

ggplot(data = data_tbl, aes(x = var1, y = prob)) +
  geom_point(color = "red") +
  binomial_smooth(formula = y ~ x + I(x^2))


# Fit model with and without quadratic
formulas <- list(response ~ var1,               # no
                 response ~ var1 + I(var1^2))   # with quadratic

model_fits <-  purrr::map_df(formulas,
  ~{mod <- glm(.x, data = data_tbl, family="binomial")
    tibble(formula = format(.x),
      AIC = round(AIC(mod),2),
      BIC = round(BIC(mod),2),
    R2adj = round(DescTools::PseudoR2(mod,which=c("McFaddenAdj")),4))
    }
  ) %>%
  arrange(desc(AIC))

model_fits




response <- var1 + var2              # both vars linear
response <- var1 + var2 + I(var2^2)  # add quad term for var2
response <- var1 + I(var2^2)        # only quad term for var2
response <- var1 + var2 + var3       # add var3, which is var2^2
response <- var1 + var3              # only var1 and var3







predictors <- seq(-.25, 1.25, by = .01)
intercept <- -5
beta1 <- 10
beta2 <- 1
predictors_logit <- intercept + beta1*(predictors) + beta2*(predictors^2)
df <- data.frame(predictors, probs = plogis(predictors_logit))
(y_mid_int <- (-(1*intercept/beta1)))
rect_df <- data.frame(xmin = c(-.25, 1), ymin=c(0,0), xmax= c(0,1.25),
  ymax = c(1,1))


# Simple logistic function with a quadratic term
set.seed(1)
intercept <- -5
pred1 <- seq(-.25, 1.25, by = .01)
pred2 <- seq(-.25, 1.25, by = .01)
beta1 <- 10
beta1sq <- -.1

df01 <- tibble(
  pred1 = pred1,
  pred1sq = pred1^2,
  pred2 = pred2,
  response = intercept + beta1*pred1 + beta1sq*pred1sq, #+ beta1sq*pred1sq,
  probs = plogis(response))

df01$response
df01$probs

ggplot(df01, aes(x = pred1, y = probs)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
    se = FALSE)

#  coord_cartesian(ylim = c(0,1))

ggplot(df,aes(x=var2,y=response)) +
  geom_point() +
  geom_smooth(method="loess")+
  coord_cartesian(ylim = c(0,1))

ggplot(df,aes(x=var1+var3,y=response)) +
  geom_point() +
  geom_smooth(method="loess")+
  coord_cartesian(ylim = c(0,1))

formulas <- list(response ~ var1 + var2,              # both vars linear
                 response ~ var1 + var2 + I(var2^2),  # add quad term for var2
                 response ~ var1 + I(var2^2),         # only quad term for var2
                 response ~ var1 + var2 + var3,       # add var3, which is var2^2
                 response ~ var1 + var3)              # only var1 and var3

# build a df of some model selection criteria:
selection <-  purrr::map_df(formulas,
  ~{mod <- glm(.x, data = df, family="binomial")
    data.frame(formula = format(.x),
      AIC = round(AIC(mod),2),
      BIC = round(BIC(mod),2),
    R2adj = round(DescTools::PseudoR2(mod,which=c("McFaddenAdj")),4))}) %>%
  arrange(desc(AIC))





x <- seq(-100, 100, by = 1)
x2 <- x^2
y <- -0.000462*x2 + 0.0265*x + 0

df <- bind_cols(y = y, x = x, x2 = x2)

ggplot(df) +
  geom_point(aes(x = x, y = y))+
  geom_line(aes(x = x, y = y))


covars_scale_sigmas <- c("x", "x2") #paste0(covars_scale, sigmas)
covars_fixed_0 <- NULL    #paste0(covars_fixed, "0")
preds <- paste(c("-1", covars_scale_sigmas), #covars_fixed_0
  collapse = " + ")
preds

glm_model_formula <- as.formula(paste('case', preds, sep = " ~ "))
glm_fit <- glm(glm_model_formula, data = ua_data, family = 'binomial')
