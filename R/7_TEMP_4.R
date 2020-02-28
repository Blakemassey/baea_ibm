## Load survival package
library(survival)

## Create outcome variable, then survival object
pbc <- within(pbc, {
    status.dichotomous <- status > 1
    survival.vector    <- Surv(time, status.dichotomous)
})


## Create vectors for outcome and predictors
outcome    <- c("survival.vector")
predictors <- c("trt", "age", "sex", "ascites","hepato","spiders")
dataset    <- pbc


## The lines below should not need modification.

## Create list of models
list_of_models <- lapply(seq_along((predictors)), function(n) {

    left_hand_side  <- outcome
    right_hand_side <- apply(X = combn(predictors, n), MARGIN = 2, paste, collapse = " + ")

    paste(left_hand_side, right_hand_side, sep = "  ~  ")
})

## Convert to a vector
vector_of_models <- unlist(list_of_models)

## Fit coxph to all models
list_of_fits <- lapply(vector_of_models, function(x) {

    formula    <- as.formula(x)
    fit        <- coxph(formula, data = dataset)
    result_AIC <- extractAIC(fit)

    data.frame(num_predictors = result_AIC[1],
               AIC            = result_AIC[2],
               model          = x)
})

## Collapse to a data frame
result <- do.call(rbind, list_of_fits)


## Sort and print
library(doBy)
orderBy(~ AIC, result)
