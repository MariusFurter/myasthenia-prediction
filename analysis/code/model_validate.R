library(bnlearn)
library(tidyverse)
library(gtools)
library(Metrics)

# Set random seed for reproducibility
set.seed(42)

# Load data
load("./data/valko_data.RData") # loaded as df
load("./data/uzh_validation.RData") # loaded as uzh_df
load("./data/canada_validation.RData") # loaded as canada_df

# Load fitting function (for cross-validation)
load("./data/fit_models.RData") # loaded as fit_models

# Load models
load("./data/models.RData") # loaded as models

# Take test data and variables as input and return the mean error rate
# If models are provided, use them for prediction. Otherwise, fit models to training data
validate <- function(test_data, variables, models = NA, train_data = NA, threshold = 0.5) {
    if (missing(models) && missing(train_data)) {
        stop("Either models or train_data must be provided")
    }

    # Reduce to complete cases
    test_data <- test_data %>%
        select(all_of(variables), mg) %>%
        drop_na()

    # Fit models to training data if models not provided
    if (missing(models)) {
        models <- fit_models(train_data, n = 100, ci = FALSE)
    }

    # Calculate error rate between predictions and actual values
    error_rate <- function(model, test_data) {
        predictions <- test_data %>%
            rowwise() %>%
            mutate(predictions = cpquery(model, event = (mg == "positive"), evidence = as.list(across(all_of(variables))), method = "lw")) %>%
            mutate(binary_predictions = ifelse(predictions > threshold, "positive", "negative")) %>%
            pull(binary_predictions)
        mean(predictions != test_data$mg)
    }

    error_rates <- lapply(models, error_rate, test_data)
    mean_error_rate <- mean(unlist(error_rates))
    mean_error_rate
}

# Calculate area under receiver operator curve (AUROC) for each model in models and return their mean
# If models are provided, use them for prediction. Otherwise, fit models to training data
auroc <- function(test_data, variables, models = NA, train_data = NA) {
    if (missing(models) && missing(train_data)) {
        stop("Either models or train_data must be provided")
    }

    # Reduce to complete cases
    test_data <- test_data %>%
        select(all_of(variables), mg) %>%
        drop_na()

    # Fit models to training data if models not provided
    if (missing(models)) {
        models <- fit_models(train_data, n = 100, ci = FALSE)
    }

    # Calculate error rate between predictions and actual values
    model_auroc <- function(model, test_data) {
        predictions <- test_data %>%
            rowwise() %>%
            mutate(predictions = cpquery(model, event = (mg == "positive"), evidence = as.list(across(all_of(variables))), method = "lw")) %>%
            pull(predictions)
        a <- auc(actual = test_data$mg == "positive", predicted = predictions)
        if (is.nan(a)) {
            1
        } # perfect predictions of 0 yield NaN in auc function.
        else {
            a
        }
    }

    aurocs <- lapply(models, model_auroc, test_data)
    mean_auroc <- mean(unlist(aurocs))
    mean_auroc
}

### Cross-validation ###

cv <- function(variables) {
    # split df into 10 folds
    folds <- split(df[sample(nrow(df)), ], 1:10)

    # calculate error rate for each fold
    fold_errors <- lapply(1:10, function(i) {
        test_data <- folds[[i]]
        train_data <- bind_rows(folds[-i])
        validate(test_data = test_data, variables = variables, train_data = train_data)
    })

    fold_auroc <- lapply(1:10, function(i) {
        test_data <- folds[[i]]
        train_data <- bind_rows(folds[-i])
        auroc(test_data = test_data, variables = variables, train_data = train_data)
    })

    # return mean error rate
    c(mean(unlist(fold_errors)), mean(unlist(fold_auroc)))
}

# Split strings of variables by comma
# Throws an ignorable length warning
split_vars <- function(variables) {
    unlist(strsplit(variables, split = ", "))
}

# Construct training data error table
training_error_table <- tibble(variables = c(
    "age, sex",
    "diplopia, ptosis",
    "anti.achr",
    "simpson.test",
    "ice.test",
    "besinger.score",
    "endrophonium",
    "rns.an.fn.max",
    "sfemg",
    "endrophonium, anti.achr",
    "age, sex, diplopia, ptosis, simpson.test",
    "age, sex, diplopia, ptosis, anti.achr, simpson.test"
)) %>%
    rowwise() %>%
    mutate(cases = select(df, all_of(split_vars(variables))) %>% drop_na() %>% nrow()) %>%
    mutate(mean_error = validate(test_data = df, variables = split_vars(variables), models = models) %>% signif(3)) %>%
    mutate(mean_auroc = auroc(test_data = df, variables = split_vars(variables), models = models) %>% signif(3))

write.csv(training_error_table, "./data/validation/training_error_table.csv")

# Construct cross-validation table
cv_table <- tibble(variables = c(
    "age, sex",
    "diplopia, ptosis",
    "anti.achr",
    "simpson.test",
    "ice.test",
    "besinger.score",
    "endrophonium",
    "rns.an.fn.max",
    "sfemg",
    "endrophonium, anti.achr",
    "age, sex, diplopia, ptosis, simpson.test",
    "age, sex, diplopia, ptosis, anti.achr, simpson.test"
)) %>%
    rowwise() %>%
    mutate(cases = select(df, all_of(split_vars(variables))) %>% drop_na() %>% nrow()) %>%
    mutate(cv = list(cv(split_vars(variables)))) %>%
    mutate(mean_error = cv[1] %>% signif(3), mean_auroc = cv[2] %>% signif(3)) %>%
    select(!cv)

write.csv(cv_table, "./data/validation/cv_table.csv")

### UZH Data Validation ###

# Construct validation table
uzh_validation_table <- tibble(variables = c(
    "age, sex",
    "diplopia, ptosis",
    "anti.achr",
    "simpson.test",
    "ice.test",
    "besinger.score",
    "endrophonium",
    "rns.an.fn.max",
    "sfemg",
    "endrophonium, anti.achr",
    "age, sex, diplopia, ptosis, simpson.test",
    "age, sex, diplopia, ptosis, anti.achr, simpson.test"
)) %>%
    rowwise() %>%
    mutate(cases = select(uzh_df, all_of(split_vars(variables))) %>% drop_na() %>% nrow()) %>%
    mutate(mean_error = validate(test_data = uzh_df, variables = split_vars(variables), models = models) %>% signif(3)) %>%
    mutate(mean_auroc = auroc(test_data = uzh_df, variables = split_vars(variables), models = models) %>% signif(3))

write.csv(uzh_validation_table, "./data/validation/uzh_validation_table.csv")

### Canada Data Validation ###

# Construct validation table
canada_validation_table <- tibble(variables = c(
    "age, sex",
    "diplopia, ptosis",
    "anti.achr",
    "simpson.test",
    "ice.test",
    "age, sex, diplopia, ptosis, simpson.test",
    "age, sex, diplopia, ptosis, anti.achr, simpson.test"
)) %>%
    rowwise() %>%
    mutate(cases = select(canada_df, all_of(split_vars(variables))) %>% drop_na() %>% nrow()) %>%
    mutate(mean_error = validate(test_data = canada_df, variables = split_vars(variables), models = models) %>% signif(3)) %>%
    mutate(mean_auroc = auroc(test_data = uzh_df, variables = split_vars(variables), models = models) %>% signif(3))

write.csv(canada_validation_table, "./data/validation/canada_validation_table.csv")
