library(tidyverse)
library(knitr)
library(pander)
library(gtools)
library(bnlearn)

# Set random seed for reproducibility
set.seed(42)

# Load data
load("./data/valko_data.RData")

### Model Fitting ###
fit_models <- function(df, n = 100, ci = FALSE, counts = FALSE) {
  # fit n models to df. Returns a list with the fitted models. If ci = TRUE, calculate 95% CI for the fitted model cpds. If counts = TRUE, return count tables for each variable.

  # Count combinations for each variable in vars
  tibble_combinations <- function(vars, df) {
    df %>%
      select(all_of(vars)) %>%
      drop_na() %>%
      group_by(across(all_of(vars)), .drop = FALSE) %>%
      count() %>%
      ungroup()
  }

  # Convert a tibble of combinations of vars to an array (required format for bnlearn)
  convert_to_array <- function(tibble, vars, value_name) {
    t <- tibble %>%
      select(all_of(rev(vars)), {{ value_name }}) %>%
      arrange(across(everything())) %>%
      pull({{ value_name }})
    dim(t) <- sapply(vars, function(var) length(levels(tibble[[var]])))
    l <- lapply(vars, function(var) levels(tibble[[var]]))
    names(l) <- vars
    dimnames(t) <- l
    t
  }

  # Nodes and their parents
  nodes <- list(
    "age" = c("age"),
    "sex" = c("sex"),
    "mg" = c("mg", "sex"),
    "rns.an.fn.max" = c("rns.an.fn.max", "mg"),
    "sfemg" = c("sfemg", "mg"),
    "simpson.test" = c("simpson.test", "mg"),
    "endrophonium" = c("endrophonium", "mg"),
    "ice.test" = c("ice.test", "mg"),
    "anti.achr" = c("anti.achr", "age", "mg"),
    "besinger.score" = c("besinger.score", "age", "mg"),
    "diplopia" = c("diplopia", "age", "mg"),
    "ptosis" = c("ptosis", "diplopia", "mg")
  )

  # Create count tables
  count_tibbles <- lapply(nodes, tibble_combinations, df)
  count_tables <- lapply(seq_along(count_tibbles), function(i) {
    convert_to_array(count_tibbles[[i]], nodes[[i]], "n")
  })
  names(count_tables) <- names(count_tibbles)

  # Calculate 95% CI from count tables
  dirichlet_ci95 <- function(table) {
    ndim <- length(dim(table))
    if (ndim == 1) {
      t <- rdirichlet(n = 10000, alpha = table + 1)
      t <- apply(t, 2, function(samps) {
        m <- median(samps)
        l <- quantile(samps, 0.025)
        r <- quantile(samps, 0.975)
        paste0("(", signif(l, 3), ", ", signif(m, 3), ", ", signif(r, 3), ")")
      })
      dim(t) <- c(1, length(t))
      dimnames(t) <- c(list(NULL), dimnames(table))
      t
    } else if (ndim == 2) {
      t <- apply(table, 2, function(x) {
        samples <- rdirichlet(n = 10000, alpha = x + 1)
        ci <- apply(samples, 2, function(samps) {
          m <- median(samps)
          l <- quantile(samps, 0.025)
          r <- quantile(samps, 0.975)
          paste0("(", signif(l, 3), ", ", signif(m, 3), ", ", signif(r, 3), ")")
        })
        ci
      })
      dimnames(t) <- dimnames(table)
      t
    } else if (ndim == 3) {
      t <- apply(table, c(2, 3), function(x) {
        samples <- rdirichlet(n = 10000, alpha = x + 1)
        ci <- apply(samples, 2, function(samps) {
          m <- median(samps)
          l <- quantile(samps, 0.025)
          r <- quantile(samps, 0.975)
          paste0("(", signif(l, 3), ", ", signif(m, 3), ", ", signif(r, 3), ")")
        })
        ci
      })
      dimnames(t) <- dimnames(table)
      t
    } else {
      stop("Too many dimensions")
    }
  }

  # Calculate 95% CI for all count tables and save result
  dirichlet_ci95_tables <- lapply(count_tables, dirichlet_ci95)
  names(dirichlet_ci95_tables) <- names(count_tables)

  # Take count table and sample from Dirichlet distribution
  dirichlet_samples <- function(table) {
    ndim <- length(dim(table))
    if (ndim == 1) {
      t <- rdirichlet(n = 1, alpha = table + 1)
      dimnames(t) <- list(NULL, dimnames(table)[[1]])
      t
    } else if (ndim == 2) {
      t <- apply(table, 2, function(x) rdirichlet(n = 1, alpha = x + 1))
      dimnames(t) <- dimnames(table)
      t
    } else if (ndim == 3) {
      t <- apply(table, c(2, 3), function(x) rdirichlet(n = 1, alpha = x + 1))
      dimnames(t) <- dimnames(table)
      t
    } else {
      stop("Too many dimensions")
    }
  }

  # Sample n cpd sets for all variables
  sampled_cpds <- replicate(n, lapply(count_tables, dirichlet_samples), simplify = F)

  # Define network structure
  net <- model2network(paste0(
    "[sex][age][mg|sex][rns.an.fn.max|mg][sfemg|mg]",
    "[simpson.test|mg][endrophonium|mg][ice.test|mg]",
    "[anti.achr|age:mg][besinger.score|age:mg][diplopia|age:mg][ptosis|diplopia:mg]"
  ))

  # For each sampled cpd, generate Bayesian network
  models <- lapply(sampled_cpds, function(cpd) {
    custom.fit(net, cpd, ordinal = c("age", "besinger.score"))
  })

  if (ci && counts) {
    return(list(models = models, dirichlet_ci95_tables = dirichlet_ci95_tables, count_tables = count_tables))
  }
  if (ci) {
    return(list(models = models, dirichlet_ci95_tables = dirichlet_ci95_tables))
  } else {
    return(models)
  }
}

# Save function so it can be used in cross-validation
save(fit_models, file = "./data/fit_models.RData")

# Fit models to df
fit <- fit_models(df, n = 100, ci = TRUE, counts = TRUE)
dirichlet_ci95_tables <- fit$dirichlet_ci95_tables
count_tables <- fit$count_tables
models <- fit$models

# Save 95% CI tables
for (n in names(dirichlet_ci95_tables)) {
  f <- paste0("./data/cpd_tables/", n, "_ci95.csv")
  table <- dirichlet_ci95_tables[[n]]

  # Transpose table
  if (length(dim(table)) == 2 && !(n %in% c("age", "sex"))) {
    table <- aperm(table, c(2, 1))
  } else if (length(dim(table)) == 3) {
    table <- aperm(table, c(3, 2, 1))
  }
  table <- stats:::format.ftable(ftable(table), quote = F)
  write.csv(table, file = f)
}

# Save models
save(models, file = "./data/models.RData")
