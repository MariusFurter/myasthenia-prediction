library(tidyverse)

### Data Preparation ###

###  UZH Valko Data ###
# Load data
df <- read.csv("./data/valko_data.csv", stringsAsFactors = T)

# Discretize age and besinger.Score
df$age <- as.numeric(df$age)
df$age <-
    cut(df$age,
        breaks = c(0, 50, 70, 120),
        ordered_result = T
    )
df$besinger.score <- as.numeric(df$besinger.score)
df$besinger.score <-
    cut(df$besinger.score,
        breaks = c(-1, 1, 4, 8, 24),
        ordered_result = T
    )

# Determine diplopia and ptosis
df$diplopia <- factor(
    ifelse(df$bes.diplopia > 0, "positive", "negative"),
    levels = c("negative", "positive")
)

df$ptosis <- factor(
    ifelse(df$bes.ptosis > 0, "positive", "negative"),
    levels = c("negative", "positive")
)

# Recode to factor
df$sex <- factor(recode(df$sex, `1` = "male", `2` = "female"), levels = c("male", "female"))

recode_negative_positive <- function(df, vars) {
    for (var in vars) {
        df[[var]] <- factor(recode(df[[var]],
            `0` = "negative",
            `1` = "positive"
        ), levels = c("negative", "positive"))
    }
    df
}

df <- recode_negative_positive(df, c(
    "mg",
    "endrophonium",
    "ice.test",
    "simpson.test",
    "sfemg",
    "rns.an.fn.max",
    "anti.achr"
))

# Remove unnecessary columns
df <- df %>% select(age, sex, diplopia, ptosis, anti.achr, simpson.test, ice.test, besinger.score, endrophonium, rns.an.fn.max, sfemg, mg)

save(df, file = "./data/valko_data.RData")

### UZH validation data ###
uzh_df <- read.csv("./data/uzh_validation.csv", stringsAsFactors = T, na.strings = c("n/a"))

# using qmg score as proxy for besinger score
uzh_df$besinger.score <- uzh_df$qmg.score

# Discretize Age and Besinger.Score
uzh_df$age <- as.numeric(uzh_df$age)
uzh_df$age <-
    cut(uzh_df$age,
        breaks = c(0, 50, 70, 120),
        ordered_result = T
    )

uzh_df$besinger.score <- as.numeric(uzh_df$besinger.score)
uzh_df$besinger.score <-
    cut(uzh_df$besinger.score,
        breaks = c(-1, 1, 4, 8, 24),
        ordered_result = T
    )

# Recode to factor
uzh_df$sex <- factor(recode(uzh_df$sex, "m" = "male", "w" = "female"), levels = c("male", "female"))

recode_negative_positive <- function(df, vars) {
    for (var in vars) {
        df[[var]] <- factor(recode(df[[var]],
            `0` = "negative",
            `1` = "positive"
        ), levels = c("negative", "positive"))
    }
    df
}

uzh_df <- recode_negative_positive(uzh_df, c(
    "mg",
    "endrophonium",
    "ice.test",
    "simpson.test",
    "sfemg",
    "rns.an.fn.max",
    "anti.achr",
    "ptosis",
    "diplopia"
))

uzh_df <- uzh_df %>% select(age, sex, diplopia, ptosis, anti.achr, simpson.test, ice.test, besinger.score, endrophonium, rns.an.fn.max, sfemg, mg)

save(uzh_df, file = "./data/uzh_validation.RData")

### Canada validation data ###
canada_df <- read.csv("./data/canada_validation.csv", stringsAsFactors = T)

# Discretize Age
canada_df$age <- as.numeric(canada_df$age)
canada_df$age <-
    cut(canada_df$age,
        breaks = c(0, 50, 70, 120),
        ordered_result = T
    )

# Recode to factor
canada_df$diplopia <- factor(recode(canada_df$diplopia, "no" = "negative", "yes" = "positive"), levels = c("negative", "positive"))
canada_df$ptosis <- factor(recode(canada_df$ptosis, "no" = "negative", "yes" = "positive"), levels = c("negative", "positive"))
canada_df$myasthenia <- factor(recode(canada_df$myasthenia, "no" = "negative", "yes" = "positive"), levels = c("negative", "positive"))

canada_df <- rename(canada_df, simpson.test = simpson_test, ice.test = ice_test, anti.achr = acetylcholine_receptor_antibody, mg = myasthenia)

# Add missing columns as NA
canada_df$besinger.score <- NA
canada_df$sfemg <- NA
canada_df$rns.an.fn.max <- NA
canada_df$endrophonium <- NA

canada_df <- canada_df %>% select(age, sex, diplopia, ptosis, anti.achr, simpson.test, ice.test, besinger.score, endrophonium, rns.an.fn.max, sfemg, mg)

save(canada_df, file = "./data/canada_validation.RData")
