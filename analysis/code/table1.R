library(tidyverse)

load("./data/valko_data.RData")
load("./data/uzh_validation.RData")
load("./data/canada_validation.RData")

make_table1 <- function(data) {
    variables <- colnames(data) %>% setdiff("mg")

    # Count number of outcomes for each variable for mg positive and negative
    table1 <- data.frame()

    for (var in variables) {
        v <- data %>%
            group_by(across(all_of(c("mg", var)))) %>%
            tally() %>%
            pivot_wider(names_from = mg, values_from = n, values_fill = 0) %>%
            pivot_longer(cols = {{ var }}, names_to = "variable") %>%
            mutate(total = negative + positive) %>%
            select(variable, value, negative, positive, total) %>%
            arrange(variable, value)
        table1 <- rbind(table1, v)
    }
    table1
}

write.csv(make_table1(df), "./data/table1/study_table1.csv")
write.csv(make_table1(uzh_df), "./data/table1/uzh_table1.csv")
write.csv(make_table1(canada_df), "./data/table1/canada_table1.csv")
