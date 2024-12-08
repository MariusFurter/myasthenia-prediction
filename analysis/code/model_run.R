library(bnlearn)

# Set random seed for reproducibility
set.seed(42)

# Load models
load("./data/models.RData")

query_models <- function(evidence) {
    lapply(models, function(m) cpquery(m, event = (mg == "positive"), evidence = evidence, method = "lw"))
}

# returns 100 samples of the probability of MG being positive given the evidence
predict_mg <- function(age = NA, sex = NA, rns.an.fn.max = NA, sfemg = NA, simpson.test = NA, endrophonium = NA, ice.test = NA, anti.achr = NA, besinger.score = NA, diplopia = NA, ptosis = NA) {
    evidence <- list(
        age = factor(age, levels = c("(0,50]", "(50,70]", "(70,120]")),
        sex = factor(sex, levels = c("male", "female")),
        rns.an.fn.max = factor(rns.an.fn.max, levels = c("negative", "positive")),
        sfemg = factor(sfemg, levels = c("negative", "positive")),
        simpson.test = factor(simpson.test, levels = c("negative", "positive")),
        endrophonium = factor(endrophonium, levels = c("negative", "positive")),
        ice.test = factor(ice.test, levels = c("negative", "positive")),
        anti.achr = factor(anti.achr, levels = c("negative", "positive")),
        besinger.score = factor(besinger.score, levels = c("(-1,1]", "(1,4]", "(4,8]", "(8,24]")),
        diplopia = factor(diplopia, levels = c("negative", "positive")),
        ptosis = factor(ptosis, levels = c("negative", "positive"))
    )
    evidence <- Filter(function(x) !is.na(x), evidence)
    unlist(query_models(evidence = evidence))
}

### Example usage ###
res <- predict_mg(endrophonium = "negative")
median(res)
quantile(res, c(0.025, 0.975))
