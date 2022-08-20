library(tree)

snakedata <- read.table("project_Galin/allsnakedata.txt", header = TRUE, na.strings = ".", stringsAsFactors = TRUE)
names(snakedata)


###########################
# Add some util variables #
###########################

# outcome as numeric variable: 1: death; 0: survival.
Lindex <- which(snakedata$Outcome == "L")
Dindex <- which(snakedata$Outcome == "D")
snakedata$Outcome01 <- snakedata$Outcome
levels(snakedata$Outcome01) <- c("1", "0")
snakedata$Outcome01 <- as.numeric(as.character(snakedata$Outcome01))

# observation weights
snakedata$outcome.weights <- 1
snakedata$outcome.weights[Dindex] <- length(Lindex) / length(Dindex)

Yindex <- which(snakedata$React == "Y")
Nindex <- which(snakedata$React == "N")
snakedata$react.weights <- 1
snakedata$react.weights[Yindex] <- length(Nindex) / length(Yindex)

# 1 vs 2 vs 3 or more
snakedata$AVVials <- 
with(snakedata, {
    temp <- AVVials0
    temp[which(AVVials0 == 1)] <- "1"
    temp[which(AVVials0 == 2)] <- "2"
    temp[which(AVVials0 >= 3)] <- "3"
    temp <- as.factor(temp)
    return(temp)
})

# 1 vs 2 or more
snakedata$AVVials1 <- 
with(snakedata, {
    temp <- AVVials0
    temp[which(AVVials0 == 1)] <- "1"
    temp[which(AVVials0 >= 2)] <- "2"
    temp <- as.factor(temp)
    return(temp)
})

# 1 or 2 vs 3 or more
snakedata$AVVials2 <- 
with(snakedata, {
    temp <- AVVials0
    temp[which(AVVials0 <= 2)] <- "1"
    temp[which(AVVials0 >= 3)] <- "2"
    temp <- as.factor(temp)
    return(temp)
})

# ≤0.5 vs >0.5-1.0 vs >1.0
snakedata$AVDose1 <- 
with(snakedata, {
    temp <- AVDose
    temp[which(AVDose <= 0.5)] <- "1"
    temp[which(AVDose > 0.5 & AVDose <= 1)] <- "2"
    temp[which(AVDose > 1)] <- "3"
    temp <- as.factor(temp)
    return(temp)
})

# ≤1.0 vs >1.0
snakedata$AVDose2 <- 
with(snakedata, {
    temp <- AVDose
    temp[which(AVDose <= 1)] <- "1"
    temp[which(AVDose > 1)] <- "2"
    temp <- as.factor(temp)
    return(temp)
})



####################
# Data exploration #
####################

plot(snakedata[, c("AVVials0", "AVDose", "BW")])
with(snakedata, {
    vials.bw <- AVVials0 / BW
    plot(AVDose[Lindex], vials.bw[Lindex], xlab = "AVDose", ylab = "AVVials0/BW")
    points(AVDose[Dindex], vials.bw[Dindex], col = "red", pch = 4, cex = 2)
    legend(2, 0.8, "Outcome: D", col = "red", pch = 4, cex = 2)
})

with(snakedata, {
    vials.bw <- AVVials0 / BW
    plot(AVDose[Nindex], vials.bw[Nindex], xlab = "AVDose", ylab = "AVVials0/BW")
    points(AVDose[Yindex], vials.bw[Yindex], col = "red", pch = 4, cex = 2)
    legend(2, 0.8, "React: Yes", col = "red", pch = 4, cex = 2)
})

table(snakedata$Outcome)
table(snakedata$React)
with(snakedata, table(Outcome, React))
with(snakedata, table(Outcome, AVVials0))


boxplot(AVDose ~ Outcome, data = snakedata)
boxplot(AVDose ~ React, data = snakedata)

with(snakedata, aggregate(AVVials0, list(Outcome), mean, na.rm = TRUE)
with(snakedata, aggregate(AVVials0, list(Outcome), median, na.rm = TRUE)

with(snakedata, aggregate(AVDose, list(Outcome), mean, na.rm = TRUE)
with(snakedata, aggregate(AVDose, list(Outcome), median, na.rm = TRUE)


############
# Modeling #
############

# without any deletion
m0 <- glm(Outcome ~ AVVials0, data = snakedata, family = "binomial")
summary(m0)
# delete 5 live cases
m1 <- glm(Outcome ~ AVVials0, data = snakedata[which(snakedata$AVVials0 <= 6), ], family = "binomial")
summary(m1)
# delete 2 live cases
m2 <- glm(Outcome ~ AVVials0, data = snakedata[which(snakedata$AVVials0 <= 8), ], family = "binomial")
summary(m2)

table(snakedata$AVVials0)
contrasts(snakedata$Outcome)


temp <- glm(Outcome ~ poly(AVVials0, degree = 2), data = snakedata[which(snakedata$AVVials0 <= 6), ], family = "binomial")
summary(temp)

log(snakedata$AVVials0)

myfit.outcome <- glm(Outcome ~ AVDose, data = snakedata, family = "binomial")
myfit.outcome.weight <- glm(Outcome ~ AVDose, data = snakedata, family = "binomial", weights = snakedata$outcome.weights)
summary(myfit.outcome) # not significant
summary(myfit.outcome.weight) # still not significant


myfit.react <- glm(React ~ AVDose, data = snakedata, family = "binomial")
myfit.react.weight <- glm(React ~ AVDose, data = snakedata, family = "binomial", weights = snakedata$react.weights)
summary(myfit.react) # borderline significant
summary(myfit.react.weight) # very significant


# use classification tree of OUTCOME to determine the split for AVDose
tree.outcome <- tree(Outcome ~ AVDose , data = snakedata)
tree.outcome.weight <- tree(Outcome ~ AVDose, data = snakedata, weights = snakedata$outcome.weights)

mypred <- predict(tree.outcome, type = "class")
mylabel <- snakedata$Outcome[! is.na(snakedata$AVDose)]
table(mypred, mylabel) # predicted all live

mypred <- predict(tree.outcome.weight, type = "class")
mylabel <- snakedata$Outcome[! is.na(snakedata$AVDose)]
table(mypred, mylabel) # predicted in a balance way


## the unweighted tree
plot(tree.outcome)
text(tree.outcome)

tree.outcome.pruned <- prune.tree(tree.outcome, best = 3)
plot(tree.outcome.pruned)
text(tree.outcome.pruned)

with(snakedata, c(
    sum(AVDose < 0.35, na.rm = TRUE),
    sum(AVDose >= 0.35 & AVDose < 3, na.rm = TRUE),
    sum(AVDose >= 3, na.rm = TRUE)
))

## the weighted tree
plot(tree.outcome.weight)
text(tree.outcome.weight)

tree.outcome.weight.pruned <- prune.tree(tree.outcome.weight, best = 3)
plot(tree.outcome.weight.pruned)
text(tree.outcome.weight.pruned)

with(snakedata, c(
    sum(AVDose < 2.35, na.rm = TRUE),
    sum(AVDose >= 2.35 & AVDose < 3.55, na.rm = TRUE),
    sum(AVDose >= 3.55, na.rm = TRUE)
))

snakedata$AVDose[Dindex]


# use classification tree of OUTCOME to determine the split for AVDose
tree.react <- tree(React ~ AVDose , data = snakedata)
tree.react.weight <- tree(React ~ AVDose, data = snakedata, weights = snakedata$react.weights)

mypred <- predict(tree.react, type = "class")
mylabel <- snakedata$React[! is.na(snakedata$AVDose)]
table(mypred, mylabel) # predicted all live

mypred <- predict(tree.react.weight, type = "class")
mylabel <- snakedata$React[! is.na(snakedata$AVDose)]
table(mypred, mylabel) # predicted in a balance way


## the unweighted tree
plot(tree.react)
text(tree.react)

tree.react.pruned <- prune.tree(tree.react, best = 3)
plot(tree.react.pruned)
text(tree.react.pruned)

with(snakedata, c(
    sum(AVDose < 0.55, na.rm = TRUE),
    sum(AVDose >= 0.55 & AVDose < 1.05, na.rm = TRUE),
    sum(AVDose >= 1.05, na.rm = TRUE)
))

## the weighted tree
plot(tree.react.weight)
text(tree.react.weight)

tree.react.weight.pruned <- prune.tree(tree.react.weight, best = 3)
plot(tree.react.weight.pruned)
text(tree.react.weight.pruned)

with(snakedata, c(
    sum(AVDose < 2.35, na.rm = TRUE),
    sum(AVDose >= 2.35 & AVDose < 3.55, na.rm = TRUE),
    sum(AVDose >= 3.55, na.rm = TRUE)
))

snakedata$AVDose[Yindex]





##################
# Power analysis #
##################

# Based on one-covaraite logistic regression: log odds ratio = beta0 + beta1 * covariate

# p0: reference death rate of dogs bitten by snakes if there were no treatment at all
# oddsratio: odds ratio of death rate when there is a treatment (AVVial0 or AVDose)
# covariate: a sample for the inquired univariate covariate, empirical dist will be used
# n: sample size; B: number of replications; alpha: test level
power.simulation <- function(p0 = 0.2, oddsratio, covariate, n, B = 2000, alpha = 0.05) {
    covariate <- na.omit(covariate)
    beta0 <- log(p0 / (1 - p0)) # reference log odds ratio
    beta1 <- log(oddsratio) # increment of log odds ratio for every unit increase of covariate
    pvalue.list <- 
    replicate(B, expr = {
        X <- sample(covariate, n, replace = TRUE)
        predictor.linear <- beta0 + beta1 * X
        predictor.prob <- exp(predictor.linear) / (1 + exp(predictor.linear))
        Y <- rbinom(n, 1, predictor.prob)
        mydata <- data.frame(Y = Y, X = X)
        suppressWarnings(
            mymodel <- glm(Y ~ X, data = mydata, family = "binomial")
        )
        zvalue <- summary(mymodel)$coef[2, 3]
        pvalue <- pnorm(zvalue)
        return(pvalue)
    })
    power.est <- mean(pvalue.list <= alpha)
    return(power.est)
}


# small odds ratio means large effect size (alternative farther away from the null)
# (cont.) oddsratio = 1 means no effect at all (under the null)
# (cont.) nonpositive beta1 <-> odds ratio \in [0, 1]

# an example
arglist <- list(
    p0 = 0.2,
    oddsratio = 0.6,
    n = 500,
    B = 1000,
    covariate = snakedata$AVDose,
    alpha = 0.05
)

do.call("power.simulation", arglist)


# power versus odds ratio for fixed n = 218
arglist1 <- list(
    p0 = 0.2,
    oddsratio = 0.1,
    n = nrow(snakedata),
    B = 10000,
    covariate = snakedata$AVDose,
    alpha = 0.05
)

p0.list <- c(0.10, 0.15, 0.20, 0.25)
oddsratio.list <- 1:10 / 10
power.mat <- matrix(0, nrow = length(p0.list), ncol = length(oddsratio.list))
rownames(power.mat) <- p0.list
colnames(power.mat) <- oddsratio.list

for (i in seq_along(p0.list)) {
    arglist1$p0 <- p0.list[i]
    for (j in seq_along(oddsratio.list)) {
        arglist1$oddsratio <- oddsratio.list[j]
        power.mat[i, j] <- do.call("power.simulation", arglist1)
        save(power.mat, file = "project_Galin/power_n218.RData")
    }
}


# power versus sample size for fixed odds ratio = 0.6
arglist2 <- list(
    p0 = 0.2,
    oddsratio = 0.6,
    n = 50,
    B = 10000,
    covariate = snakedata$AVDose,
    alpha = 0.05
)

p0.list <- c(0.10, 0.15, 0.20, 0.25)
n.list <- 1:25 * 20
power.mat <- matrix(0, nrow = length(p0.list), ncol = length(n.list))
rownames(power.mat) <- p0.list
colnames(power.mat) <- n.list

for (i in seq_along(p0.list)) {
    arglist2$p0 <- p0.list[i]
    for (j in seq_along(n.list)) {
        arglist2$n <- n.list[j]
        power.mat[i, j] <- do.call("power.simulation", arglist2)
        save(power.mat, file = "project_Galin/power_oddsratio06.RData")
    }
}

# visualization of power analysis
library(ggplot2)

p0.list <- c(0.10, 0.15, 0.20, 0.25)
oddsratio.list <- 1:10 / 10
n.list <- 1:25 * 20

# loads an RData file, and returns it
loadRData <- function(fileName){
    load(fileName)
    get(ls()[ls() != "fileName"])
}

# power versus effect size for n = 128
temp <- loadRData("project_Galin/power_n218.RData")


power <- c(temp[1, ], temp[2, ], temp[3, ], temp[4, ])
effect <- 1 - as.numeric(names(power))
p0 <- as.factor(paste("p0 = ", sep = "", rep(p0.list, rep(length(oddsratio.list), 4))))
temp.df <- data.frame(power = power, effect = effect, p0 = p0)

ggplot(data = temp.df, aes(x = effect, y = power, col = p0)) +
    geom_line() +
    ggtitle("Power versus effect size for n = 218") +
    xlab("effect size")
    

# power versus sample size for odds ratio = 0.6
temp <- loadRData("project_Galin/power_oddsratio06.RData")

power <- c(temp[1, ], temp[2, ], temp[3, ], temp[4, ])
n <- as.numeric(names(power))
p0 <- as.factor(paste("p0 = ", sep = "", rep(p0.list, rep(length(n.list), 4))))
temp.df <- data.frame(power = power, n = n, p0 = p0)

ggplot(data = temp.df, aes(x = n, y = power, col = p0)) +
    geom_line() +
    ggtitle("Power versus sample size for odds ratio = 0.6") +
    xlab("sample size")



# Miscellaneous
# list2env(arglist, .GlobalEnv)
mean(snakedata$AVDose, na.rm = TRUE)
median(snakedata$AVDose, na.rm = TRUE)

beta0 <- log(p0 / (1 - p0))
deathrate <- mean(snakedata$Outcome == "D", na.rm = TRUE)
