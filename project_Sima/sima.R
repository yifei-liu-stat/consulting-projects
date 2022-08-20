library(PerformanceAnalytics)
library(corrplot)
library(Hmisc)




mydata <- read.csv("project_Sima/WINCS_2.8.2022-v1.csv", header = TRUE, check.names = FALSE)

#####################
# shorten the names #
#####################

colnames_list_full <- names(mydata)
names_dict <- read.csv("project_Sima/names_match.csv")

ori_names <- data.frame(full_name = colnames_list_full)
temp <- merge(ori_names, names_dict[, 1:2], by.x = "full_name", all.x = TRUE, sort = FALSE)
# # some names are a little bit different in the dict csv file
# temp[is.na(temp[, 2]), 1]
# colnames_list_full %in% names_dict$full_name
temp[is.na(temp[, 2]), 2] <- c("source", "track", "guest", "raceethnic", "marital", "incentiveleader", "levelguest", "fund", "levelaward")
colnames_list_short <- merge(ori_names, temp, by.x = "full_name", sort = FALSE)[, 2]

# save the names and the matching
names_list <- list(names_full = colnames_list_full, names_short = colnames_list_short)
save(names_list, file = "project_Sima/names_list.RData")

#######################
# dataset from Yuyuan #
#######################
load(file = "project_Sima/temp.RData")
tempdata <- sima_new
names(tempdata)

# "record_id" "institution"
for (i in 1:2)
    tempdata[, i] <- as.factor(as.character(tempdata[, i]))
# "genderidentity" "raceethnic" "marital" "track" "trackmisc"
for (i in 3:7)
    tempdata[, i] <- as.factor(tempdata[, i])

summary(tempdata[, 1:7])
sapply(1:7, FUN = function(x) {
    length(unique(na.omit(tempdata[, x])))
})


# "leadership" "leadernumber entry" "incentiveleader"
tempdata[, 8] <- as.factor(tempdata[, 8])
tempdata[tempdata[, 8] == "No", 9] <- 0

# "incentiveleader" as a binary variable
tempdata[tempdata[, 10] == "", 10] <- "No"
prefix <- c("Huh??", "No", "no", "never", "Never", "0", "na", "N/A", "N/a", ",,", "I don't think it was ever incentivized")
temp <- 
sapply(tempdata[, 10], FUN = function(s) {
    any(startsWith(s, prefix))
}) # index of no or yest
names(temp) <- NULL
tempdata[temp, 10] <- "No"
tempdata[!temp, 10] <- "Yes"
tempdata[, 10] <- as.factor(tempdata[, 10])

summary(tempdata[, 8:10])

# "pub" "author"
# the first one is okay; for the second one, 84 na (82 persons with 0 pub and 2 persons with 3 pub)
tempdata[is.na(tempdata[, 12]), 12] <- 0
summary(tempdata[, 11:12])

# "fund" "source" "sourceetc" "amount"
tempdata[is.na(tempdata[, 13]), 13] <- 0
tempdata[, 14] <- paste(tempdata[, 14], tempdata[, 15], sep = ",") # combine source and scourceetc
names(tempdata)[14] <- "source_combined"
## DELETION TBD: "sourceetc" "amount" are "useless" features after processing

summary(tempdata[, 13:14])

# "award" "levelaward" "levelawardetc"
tempdata[, 18] <- paste(tempdata[, 18], tempdata[, 19], sep = ",") # combine
names(tempdata)[18] <- "levelaward_combined"
## DELETION TBD: "levelawardetc" is "useless" after processing

# ## DELETION DONE: these respondants didn't have any responses afterwards
# tempdata <- tempdata[- which(is.na(tempdata[, 17])), ] # 167 363 378 405 (+1 row index in v1) 168 364 379 406

summary(tempdata[17:19])

# "guest" "levelguest" "levelguestetc"
tempdata[, 21] <- paste(tempdata[, 21], tempdata[, 22], sep = ",") # combine
names(tempdata)[21] <- "levelguest_combined"
## DELETION TBD: "levelguestetc" is "useless" after processing

# ## DELETION DONE: these respondants didn't have any responses afterwards
# tempdata <- tempdata[- which(is.na(tempdata[, 20])), ] # should be able to be dealt with by "complete" column
summary(tempdata[, 20:22])

data_v2 <- tempdata

# # DELETION OF INCOMPETED SURVEYS: not sure what "Complete?" column means.
# alldata <- read.csv("project_Sima/WINCS_2.8.2022 stats class dataset 447.csv")
# ids_complete <- alldata[alldata[, 131] == "Complete", 1]

# ids_v1 <- tempdata$record_id
# ids_v1_complete <- 
# sapply(ids_v1, FUN = function(id) {
#     id %in% ids_complete
# })
# data_v2 <- tempdata[ids_v1_complete, ]

save(data_v2, file = "project_Sima/data_v2.RData")


#######################
# pairwise comparison #
#######################

###########################
## further data cleaning ##
###########################

tempdata2 <- tempdata

# FWC to numerical (6 variables in total)

for (i in 23:28) {
    levels(tempdata2[, i]) <- 0:7
    tempdata2[, i] <- as.numeric(as.character(tempdata2[, i]))
    tempdata2[tempdata2[, i] == 0, i] <- NA
}

fwc <- paste("famworkconflict", 1:6, sep = "")

# variables about academic productivity
# "amount" has strange input
# multiple optional questions with etc are hard to dealt with

academic <- c("leadernumber entry", "incentiveleader", "pub", "author", "fund", "award", "guest")

# variables about burnout
sum(tempdata2$burnoutopt != "Yes") - sum(tempdata2$burnout == "missing") # 0, match!

tempdata2$workinterf <- relevel(factor(tempdata2$workinterf, ordered = FALSE), ref = "Never")
tempdata2$burnout <- relevel(factor(tempdata2$burnout, ordered = FALSE), ref = "1 Not at all burned out")

burnout <- c("workinterf", "burnout", paste("burn", 1:9, sep = ""))


# gender variable
temp <- as.character(tempdata2$gender)
temp[!(temp %in% c("Man", "Woman"))] <- "Others"
tempdata2$genderidentity <- as.factor(temp)

# some rows with strange input
tempdata2$record_id[which(tempdata2$pub == 300)] # row: 335; id: 346
tempdata2$record_id[which((tempdata$fund != 0) & (tempdata2$source_combined == ","))] # row: 378; id: 389
tempdata2$record_id[which(tempdata2$award == 2130000)] # row: 381; id: 392
which(tempdata2$record_id == 246) # row: 235
weird_id_list <- c(235, 335, 378, 381)

# final dataset after cleaning
tempdata2 <- tempdata2[-weird_id_list, ]
dim(tempdata2)

save(tempdata2, file = "project_Sima/data_v3.RData")


# final dataset for comparison and data exploration
tempdata3 <- tempdata2[, c("record_id", "genderidentity", fwc, academic, burnout)]
dim(tempdata3)

save(tempdata3, file = "project_Sima/data_exploration.RData")

# a test run 
summary(lm(famworkconflict1 ~ burnout, data = tempdata3))


######################
## data exploration ##
######################

tempdata4 <- tempdata3
names(tempdata4)[c(1, 2:10)] <- c("id", "gender", paste("fwc", 1:6, sep = ""), "leader", "incentive")
# convert all ordered factor to numerical vector

num_cov <- c(paste("fwc", 1:5, sep = ""), "leader", "pub", "author", "fund", "award", "guest")
num_cov_burnout <- c(paste("fwc", 1:5, sep = ""), burnout)
idx_woman <- which(tempdata4$gender == "Woman")
idx_man <- which(tempdata4$gender == "Man")

# individual fwc covariates against continuous academic features
cor_mat <- rcorr(as.matrix(tempdata4[, num_cov]), type = "spearman")
png("project_Sima/temp.png")
corrplot(cor_mat$r, method = "color", type = "upper", p.mat = cor_mat$P, sig.level = 0.1, tl.pos = "td")
dev.off()

# sum over the first five fwc covariates
temp <- tempdata4
temp$fwc <- tempdata4$fwc1 + tempdata4$fwc2 + tempdata4$fwc3 + tempdata4$fwc4 + tempdata4$fwc5 
num_cov_sum <- c("fwc", "leader", "pub", "author", "fund", "award", "guest")
cor_mat <- rcorr(as.matrix(temp[, num_cov_sum]), type = "spearman")
png("project_Sima/temp.png")
corrplot(cor_mat$r, method = "color", type = "upper", p.mat = cor_mat$P, sig.level = 0.1)
dev.off()

# same, but for woman and man, respectively
cor_mat_woman <- rcorr(as.matrix(tempdata4[idx_woman, num_cov]), type = "spearman")
cor_mat_man <- rcorr(as.matrix(tempdata4[idx_man, num_cov]), type = "spearman")
png("project_Sima/temp.png", width = 800, height = 400)
par(mfrow = c(1, 2))
corrplot(cor_mat_woman$r, method = "color", type = "upper", tl.pos = "td", p.mat = cor_mat_woman$P, sig.level = 0.1)
title("Woman", line = -21)
corrplot(cor_mat_man$r, method = "color", type = "upper", tl.pos = "td", p.mat = cor_mat_man$P, sig.level = 0.1)
title("Man", line = -21)
par(mfrow = c(1, 1))
dev.off()

# TBD: the binary incentive feature


# individual fwc covariates against burnout features
## first convert them to numbers

levels(tempdata4$workinterf) <- c(1, 0, 2, 3, 4, 5)
tempdata4$workinterf <- as.numeric(as.character(tempdata4$workinterf))
tempdata4$workinterf[which(tempdata4$workinterf == 0)] <- NA

levels(tempdata4$burnout) <- c(1, NA, 2, 3, 4, 5)
tempdata4$burnout <- as.numeric(as.character(tempdata4$burnout))


burnout_scale <- function(item) {
    if (item == "Every day") return(7)
    if (item == "A few times per week") return(6)
    if (item == "Once per week") return(5)
    if (item == "A few times per month") return(4)
    if (item == "Once per month or less") return(3)
    if (item == "A few times per year") return(2)
    if (item == "Never") return(1)
    if (item == "missing") return(NA)
}
burnout_scale <- Vectorize(burnout_scale, SIMPLIFY = TRUE, USE.NAMES = FALSE)

for (i in 18:26) {
    levels(tempdata4[, i]) <- burnout_scale(levels(tempdata4[, i]))
    tempdata4[, i] <- as.numeric(as.character(tempdata4[, i]))
}


# individual fwc covariates against continuous academic features
cor_mat <- rcorr(as.matrix(tempdata4[, num_cov_burnout]), type = "spearman")
png("project_Sima/temp.png")
corrplot(cor_mat$r, method = "color", type = "upper", p.mat = cor_mat$P, sig.level = 0.1, tl.pos = "td")
dev.off()

# sum over the first five fwc covariates
temp <- tempdata4
temp$fwc <- tempdata4$fwc1 + tempdata4$fwc2 + tempdata4$fwc3 + tempdata4$fwc4 + tempdata4$fwc5 
num_cov_burnout_sum <- c("fwc", burnout)
cor_mat <- rcorr(as.matrix(temp[, num_cov_burnout_sum]), type = "spearman")
png("project_Sima/temp.png")
corrplot(cor_mat$r, method = "color", type = "upper", p.mat = cor_mat$P, sig.level = 0.1)
dev.off()

# same, but for woman and man, respectively
cor_mat_woman <- rcorr(as.matrix(tempdata4[idx_woman, num_cov_burnout]), type = "spearman")
cor_mat_man <- rcorr(as.matrix(tempdata4[idx_man, num_cov_burnout]), type = "spearman")
png("project_Sima/temp.png", width = 800, height = 400)
par(mfrow = c(1, 2))
corrplot(cor_mat_woman$r, method = "color", type = "upper", tl.pos = "td", p.mat = cor_mat_woman$P, sig.level = 0.1)
title("Woman", line = -21)
corrplot(cor_mat_man$r, method = "color", type = "upper", tl.pos = "td", p.mat = cor_mat_man$P, sig.level = 0.1)
title("Man", line = -21)
par(mfrow = c(1, 1))
dev.off()




names(tempdata4)
tempdata4$fwc <- tempdata4$fwc1 + tempdata4$fwc2 + tempdata4$fwc3 + tempdata4$fwc4 + tempdata4$fwc5

myformular <- fwc ~ gender * (leader + incentive + pub + author + fund + award + guest)
myfit <- lm(myformular, data = tempdata4)
summary(myfit)


academic

summary(lm(fwc ~ award * gender, data = tempdata4))








