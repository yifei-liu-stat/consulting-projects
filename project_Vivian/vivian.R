library(nlme)
require(lattice)
library(latticeExtra)
library(dplyr)

## read data
## !! shift time; add a new covariate.
setwd("/home/liu00980/Documents/8801project/project_Vivian")
data_1<-read.csv('Data.csv')
table(data_1$Time)

## data cleaning (some check)
length(unique(data_1$ID))
length(unique(data_1$Time))
data_1$Time[data_1$ID==138]

for(i in unique(data_1$ID)){
a=length(data_1$Time[data_1$ID==i])
b=length(unique(data_1$Time[data_1$ID==i]))
if (a!=b) print('1')
}

#Q:
## ----- DATA CLEANING -----
## add a four level factor for variable Time as Time.pand
data_1 <- data_1[data_1$Time != 100, ]

Time.pand=rep(0,nrow(data_1))
for(i in 1:length(Time.pand)){
  if(data_1$Time[i]==100) Time.pand[i]='A'
  else if(data_1$Time[i]>=101 & data_1$Time[i]<=106) Time.pand[i]='B'
  else if(data_1$Time[i]>=200 & data_1$Time[i]<=299) Time.pand[i]='C'
  else if(data_1$Time[i]>=300)       Time.pand[i]='D' 
}
Time.pand=relevel(as.factor(Time.pand), ref = 'B')
data_1$Time.pand = Time.pand


data_1 %>%
  group_by(ID) %>%
  mutate(Time.start = min(as.character(Time.pand))) %>%
  as.data.frame() -> data_1
data_1$Time.start <- relevel(as.factor(data_1$Time.start), ref = "B")


# data_1 %>%
#   arrange(ID) %>%
#   select(ID, Time.start) %>%
#   group_by(ID) %>%
#   summarize(unique(Time.start))
  

## Shift Time (add a new variable, Time.shift)
Time.shift=rep(0,nrow(data_1))
for(i in unique(data_1$ID)){
  Time.shift[which(data_1$ID==i)]=as.numeric(as.factor(data_1$Time[which(data_1$ID==i)]))
}
data_1=data.frame(data_1,'Time.shift'=Time.shift)


# # IDs with baselines and have interventions
# id.baseline <- as.numeric(names(which(table(data_1[data_1$ID %in% data_1[data_1$Time == 100, "ID"], "ID"]) > 1)))

# data_1 %>%
#   filter(ID %in% id.baseline) %>%
#   select(ID, Time) %>%
#   arrange(ID, Time)


# intersect(
#   unique(data_1[data_1$Condition == 0, "ID"]),
#   unique(data_1[data_1$Condition != 0, "ID"])
# )

#############
# PANAS_Neg #
#############

data_neg <- data_1[, c("ID", "PANAS_Neg", "Time.pand", "Time.shift", "Time.start")]
data_neg <- na.omit(data_neg)

data_neg_group <- groupedData(PANAS_Neg ~ Time.shift | ID, data = data_neg)

## Q1: change over time
lme_neg_linear <- lme(fixed = PANAS_Neg ~ Time.shift + Time.pand + Time.start, random = ~ 1 | ID, data = data_neg_group)

summary(lme_neg_linear) # individual significant: time; borderline significant: time.start.pand compared to time.start.prepand
anova(lme_neg_linear) # overall significant: time

a <- xyplot(
  fitted(lme_neg_linear) ~ Time.shift | ID,
  data = data_neg_group,
  cex = 0.1, col = 2, lwd = 1,
  xlab = "Number of treated weeks",
  ylab = "Negative affect score"
  )
b <- xyplot(PANAS_Neg ~ Time.shift | ID, data = data_neg_group, add = T, cex = 0.2)
a + as.layer(b)


## Q2: recommended weeks

data_neg_group_quad <- data_neg_group[data_neg_group$ID %in% c(105, 121, 125, 129, 138), ]

lme_neg_quad <- lme(fixed = PANAS_Neg ~ Time.shift + I(Time.shift^2) + Time.start, random = list(~ Time.shift + I(Time.shift^2) | ID), data = data_neg_group_quad, method = "ML")
lme_neg_quad_0 <- lme(fixed = PANAS_Neg ~ Time.shift + I(Time.shift^2) + Time.start, random = list(~ 1 | ID), data = data_neg_group_quad, method = "ML")

anova(lme_neg_quad, lme_neg_quad_0)


# only time is significant
summary(lme_neg_quad)
anova(lme_neg_quad)

apply(coef(lme_neg_quad), 1, function(x) - x[2] / (2 * x[3])) 

a <- xyplot(
  fitted(lme_neg_quad) ~ Time.shift | ID,
  data = data_neg_group_quad,
  cex = 0.1, col = 2, lwd = 1,
  xlab = "Number of treated weeks",
  ylab = "Negative affect score"
  )
b <- xyplot(PANAS_Neg ~ Time.shift | ID, data = data_neg_group_quad, add = T, cex = 0.2)
a + as.layer(b)


## Q3: differential effect of pandemic

### same model as Q1
lme_neg_linear <- lme(fixed = PANAS_Neg ~ Time.shift + Time.start, random = ~ 1 | ID, data = data_neg_group)

summary(lme_neg_linear) # individual significant: time; borderline significant: time.start.pand compared to time.start.prepand
anova(lme_neg_linear) # overall significant: time

a <- xyplot(
  fitted(lme_neg_linear) ~ Time.shift | ID,
  data = data_neg_group,
  cex = 0.1, col = 2, lwd = 1,
  xlab = "Number of treated weeks",
  ylab = "Negative affect score"
  )
b <- xyplot(PANAS_Neg ~ Time.shift | ID, data = data_neg_group, add = T, cex = 0.2)
a + as.layer(b)


### factor only
lme_neg_factor <- lme(fixed = PANAS_Neg ~ Time.start, random = ~ 1| ID, data = data_neg_group)

summary(lme_neg_factor) # Time.pand borderline significant
anova(lme_neg_factor) # overall not significant

a <- xyplot(
  fitted(lme_neg_factor) ~ Time.pand | ID,
  data = data_neg_group,
  cex = 0.1, col = 2, lwd = 1,
  xlab = "Number of treated weeks",
  ylab = "Negative affect score"
  )
b <- xyplot(PANAS_Neg ~ Time.pand | ID, data = data_neg_group, add = T, cex = 0.2)
a + as.layer(b)


## Q4: most/least improvement

lme_neg_linear_rs <- lme(fixed = PANAS_Neg ~  Time.shift + Time.pand + Time.start, random = ~ Time.shift | ID, data = data_neg_group)
lme_neg_linear <- lme(fixed = PANAS_Neg ~  Time.shift + Time.pand + Time.start, random = ~ 1 | ID, data = data_neg_group)


# the random effect of time is not significant
anova(lme_neg_linear_rs, lme_neg_linear)


summary(lme_neg_linear_rs) # individual significant: time
anova(lme_neg_linear_rs) # overall significant: time (fixed effect)

a <- xyplot(
  fitted(lme_neg_linear_rs) ~ Time.shift | ID,
  data = data_neg_group,
  cex = 0.1, col = 2, lwd = 1,
  xlab = "Number of treated weeks",
  ylab = "Negative affect score"
  )
b <- xyplot(PANAS_Neg ~ Time.shift | ID, data = data_neg_group, add = T, cex = 0.2)
a + as.layer(b)

slope<- coef(lme_neg_linear_rs)[, 2]
index.temp <- rownames(coef(lme_neg_linear_rs))
effect.temp <- data_neg %>%
  filter(Time.shift == 1 ) %>%
  select(c(ID, PANAS_Neg))

effect.negative <- effect.temp$PANAS_Neg
names(effect.negative) <- effect.temp$ID
effect.negative <- effect.negative[index.temp]


summary(lm(slope ~ effect.negative))

plot(
  effect.negative,
  slope,
  pch = 16,
  xlab = "Initial negative affect",
  ylab = "Individual slope for Time.shift"
)