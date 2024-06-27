
###### ###### ###### ###### ###### ######

# Leonardo Bonetti
# leonardo.bonetti@psych.ox.ac.uk
# leonardo.bonetti@clin.au.dk

###### *** INDEPENDENT PSYCHOLOGICAL FEATURES *** ######

###### installing and loading packages ###### 

#install packages
install.packages("lme4")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("haven")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("broom")
install.packages("readxl")
install.packages("jmv")
install.packages("effectsize")
install.packages("Hotelling")
install.packages("rstatix")
install.packages("coin")
if (!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github('jorvlan/raincloudplots')

#Load packages
library(lme4)
library(tidyr)         #data management package from the tidyverse suite.
library(ggplot2)       #plot package from the tidyverse suite.
library(tidyverse)     #for data manipulation and visualization
library(haven)         #
library(ggpubr)        #for creating easily publication ready plots
library(rstatix)       #for easy pipe-friendly statistical analyses
library(broom)         #for printing a summary of statistical tests as data frames
library(readxl)        #for reading excel files
library(jmv)           #for manova analyses
library(effectsize)    #for calculating effect sizes
library(lmerTest)
library(MASS)
library(Hotelling)
library(rstatix)
library(coin)
library(raincloudplots)
library(dplyr)

######

###### COMPARING FOOTBALL PLAYERS AGAINST NORMS (BRAZILIAN AND SWEDISH SAMPLES) ###### 

setwd("/Users/au550322/Documents/AarhusUniversitet/CarlsbergFoundation_Oxford_PaperWork/Research/PredragPetrovic/Alberto_Predrag_Torbjorn/")
#Load data
tsa <- read_excel("Data_Vestberg_et_al_2020_DOI_10_T2.xlsx",col_names = TRUE, sheet = 1)
#tsa2 <- tsa[tsa$DF3scale >= 10, ] #removing outliers (it improves the results)
#tsa <- tsa2
male <- sum(tsa$Gender[1:51] == 1)
female <- 51 - male
malefinal <- male + (174 - 51)

#TMT
result <- t.test(tsa$TMT2scale, mu = 10); print(result)
result <- t.test(tsa$TMT3scale, mu = 10); print(result)
result <- t.test(tsa$TMT4scale, mu = 10); print(result)
#DF
result <- t.test(tsa$DF1scale, mu = 10); print(result)
result <- t.test(tsa$DF2scale, mu = 10); print(result)
result <- t.test(tsa$DF3scale, mu = 10); print(result)
#Stroop
result <- t.test(tsa$CWI1scale, mu = 10); print(result)
result <- t.test(tsa$CWI2scale, mu = 10); print(result)
result <- t.test(tsa$CWI3scale, mu = 10); print(result)
result <- t.test(tsa$CWI4scale, mu = 10); print(result)

### (ONLY SWEDISH OR BRAZILIAN PLAYERS)
## SWEDISH
tsa_Swed <- tsa[tsa$Dataset == 1, ]
#TMT
result <- t.test(tsa_Swed$TMT2scale, mu = 10); print(result)
result <- t.test(tsa_Swed$TMT3scale, mu = 10); print(result)
result <- t.test(tsa_Swed$TMT4scale, mu = 10); print(result)
#DF
result <- t.test(tsa_Swed$DF1scale, mu = 10); print(result)
result <- t.test(tsa_Swed$DF2scale, mu = 10); print(result)
result <- t.test(tsa_Swed$DF3scale, mu = 10); print(result)
#Stroop
result <- t.test(tsa_Swed$CWI1scale, mu = 10); print(result)
result <- t.test(tsa_Swed$CWI2scale, mu = 10); print(result)
result <- t.test(tsa_Swed$CWI3scale, mu = 10); print(result)
result <- t.test(tsa_Swed$CWI4scale, mu = 10); print(result)

## BRAZILIAN
tsa_Braz <- tsa[tsa$Dataset == 2, ]
#TMT
result <- t.test(tsa_Braz$TMT2scale, mu = 10); print(result)
result <- t.test(tsa_Braz$TMT3scale, mu = 10); print(result)
result <- t.test(tsa_Braz$TMT4scale, mu = 10); print(result)
#DF
result <- t.test(tsa_Braz$DF1scale, mu = 10); print(result)
result <- t.test(tsa_Braz$DF2scale, mu = 10); print(result)
result <- t.test(tsa_Braz$DF3scale, mu = 10); print(result)
#Stroop
result <- t.test(tsa_Braz$CWI1scale, mu = 10); print(result)
result <- t.test(tsa_Braz$CWI2scale, mu = 10); print(result)
result <- t.test(tsa_Braz$CWI3scale, mu = 10); print(result)
result <- t.test(tsa_Braz$CWI4scale, mu = 10); print(result)



###### MANCOVAs COMPARING FOOTBALL PLAYERS AND CONTROLS (BRAZILIAN SAMPLE)

setwd("/Users/au550322/Documents/AarhusUniversitet/CarlsbergFoundation_Oxford_PaperWork/Research/PredragPetrovic/Alberto_Predrag_Torbjorn/")
#Load data
tsa <- read_excel("Base_Futebol_2021.xlsx",col_names = TRUE, sheet = 1)

#computing mean age, football players
age_mean_football <- mean(tsa$Age[1:153])
age_sd_football <- sd(tsa$Age[1:153])
#computing mean age, controls
age_mean_controls <- mean(tsa$Age[153:277])
age_sd_controls <- sd(tsa$Age[153:277])
#computing mean years of education, football players
edu_mean_football <- mean(tsa$`School (in years)`[1:153])
edu_sd_football <- sd(tsa$`School (in years)`[1:153])
#computing mean years of education, controls
edu_mean_controls <- mean(tsa$`School (in years)`[153:277])
edu_sd_controls <- sd(tsa$`School (in years)`[153:277])

# PERSONALITY MEASURES
dependent <- cbind(tsa$Neuroticism, tsa$Extroversion, tsa$Openness, tsa$Agreeableness, tsa$Conscientiousness)
age <- tsa$Age
mancova_model <- manova(dependent ~ age + tsa$Group) #manova + mancova
summary(mancova_model, test = "Wilks")
summary.aov(mancova_model) #univariate tests
eta_squared(mancova_model, partial = TRUE) #effect size
#effect size of the single independent anovas
aov2 <- aov(tsa$Neuroticism ~ age + tsa$Group)
summary(aov2)
eta_squared(aov2) #effect size anova
aov2 <- aov(tsa$Extroversion ~ age + tsa$Group)
summary(aov2)
eta_squared(aov2) #effect size anova
aov2 <- aov(tsa$Openness ~ age + tsa$Group)
summary(aov2)
eta_squared(aov2) #effect size anova
aov2 <- aov(tsa$Agreeableness ~ age + tsa$Group)
summary(aov2)
eta_squared(aov2) #effect size anova
aov2 <- aov(tsa$Conscientiousness ~ age + tsa$Group)
summary(aov2)
eta_squared(aov2) #effect size anova
# Check multivariate normality assumption
qqnorm(residuals(mancova_model))
qqline(residuals(mancova_model))
#Summary statistics
football <- tsa[tsa$Group==1,]
control <- tsa[tsa$Group==2,]
summary(football$Neuroticism); sd(football$Neuroticism)
summary(control$Neuroticism); sd(control$Neuroticism)
summary(football$Extroversion); sd(football$Extroversion)
summary(control$Extroversion); sd(control$Extroversion)
summary(football$Openness); sd(football$Openness)
summary(control$Openness); sd(control$Openness)
summary(football$Agreeableness); sd(football$Agreeableness)
summary(control$Agreeableness); sd(control$Agreeableness)
summary(football$Conscientiousness); sd(football$Conscientiousness)
summary(control$Conscientiousness); sd(control$Conscientiousness)

# 5-POINTS NEW/REPETITION
# PARAMETRIC MANCOVA
#dependent <- cbind(tsa$`5-points new`,tsa$`5-points repetition`)
dependent <- cbind(tsa$`5-points new`,tsa$`5-points repetition`,tsa$`Simple Manual Reaction Time`)
age <- tsa$Age
mancova_model <- manova(dependent ~ age + tsa$Group) #manova + mancova
summary(mancova_model, test = "Wilks")
summary.aov(mancova_model) #univariate tests
eta_squared(mancova_model, partial = TRUE) #effect size
#effect size of the single independent anovas
aov2 <- aov(tsa$`5-points new` ~ age + tsa$Group)
summary(aov2)
eta_squared(aov2) #effect size anova
#effect size of the single independent anovas
aov2 <- aov(tsa$`5-points repetition` ~ age + tsa$Group)
summary(aov2)
eta_squared(aov2) #effect size anova
#effect size of the single independent anovas
aov2 <- aov(tsa$`Simple Manual Reaction Time` ~ age + tsa$Group)
summary(aov2)
eta_squared(aov2) #effect size anova
# Check multivariate normality assumption
qqnorm(residuals(mancova_model))
qqline(residuals(mancova_model))
#Summary statistics
football <- tsa[tsa$Group==1,]
control <- tsa[tsa$Group==2,]
summary(football$`5-points new`); sd(football$`5-points new`)
summary(control$`5-points new`); sd(control$`5-points new`)
summary(football$`5-points repetition`)
summary(control$`5-points repetition`)
summary(football$`Simple Manual Reaction Time`); sd(football$`Simple Manual Reaction Time`)
summary(control$`Simple Manual Reaction Time`); sd(control$`Simple Manual Reaction Time`)
# NON-PARAMETRIC TESTS ALTERNATIVE
# Perform Mann-Whitney U test
wilcox_result <- wilcox.test(tsa$`5-points new` ~ tsa$Group); print(wilcox_result)
# Calculate CLES
cles <- wilcox_effsize(data = tsa, formula = `5-points new` ~ Group); print(cles)
# Perform Mann-Whitney U test
wilcox_result <- wilcox.test(tsa$`5-points repetition` ~ tsa$Group); print(wilcox_result)
# Calculate CLES
cles <- wilcox_effsize(data = tsa, formula =  `5-points repetition` ~ Group); print(cles)
# Perform Mann-Whitney U test
wilcox_result <- wilcox.test(tsa$`Simple Manual Reaction Time` ~ tsa$Group); print(wilcox_result)
# Calculate CLES
cles <- wilcox_effsize(data = tsa, formula = `Simple Manual Reaction Time` ~ Group); print(cles)

# WORKING MEMORY
# PARAMETRIC MANCOVA
dependent <- cbind(tsa$`Forward Digit Span`,tsa$`Backward Digit Span`)
age <- tsa$Age
mancova_model <- manova(dependent ~ age + tsa$Group) #manova + mancova
summary(mancova_model, test = "Wilks")
summary.aov(mancova_model) #univariate tests
eta_squared(mancova_model, partial = TRUE) #effect size
#effect size of the single independent anovas
aov2 <- aov(tsa$`Forward Digit Span` ~ age + tsa$Group)
summary(aov2)
eta_squared(aov2) #effect size anova
#effect size of the single independent anovas
aov2 <- aov(tsa$`Backward Digit Span` ~ age + tsa$Group)
summary(aov2)
eta_squared(aov2) #effect size anova
# Check multivariate normality assumption
qqnorm(residuals(mancova_model))
qqline(residuals(mancova_model))
#Summary statistics
football <- tsa[tsa$Group==1,]
control <- tsa[tsa$Group==2,]
summary(football$`Forward Digit Span`); sd(football$`Forward Digit Span`)
summary(control$`Forward Digit Span`); sd(control$`Forward Digit Span`)
summary(football$`Backward Digit Span`); sd(football$`Backward Digit Span`)
summary(control$`Backward Digit Span`); sd(control$`Backward Digit Span`)
#summary of age
summary(football$Age)
summary(control$Age)
# NON-PARAMETRIC TESTS ALTERNATIVE
# Perform Mann-Whitney U test
wilcox_result <- wilcox.test(tsa$`Forward Digit Span` ~ tsa$Group); print(wilcox_result)
# Calculate CLES
cles <- wilcox_effsize(data = tsa, formula = `Forward Digit Span` ~ Group); print(cles)
# Perform Mann-Whitney U test
wilcox_result <- wilcox.test(tsa$`Backward Digit Span` ~ tsa$Group); print(wilcox_result)
# Calculate CLES
cles <- wilcox_effsize(data = tsa, formula = `Backward Digit Span` ~ Group); print(cles)

# TOWERS OF HANOI
dependent <- cbind(tsa$`Tower of Hanoi (seconds)`,tsa$`Tower of Hanoi (moves)`,tsa$`Stroop Test 30"`)
age <- tsa$Age
mancova_model <- manova(dependent ~ age + tsa$Group) #manova + mancova
summary(mancova_model, test = "Wilks")
summary.aov(mancova_model) #univariate tests
eta_squared(mancova_model, partial = TRUE) #effect size
#effect size of the single independent anovas
aov2 <- aov(tsa$`Tower of Hanoi (seconds)` ~ age + tsa$Group)
summary(aov2)
eta_squared(aov2) #effect size anova
#effect size of the single independent anovas
aov2 <- aov(tsa$`Tower of Hanoi (moves)` ~ age + tsa$Group)
summary(aov2)
eta_squared(aov2) #effect size anova
# Check multivariate normality assumption
qqnorm(residuals(mancova_model))
qqline(residuals(mancova_model))
#Summary statistics
football <- tsa[tsa$Group==1,]
control <- tsa[tsa$Group==2,]
summary(football$`Tower of Hanoi (moves)`); sd(football$`Tower of Hanoi (moves)`)
summary(control$`Tower of Hanoi (moves)`); sd(control$`Tower of Hanoi (moves)`)
summary(football$`Stroop Test 30"`)
summary(control$`Stroop Test 30"`)
# NON-PARAMETRIC TESTS ALTERNATIVE
# Perform Mann-Whitney U test
wilcox_result <- wilcox.test(tsa$`Tower of Hanoi (seconds)` ~ tsa$Group); print(wilcox_result)
# Calculate CLES
cles <- wilcox_effsize(data = tsa, formula = `Tower of Hanoi (seconds)` ~ Group); print(cles)
# Perform Mann-Whitney U test
wilcox_result <- wilcox.test(tsa$`Tower of Hanoi (moves)` ~ tsa$Group); print(wilcox_result)
# Calculate CLES
cles <- wilcox_effsize(data = tsa, formula = `Tower of Hanoi (moves)` ~ Group); print(cles)

######

###### PREDICTING FOOTBALL VARIABLES BASED ON PERSONALITY AND COGNITIVE SCORES (BRAZILIAN SAMPLE)

setwd("/Users/au550322/Documents/AarhusUniversitet/CarlsbergFoundation_Oxford_PaperWork/Research/PredragPetrovic/Alberto_Predrag_Torbjorn/")
#Load data
tsa <- read_excel("Base_Futebol_2021.xlsx",col_names = TRUE, sheet = 1)
tsa2 = tsa[tsa$Participant <= 153, ] #only football players

#goals
model <- lm(tsa2$Goals ~ tsa2$Age + tsa2$Neuroticism + tsa2$Extroversion + tsa2$Openness + tsa2$Agreeableness + tsa2$Conscientiousness + tsa2$`5-points new` + tsa2$`Simple Manual Reaction Time` + tsa2$`WM comb standardized` + tsa2$`Tower of Hanoi (moves)`, data = tsa2); summary(model); qqnorm(residuals(model)); qqline(residuals(model))

#avg shots per game
model <- lm(tsa2$`Avg Shots Per Game` ~ tsa2$Age + tsa2$Neuroticism + tsa2$Extroversion + tsa2$Openness + tsa2$Agreeableness + tsa2$Conscientiousness + tsa2$`5-points new` + tsa2$`Simple Manual Reaction Time` + tsa2$`WM comb standardized` + tsa2$`Tower of Hanoi (moves)`, data = tsa2); summary(model); qqnorm(residuals(model)); qqline(residuals(model))

#assists
model <- lm(tsa2$Assists ~ tsa2$Age + tsa2$Neuroticism + tsa2$Extroversion + tsa2$Openness + tsa2$Agreeableness + tsa2$Conscientiousness + tsa2$`5-points new` + tsa2$`Simple Manual Reaction Time` + tsa2$`WM comb standardized` + tsa2$`Tower of Hanoi (moves)`, data = tsa2); summary(model); qqnorm(residuals(model)); qqline(residuals(model))

#avg attempted dribbles
model <- lm(tsa2$`Avg attempted dribbles` ~ tsa2$Age + tsa2$Neuroticism + tsa2$Extroversion + tsa2$Openness + tsa2$Agreeableness + tsa2$Conscientiousness + tsa2$`5-points new` + tsa2$`Simple Manual Reaction Time` + tsa2$`WM comb standardized` + tsa2$`Tower of Hanoi (moves)`, data = tsa2); summary(model); qqnorm(residuals(model)); qqline(residuals(model))

#avg successful dribbles
model <- lm(tsa2$`Avg successful dribbles` ~ tsa2$Age + tsa2$Neuroticism + tsa2$Extroversion + tsa2$Openness + tsa2$Agreeableness + tsa2$Conscientiousness + tsa2$`5-points new` + tsa2$`Simple Manual Reaction Time` + tsa2$`WM comb standardized` + tsa2$`Tower of Hanoi (moves)`, data = tsa2); summary(model); qqnorm(residuals(model)); qqline(residuals(model))

# Summary of the regression model
summary(model)
# Coefficients of the predictors
coef(model)
# Predicted values
fitted(model)
# Residuals
residuals(model)
# Diagnostic plots
plot(model)

###### ###### ######
