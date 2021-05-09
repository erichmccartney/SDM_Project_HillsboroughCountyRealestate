#' SDM Project: Hillsborough County Real Estate

library(readxl)
library(lattice)
library(survival)
library(stargazer)
library(dplyr)
library(ggplot2)
library(lme4)
library(PerformanceAnalytics)
setwd("~/GitHub/R/DataSets")
df <- read.csv("HillsboroughCountyData.csv")
df$PropertyType= as.factor(df$PropertyType)
df$SiteCity= as.factor(df$SiteCity)
df$Homestead= as.factor(df$Homestead)
df$Neighborhood= as.factor(df$Neighborhood)
df$LastSaleDate= as.factor(df$LastSaleDate)
  

#' Data visualizations (Log)

hist(df$YearsSinceTurnover)
hist(log(df$YearsSinceTurnover))       # Misleading histogram: has different varieties


#' OLS model (pooled)

ols <- lm(YearsSinceTurnover ~ PropertyType*Neighborhood, data=df)
summary(ols)

#' Questions: What inference do you make from the OLS analysis?

#' Fixed effects model (controls for BETWEEN block differences)

fe <- lm(yield ~ nitrogen*variety + block, data=d)
summary(fe)
confint(fe)

#' Random effects model (controls for WITHIN block differences) using lme4 package (LMM estimation)
#' LMM = linear mixed model: an extension of linear models where the linear predictor 
#' contains random effects in addition to the usual fixed effects.
#' LME = linear mixed effects; mixed effects models contain both fixed and random effects.
#' Note: In lme4, random effects are specified within parentheses; fixed effects outside parenthesis.
#' Note: If REML (Restricted Maximum Likelihood) is set to FALSE, we use standard MLE.

library(lme4) 
re <- lmer(yield ~ nitrogen*variety + (1 | block), data=d, REML=FALSE)
summary(re)
confint(re)
AIC(re)
fixef(re)                                       # Magnitude of fixed effects
ranef(re)                                       # Magnitude of random effects
coef(re)                                        # Magnitude of total effects

library(stargazer)
stargazer(ols, fe, re, type="text", single.row=TRUE)
AIC(ols, fe, re)