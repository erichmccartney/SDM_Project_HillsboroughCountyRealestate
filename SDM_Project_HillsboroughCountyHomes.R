#' SDM Project: Hillsborough County Real Estate

library(readxl)
setwd("~/GitHub/R/DataSets")
df <- read.csv("HillsboroughCountyData.csv")
str(df)
view(df)


# ------------
  
  
  
  
#' Feature engineering/Data Preprocessing
#' (this is SUPER-IMPORTANT, but something that most of you will never do)

df$baths <- df$bathsfull + 0.5*df$bathshalf
df$specialsale <- ifelse(df$splsale=='None', 0, 1)
df$tileroof <- ifelse(df$Roof=='Tile' | df$Roof=='Slate' | df$Roof=='Slate, Tile' |
                        df$Roof=='Concrete, Tile', 1, 0)
df$privatepool <- ifelse(df$Pool=='Private' | df$Pool=='Private, Community', 1, 0)
df$communitypool <- ifelse(df$Pool=='Community' | df$Pool=='Private, Community', 1, 0)
df$year <- as.numeric(format(df$PendingDate,'%Y'))
View(df)

which(! complete.cases(df)) 
colSums(is.na(df))
# We have 311 NA values in spa; hence, it's better to avoid this variable
df1 <- df[, -c(1:3, 5:7, 10, 13:15, 17, 20, 22:24)]
str(df1)

#' Data visualizations

hist(df$pricesold)
hist(df$adom_agentdaysonmarket)
hist(log(df$adom_agentdaysonmarket))


temp <- df1[, c(1:10)]
library(PerformanceAnalytics)
chart.Correlation(temp)

#' High correlations (>0.8) between listprice, sqft, and lotsqft; between pricesold and listprice (1.00); 
#' and between baths, pricesold, listprice, and sqft. Since sqft is a core predictor of pricesold, 
#' we must drop listprice and lotsqft from our analysis to avoid multicollinearity. We keep baths 
#' though it has a 0.85 correlation with sqft because it is an important variable.

#' Regression models

m1 <- lm(pricesold ~ sqft + Beds + baths + garages + tileroof + yrblt + 
              privatepool + communitypool + specialsale + year, data=df1)
summary(m1)

m2 <- lm(pricesold ~ sqft*Beds + sqft*baths + garages + tileroof + yrblt + 
              privatepool + communitypool + specialsale + year, data=df1)

m3 <- lm(pricesold ~ year, data=df1)         # Why is this model interesting?

library(stargazer)
stargazer(m3, m1, m2, type='text', single.row=TRUE)

m4 <- lm(adom_agentdaysonmarket ~ yrblt + privatepool + communitypool + specialsale + lppersqft + year, data=df)

m5 <- lm(adom_agentdaysonmarket ~ yrblt*lppersqft + privatepool + communitypool + specialsale + year, data=df)

m6 <- lm(adom_agentdaysonmarket ~ year, data=df1)

stargazer(m6, m4, m5, type='text', single.row=TRUE)

#' Test for assumptions

plot(m2)
plot(m5)

shapiro.test(m2$res)                        # Shapiro-Wilk's test of multivariate normality
shapiro.test(m5$res)

bartlett.test(list(m2$res, m2$fit))         # Bartlett's test of homoskedasticity
bartlett.test(list(m5$res, m5$fit))

library("car")                              # Multicollinearity test
vif(m1)
vif(m4)

library(lmtest)
dwtest(m2)                                  # Durbin-Watson test of autocorrelation
dwtest(m5)

#' Was this a good analysis? How do we know?
#' Can we make this analysis better? How?

#'------------------------------------------------------------------

#' Seemingly unrelated regression (SUR) models
#' A system of linear equations that are correlated across the equations
#' but uncorrelated across observations. SUR corrects for correlated errors
#' across equations
#' 
df <- read_excel("HuntersGreenHomeSales.xlsx", sheet='Data')

eq1 <- pricesold - sqft*Beds + sqft*Beds + garages + tileroof + age +
    privatepool + communitypool + specialsale + year

eq2 <- adom_agentdaysonmarket - age*lppersqft + privatepool + communitypool + 
  specialsale + year

library(systemfit)
system <-list(eq1=eq1, eq2=eq2)
sur <- systemfit(system, method="SUR", date=df)
summary (sur)

summary(m2)
summary(m5)

#------------------------------------------------

hist(df1$adom_agentdaysonmarket)
hist(log(df1$adom_agentdaysonmarket))

m4 <- lm(log(adom_agentdaysonmarket) ~ age + privatepool + communitypool +
          specialsale + lppersqft + year, date=df1)

m5 <- lm(log(adom_agentdaysonmarket+1) ~ age + privatepool + communitypool +
           specialsale + lppersqft + year, date=df1)

stargazer(m4, m5, type-"text", single.row = TRUE)

m5$coefficients
exp(m5$coefficients)
