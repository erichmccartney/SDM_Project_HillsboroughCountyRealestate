#' SDM Project: Hillsborough County Real Estate

df <- read.csv("HillsboroughCountyData.csv")
str(df)
View(df)

hist(df$YearsSinceTurnover)
hist(log(df$YearsSinceTurnover))
  
#DensityPlot 
library(lattice)
densityplot(~YearsSinceTurnover | Avg_GradePoint2019, data=df)

#Linear Regression
#Summary: 
linearMod <- lm(YearsSinceTurnover ~ Avg_GradePoint2019, data=df) 
print(linearMod)

#Pooled OLS Model 
ols <- lm(YearsSinceTurnover ~ Avg_GradePoint2019*LastSalePrice, data=df)
summary(ols)

#Fixed Effects Model 
fe <- lm(YearsSinceTurnover ~ Avg_GradePoint2019*LastSalePrice + Avg_GradePoint2019*SchoolZipCodeGroup + as.factor(Neighborhood), data=df)
summary(fe)

library(stargazer)
options(max.print = 60000)
stargazer(linearMod, ols, fe, type="text", single.row=TRUE)

#Test for Assumptions 
hist(ols$res)
ols$fit

#ResidualPlot 
plot(fe$res ~ fe$fit)  

##QQPlot 
qqnorm(fe$res)
qqline(fe$res, col="red")

#Shapiro-Wilk's Test inconclusive sample size must be between 3 and 5000 
shapiro.test(fe$res)  