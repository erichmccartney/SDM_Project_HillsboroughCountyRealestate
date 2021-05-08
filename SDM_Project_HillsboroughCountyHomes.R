#' SDM Project: Hillsborough County Real Estate
# Carla

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

#Viviana

install.packages("rio")
install.packages("moments")
install.packages("car")
install.packages("readxl")
install.packages("corrplot")
install.packages("reshape2")

rm(list=ls())

library(rio)
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(readxl)
library(openxlsx)
library(lubridate)
library(reshape2)
library(stargazer)
library(lme4) 


#1.	Load the file   
df <- read.csv("HillsboroughCountyData.csv")

#2. Feature engineering
dataset$BuildingAge =  2021 - dataset$YearBuilt
dataset$PricePerHeatedArea = dataset$JustValue/dataset$TotalHeatedAreaSqFt
dataset$HeatedAreaProportion = dataset$TotalHeatedAreaSqFt/(dataset$Acreage*43560)
dataset$LastSaleDate = as.Date(dataset$LastSaleDate, format =  "%m/%d/%y" ) 
dataset$LengthOwnershipProportion = dataset$YearsSinceTurnover/dataset$BuildingAge

#3. Group by neighborhood (unit of analysis)
neighborhood_df = dataset %>%
  group_by(Neighborhood) %>%
  summarize(stories_avg = mean(TotalStories, na.rm = TRUE),
            bedrooms_avg = mean(TotalBedrooms, na.rm = TRUE),
            bathrooms_avg = mean(TotalBathrooms, na.rm = TRUE),
            building_age_avg = mean(BuildingAge, na.rm = TRUE),
            price_avg = mean(PricePerHeatedArea, na.rm = TRUE),
            heated_area_proportion_avg = mean(HeatedAreaProportion, na.rm = TRUE),
            grade_point_2019 = mean(Avg_GradePoint2019, na.rm = TRUE),
            minority_percentage = mean(Avg_Percentage.of.Minority.Students, na.rm=TRUE),
            economically_disadvantaged_percentage = mean(Avg_Percentage.of.Economically.Disadvanteged.Students, na.rm = TRUE),
            length_of_ownership = mean(YearsSinceTurnover, na.rm=TRUE)
  )

#4. Checking missing values
summary(neighborhood_df)
summary(dataset$TotalHeatedAreaSqFt)
dataset$TotalHeatedAreaSqFt == 0)


# We discovered that 361 observations did not have values for TotalHeatedAreaSqFt and 1832 did not have values for Acreage and were dropped from the analysis
# It represents 5.6823% of our dataset
# It caused us to drop 2 neighborhoods 
nrow(filter(dataset, Acreage == 0 | TotalHeatedAreaSqFt == 0))
nrow(filter(dataset, Acreage == 0 | TotalHeatedAreaSqFt == 0))/38945


dataset2 = filter(dataset, TotalHeatedAreaSqFt != 0)
dataset2 = filter(dataset2, Acreage != 0)

neighborhood_df2 = dataset2 %>%
  group_by(Neighborhood) %>%
  summarize(stories_avg = mean(TotalStories, na.rm = TRUE),
            bedrooms_avg = mean(TotalBedrooms, na.rm = TRUE),
            bathrooms_avg = mean(TotalBathrooms, na.rm = TRUE),
            building_age_avg = mean(BuildingAge, na.rm = TRUE),
            price_avg = mean(PricePerHeatedArea, na.rm = TRUE),
            heated_area_proportion_avg = mean(HeatedAreaProportion, na.rm = TRUE),
            grade_point_2019 = mean(Avg_GradePoint2019, na.rm = TRUE),
            minority_percentage = mean(Avg_Percentage.of.Minority.Students, na.rm=TRUE),
            economically_disadvantaged_percentage = mean(Avg_Percentage.of.Economically.Disadvanteged.Students, na.rm = TRUE),
            length_of_ownership = mean(YearsSinceTurnover, na.rm=TRUE),
            length_of_ownership_proportion = mean(LengthOwnershipProportion, na.rm=TRUE)
  )
summary(neighborhood_df2)

# 5. Create Visualizations
attach(neighborhood_df2)
par(mfrow=c(3,4))
hist(stories_avg)
hist(bedrooms_avg)
hist(bathrooms_avg)
hist(building_age_avg)
hist(price_avg)
hist(grade_point_2019)
hist(heated_area_proportion_avg)
hist(minority_percentage)
hist(economically_disadvantaged_percentage)
hist(length_of_ownership)
hist(length_of_ownership_proportion)


# 6. Check for extremely high correlations     
par(mfrow=c(1,1))
cor = cor(neighborhood_df2[,c(-1)])
cor
corrplot(cor, method = "circle")

# we found that the percentage of minority and economically disadvantage percentage are highly and negatively correlated to grade point, and so it will be dropped from our analysis to avoid multicollinearity
colnames(neighborhood_df2)

# 7. Statistical Analysis

model1 = lm(length_of_ownership~stories_avg + bedrooms_avg + bathrooms_avg 
            + price_avg + heated_area_proportion_avg + grade_point_2019 
            + building_age_avg, neighborhood_df2)

model2 = lm(length_of_ownership~stories_avg + bedrooms_avg + bathrooms_avg 
            + price_avg + heated_area_proportion_avg + grade_point_2019 
            + length_of_ownership_proportion, neighborhood_df2)

model3 = lm(length_of_ownership~stories_avg + bedrooms_avg + bathrooms_avg 
            + log(price_avg) + heated_area_proportion_avg + grade_point_2019 
            + building_age_avg, neighborhood_df2)

summary(model1)
summary(model2)
summary(model3)

stargazer(model1, model2, model3, type="text")

#Linearity
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
plot(neighborhood_df2$length_of_ownership,model1$fitted.values,
     pch=19,main="Length of Ownership Actuals v. Fitted")
abline(0,1,col="red",lwd=3)

#Normality
par(mar=c(5.1,4.1,4.1,2.1))
qqnorm(model1$residuals,pch=19,
       main="Length of Ownership Normality Plot")
qqline(model1$residuals,lwd=3,col="red")

#Equality of Variances
par(mar=c(5.1,4.1,4.1,2.1))
plot(neighborhood_df2$length_of_ownership,rstandard(model1),
     pch=19,main="Model 1 Residual Plot")
abline(0,0,col="red",lwd=3)

#It looks like we have some heteroskedasticety

model4 = lm(1+log(length_of_ownership)~stories_avg + bedrooms_avg + bathrooms_avg 
            + log(price_avg) + heated_area_proportion_avg + grade_point_2019 
            + building_age_avg, neighborhood_df2)


#Equality of Variances
par(mar=c(5.1,4.1,4.1,2.1))
plot(neighborhood_df2$length_of_ownership,rstandard(model4),
     pch=19,main="Model 4 Residual Plot")
abline(0,0,col="red",lwd=3)


#Identifying high leverage points.
lev=hat(model.matrix(model1))
plot(lev,pch=19,ylim=c(0,.5), main="High leverage points")
abline(3*mean(lev),0,col="red",lwd=3)
neighborhood_df2[lev>(3*mean(lev)),]  ##identifying which data points are 3 times higher than the mean laverage
neighborhood_df2[lev>(3*mean(lev)),1]
outliers = which(lev>(3*mean(lev)),1)

df_no_outliers = neighborhood_df2[-outliers,]

model5 = lm(length_of_ownership~stories_avg + bedrooms_avg + bathrooms_avg 
            + price_avg + heated_area_proportion_avg + grade_point_2019 
            + building_age_avg, df_no_outliers)

summary(model5)


par(mar=c(5.1,4.1,4.1,2.1))
plot(df_no_outliers$length_of_ownership,rstandard(model5),
     pch=19,main="Model 5 Residual Plot")
abline(0,0,col="red",lwd=3)


model6 = lm(1+log(length_of_ownership)~stories_avg + bedrooms_avg + bathrooms_avg 
            + price_avg + heated_area_proportion_avg + grade_point_2019 
            + building_age_avg, df_no_outliers)

par(mar=c(5.1,4.1,4.1,2.1))
plot(df_no_outliers$length_of_ownership,rstandard(model6),
     pch=19,main="Model 6 Residual Plot")
abline(0,0,col="red",lwd=3)