library(readr)
library(tidyverse)
library(car)
library(lmtest)
library(olsrr)
library(corrplot)
library(dplyr)

data <- read_csv("C:/Users/User/OneDrive/Desktop/Econometrics/project/WorldHappiness_Corruption_2015_2020.csv")
# Group by continent and summarize the happiness score
library(ggplot2)
library(dplyr)
ggplot(data, aes(x = happiness_score, fill = continent)) +
  geom_histogram(binwidth = 0.5, position = "identity", alpha = 0.6) +
  facet_wrap(~continent, scales = "free_y") +
  labs(title = "Distribution of Happiness Score by Continent",
       x = "Happiness Score",
       y = "Count",
       fill = "Continent") +
  theme_minimal()


# Excluding Country, continent, Year columns
data_numeric <- data %>% select(-Country, -continent, -Year) 

View(data_numeric)
summary(data_numeric)

# Multiple linear regression model
model <- lm(happiness_score ~ gdp_per_capita + health + freedom + generosity + government_trust + social_support + cpi_score + dystopia_residual + family, data=data_numeric)
summary(model)

#calculate the VIF for each predictor variable in the model
vif(model)
#We can see the VIF values for each of the predictor variables:
#gdp_per_capita: VIF = 4.002235
#health: VIF = 3.286118
#freedom: VIF = 1.746914
#generosity: VIF = 1.288610
#government_trust: VIF = 1.879083
#social_support: VIF = 9.422150
#cpi_score: VIF = 3.117059
#family: VIF = 7.831387
#dystopia_residual: VIF = 1.871957
#Since each of the VIF values are lower than 10, multicollinearity is not a problem in the model.

ols_test_normality(model) #Test Kolmogorov-Smirnov p value 0.5402 > 0.05.it indicates a normal distribution. 

# Perform the Breusch–Pagan Test to Check Heteroscedasticity
lmtest::bptest(model) # p-value = 7.794e-16 <0.05. there is a strong evidence of heteroskedacity

#Corrected Standard Errors
library(sandwich)
vcov <- vcovHC(model, type = "HC1")
vcov
robust_se <- sqrt(diag(vcov))
robust_se
coeftest(model, vcov. = vcov)

#####Visualizing the correlation matrix#########
install.packages("corrplot")
library(corrplot)
windows(width = 12, height = 8)
robust_corr_matrix <- cov2cor(vcov)
corrplot(robust_corr_matrix)

# Perform Weighted Least Squares (WLS) regression
wt <- 1 / lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2
wls_model <- lm(happiness_score ~ gdp_per_capita + health + freedom + 
                  generosity + government_trust + social_support + 
                  cpi_score + family + dystopia_residual, 
                data = data_numeric, weights = wt)

lmtest::bptest(wls_model) # There is still heteroskedacity. so we are going to use corrected standard errors for the hypothesis.

library("olsrr")

# Create a Linear Regression Model
model_government_trust <- lm(data_numeric$happiness_score ~ government_trust, data = data_numeric)
summary(model_government_trust)
 
# 1. Check the Residuals vs Fitted Plot I
windows(width = 12, height = 8)# Resize the plotting window
plot(model_government_trust)

# 2. Check the Normal Q-Q Plot
ols_plot_resid_qq(model_government_trust)

# 3. Create a Histogram of the Residuals
ols_plot_resid_hist(model_government_trust)

# 4. Create a Boxplot I
boxplot(model_government_trust$residuals, main="Residual Box Plot")


## 5. Perform a Normality 
ols_test_normality(model_government_trust)
#Test Kolmogorov-Smirnov p value 0.5314 > 0.05.it indicates a normal distribution. 

####Checking for linearity############
install.packages("ggplot2")
install.packages("dplyr")
install.packages("aTSA")
install.packages("knitr")
install.packages("dynlm")
install.packages("zoo")

library(readxl)
library(ggplot2)
library(dplyr)
library(aTSA)
library(knitr)
library(dynlm)

#Ramsey’s RESET (linearity) testing
library(lmtest)
modelA.RESET <- resettest(hs, power = 2, type = "fitted", 
                          data = data_numeric)
modelA.RESET #p value 0.2537 > 0.05 The model is correct linearly specified.

#Jarque-Bera (residuals normality)
library(tsoutliers)
modelA.JB <- JarqueBera.test(hs$residuals)
modelA.JB

