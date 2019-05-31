# Loading the rent and income dataset of Ireland

income_rent <- read.csv("Income_Rent.csv")
str(income_rent)
head(income_rent)

# Building linear regression model Using Ireland rent and income dataset
# Which contains rent and income details of various part of Ireland.
linear_model <- lm(Rent ~ Income, data = income_rent)
linear_model
summary(linear_model)

# The estimates of the beta coefficients
# the standard errors (SE), which defines the accuracy of beta coefficients. 
# For a given beta coefficient, the SE reflects how the coefficient varies under 
# repeated sampling. It can be used to compute the confidence intervals and the t-statistic.
# the t-statistic and the associated p-value, which defines the statistical significance of the beta coefficients.



# Plotting Income and Rent variable to see relationship between the response(rent) and
# predictor (Income) variable

plot(income_rent$Income,income_rent$Rent,
     xlab="Income",
     ylab="Rent",
     main = "Scatter plot showing regression line
     for Rent predicted from Income")
abline(linear_model)

# Graph shows a there is some relationship between rent and income variable

cor(income_rent$Rent,income_rent$Income)

# Examining the 95% confidence intervals of the model

confint(linear_model)

# Scatter plots helps to visualise any linear relationships between the 
# dependent (response) rent variable and independent (predictor) Income variables



scatter.smooth(x = income_rent$Income, 
               y = income_rent$Rent, 
               main = "Income ~ Rent",
               xlab = "Income",
               ylab = "Rent")


# Box Plot
par(mfrow = c(1, 2)) # divide graph area in 2 columns
boxplot(income_rent$Income, main = "Income", sub = paste("Outlier rows: ", boxplot.stats(income_rent$Income)$out)) # box plot for 'Income'
boxplot(income_rent$Rent, main = "Rent", sub = paste("Outlier rows: ", boxplot.stats(income_rent$Rent)$out)) # box plot for 'Rent'


# Skewness function to examine normality of data
# install.packages("e1071")
# Density Plot
library(e1071)
# Divide graph area in 2 columns
par(mfrow = c(1, 2))

# Density plot for Income
plot(density(income_rent$Income), main = "Density Plot :Income",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(income_rent$Income), 2)))

# Filling the area within the density plot to red
polygon(density(income_rent$Income), col = "red")

# Density plot for Rent
plot(density(income_rent$Rent), main = "Density Plot :Rent",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(income_rent$Rent), 2)))

# Filling the area within the density plot to red
polygon(density(income_rent$Rent), col = "red")


# Calculating correlation test between Income and Rent
cor(income_rent$Income, income_rent$Rent)

# build linear regression model on full data
linearMod <- lm(Rent ~ Income, data = income_rent)
linearMod

# model summary
summary(linearMod)

model_summary <- summary(linearMod)

# model coefficients
model_coeffs <- model_summary$coefficients
model_coeffs

# get beta estimate for Income
beta.estimate <- model_coeffs["Income", "Estimate"]

# get std.error for Income
std_error <- model_coeffs["Income", "Std. Error"]

# calc t statistic
t_value <- beta.estimate / std_error
p_value <- 2 * pt(-abs(t_value), df = nrow(income_rent) - ncol(income_rent)) # calc p Value
f_statistic <- linearMod$fstatistic[1] # fstatistic
f <- summary(linearMod)$fstatistic # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower = FALSE)


# sample chooses a random sample
# from 1:all records from cars, 80% of rows
no_of_records <- sample(1:nrow(income_rent), 0.8 * nrow(income_rent))
# model training data
training_data <- income_rent[no_of_records,]
training_data
# test data
testing_data <- income_rent[-no_of_records,]
testing_data

# Build the model on training data
# lm(formula, data) where
# formula describes the model to be fit
lm_model <- lm(Rent ~ Income, data = training_data)

# model summary
summary(lm_model)

# predict rent from testing data
lm_predicted <- predict(lm_model, testing_data)
summary(lm_predicted)
# make actuals_predicteds dataframe.
lm_actuals_preds <- data.frame(cbind(actuals = testing_data$Rent, 
                                       predicted = lm_predicted))
head(lm_actuals_preds)

AIC(linearMod)

BIC(linearMod)

correlation_accuracy <- cor(lm_actuals_preds)
correlation_accuracy

# Min - max accuracy
lm_min_max_accuracy <- mean(apply(lm_actuals_preds, 1, min) / apply(lm_actuals_preds, 1, max))
lm_min_max_accuracy

# MAPE
lm_mape <- mean(abs((lm_actuals_preds$predicted - lm_actuals_preds$actuals)) / lm_actuals_preds$actuals)
lm_mape

# Global validation of linear model assumption
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(linearMod)
summary(gvmodel)


#Building Polynomial Model 


# # sample chooses a random sample
# # from 1:all records from cars, 80% of rows
# no_of_records <- sample(1:nrow(income_rent), 0.8 * nrow(income_rent))
# # model training data
# training_data <- income_rent[no_of_records,]
# training_data
# # test data
# testing_data <- income_rent[-no_of_records,]
# testing_data
# polymodel <- lm(Rent ~ Income + I(Income^2), data = income_rent)

# Build the Polynomial model on training data
# lm(formula, data) where
# formula describes the model to be fit
poly_model <- lm(Rent ~ Income + I(Income^2), data = income_rent)


# model summary
summary(poly_model)


# predict distance from testing data
poly_predicted <- predict(poly_model, testing_data)
summary(poly_predicted)
# make actuals_predicteds dataframe.
poly_actuals_preds <- data.frame(cbind(actuals = testing_data$Rent, 
                                  predicted = poly_predicted))
head(poly_actuals_preds)

AIC(poly_model)

BIC(poly_model)

correlation_accuracy <- cor(poly_actuals_preds)
correlation_accuracy

# Min - max accuracy
poly_min_max_accuracy <- mean(apply(poly_actuals_preds, 1, min) / apply(poly_actuals_preds, 1, max))
poly_min_max_accuracy

# MAPE
poly_mape <- mean(abs((poly_actuals_preds$predicted - poly_actuals_preds$actuals)) / poly_actuals_preds$actuals)
poly_mape

summary(poly_predicted)

library(gvlma)
gvmodel <- gvlma(poly_model)
summary(gvmodel)


multi_linear <- lm(Rent ~ Income + Year, data = income_rent)
par(mfrow = c(2, 2))
plot(multi_linear)

summary(multi_linear)

multi_predicted <- predict(multi_linear, testing_data)
summary(multi_predicted)

# make actuals_predicteds dataframe.
multi_actuals_preds <- data.frame(cbind(actuals = testing_data$Rent, 
                                       predicted = multi_predicted))
head(multi_actuals_preds)

AIC(multi_linear)

BIC(multi_linear)

correlation_accuracy <- cor(multi_actuals_preds)
correlation_accuracy

# Min - max accuracy
multi_min_max_accuracy <- mean(apply(multi_actuals_preds, 1, min) / apply(multi_actuals_preds, 1, max))
multi_min_max_accuracy

# MAPE
multi_mape <- mean(abs((multi_actuals_preds$predicted - multi_actuals_preds$actuals)) / multi_actuals_preds$actuals)
multi_mape

par(mar=c(1,1,1,1))
library(gvlma)
gvmodel <- gvlma(multi_linear)
summary(gvmodel)


