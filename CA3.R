data <- read.csv('C:/Users/Owner/Downloads/carbon.csv', header = FALSE, stringsAsFactors = FALSE)[-c(1, 2, 3, 4),]
colnames <- c("Year", "Agricultureandfishing", "Rubberandplasticproduct") # renaming the columns
colnames(data) <- colnames
data$Agricultureandfishing <- as.numeric(as.character(data$Agricultureandfishing))
data$Rubberandplasticproduct <- as.numeric(as.character(data$Rubberandplasticproduct))
str(data)
data
plot(data)
plot(data$Agricultureandfishing, data$Rubberandplasticproduct, type = "o", col = "red", xlab = "Rubberandplasticproduct", ylab = "Agricultureandfishing")
title(main = "Rubberandplasticproduct and Agricultureandfishing product", col.main = 'blue', font.main = 4)
#scatter plot
scatter.smooth(x = data$Agricultureandfishing, y = data$Rubberandplasticproduct, maintainer = "Rubberandplasticproduct ~ Agricultureandfishing")

#density plot
library(e1071)
par(mfrow = c(1, 2))
plot(density(data$Rubberandplasticproduct), main = "Density Plot : Rubberandplasticproduct",
    ylab = "Frequency",
    sub = paste("Skewness:", round(e1071::skewness(data$Rubberandplasticproduct), 2)))
polygon(density(data$Rubberandplasticproduct), col = "green")

plot(density(data$Agricultureandfishing), main = "Density Plot : Agricultureandfishing",
    ylab = "Frequency",
    sub = paste("Skewness:", round(e1071::skewness(data$Agricultureandfishing), 2)))
polygon(density(data$Agricultureandfishing), col = "blue")

# Correlation Test
cor(data$Agricultureandfishing, data$Rubberandplasticproduct)
# linear Model
linearMod <- lm(Agricultureandfishing ~ Rubberandplasticproduct, data = data)nal
print(linearMod)
summary(linearMod)
#polynominal
polynomialMod <- lm(Agricultureandfishing ~ Rubberandplasticproduct + I(Agricultureandfishing ^ 2), data = data)
print(polynomialMod)
summary(polynomialMod)
# Sampling
no_of_records <- sample(1:nrow(data), 0.8 * nrow(data))
training_data <- data[no_of_records,]
testing_data <- data[-no_of_records,]

#Training linear Model
lr_model <- lm(Agricultureandfishing ~ Rubberandplasticproduct, data = training_data)
lm_predicted <- predict(lr_model, testing_data)
lm_predicted
lm_actual_preds <- data.frame(cbind(actuals = testing_data$Agricultureandfishing, predicted = lm_predicted))
lm_actual_preds

#Training Polynomial Model(Second Order)
pl_model <- lm(Agricultureandfishing ~ Rubberandplasticproduct + I(Rubberandplasticproduct ^ 2), data = training_data)
pl_predicted <- predict(pl_model, testing_data)
pl_predicted
pl_actual_preds <- data.frame(cbind(actuals = testing_data$Agricultureandfishing, predicted = pl_predicted))
pl_actual_preds



# Lets validate, Compare and Decide which model fits our data
# AIC
AIC(linearMod)
AIC(polynomialMod)
# BIC
BIC(linearMod)
BIC(polynomialMod)
# Correlation Accuracy
lm_correlation_accuracy <- cor(lm_actual_preds)
lm_correlation_accuracy

pl_correlation_accuracy <- cor(pl_actual_preds)
pl_correlation_accuracy

#min_max accuracy
lm_min_max_accuracy <- mean(apply(lm_actual_preds, 1, min) / apply(lm_actual_preds, 1, max))
lm_min_max_accuracy

pl_min_max_accuracy <- mean(apply(pl_actual_preds, 1, min) / apply(pl_actual_preds, 1, max))
pl_min_max_accuracy
# Mape
lm_mape <- mean(abs(lm_actual_preds$predicted - lm_actual_preds$actuals) / lm_actual_preds$actuals)
lm_mape
pl_mape <- mean(abs(pl_actual_preds$predicted - pl_actual_preds$actuals) / pl_actual_preds$actuals)
pl_mape
# Summary
summary(lr_model)
summary(pl_model)