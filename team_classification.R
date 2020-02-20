source("team_helper_functions.r")

# Clean data
data <- read.csv("input/data.csv", na.strings=c(""," ","NA"), header=TRUE) # Loading data
data <- clean(data)

# Split data
split_data <- splitData(data, 0.8)
training <- split_data$training
validation <- split_data$validation
testing <- split_data$testing

##
## Defining the model using ctree at validation data
#

variable <- Is_Resigning~ YearsAtCompany + YearsInCurrentRole + YearsWithCurrManager + YearsSinceLastPromotion + TotalWorkingYears + JobLevel + MonthlyIncome + Department + PerformanceRating + EducationField + DistanceFromHome + Education + PercentSalaryHike + Age + JobRole + NumCompaniesWorked
avg_probability <- mean(training$Is_Resigning == "1")

prediction <- predictCTree(variable, training, validation, avg_probability)
confusionMatrix(prediction$classification,validation$Is_Resigning, positive = "1")

# ROC Curve
ROC_prediction <- prediction(prediction$probabilities, validation$Is_Resigning)
ROC <- performance(ROC_prediction,"tpr","fpr") # Create ROC curve data
plot(ROC) # Plot ROC curve

# AUC (area under curve)
# 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value
auc.tmp <- performance(ROC_prediction,"auc") # Create AUC data
auc_validation <- as.numeric(auc.tmp@y.values) # Calculate AUC
auc_validation

##
## Comparing with logistic
##

prediction <- predictLogistic(variable, training, validation, avg_probability)
confusionMatrix(prediction$classification,validation$Is_Resigning, positive = "1")

# ROC Curve
ROC_prediction <- prediction(prediction$probabilities, validation$Is_Resigning)
ROC <- performance(ROC_prediction,"tpr","fpr") # Create ROC curve data
plot(ROC) # Plot ROC curve

# AUC (area under curve)
# 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value
auc.tmp <- performance(ROC_prediction,"auc") # Create AUC data
auc_validation <- as.numeric(auc.tmp@y.values) # Calculate AUC
auc_validation

##
## Validating CTREE model using test data
#
prediction <- predictCTree(variable, training, testing, avg_probability)
confusionMatrix(prediction$classification,testing$Is_Resigning, positive = "1")

# ROC Curve
ROC_prediction <- prediction(prediction$probabilities, validation$Is_Resigning)
ROC <- performance(ROC_prediction,"tpr","fpr") # Create ROC curve data
plot(ROC) # Plot ROC curve

# AUC (area under curve)
# 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value
auc.tmp <- performance(ROC_prediction,"auc") # Create AUC data
auc_validation <- as.numeric(auc.tmp@y.values) # Calculate AUC
auc_validation
