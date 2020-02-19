if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")}
pacman::p_load("caret", "dplyr", "e1071", "glmnet","lift","MASS", "ROCR", "partykit", "pracma","xgboost", "Hmisc", "pastecs", "psych", "plyr")

library(stringr)
library(dplyr)
library(plyr)

# splitData <- function (data, training_pct) {
#   size_all <- nrow(data)
#   size_training <- size_all * training_pct
#   
#   set.seed(110392) 
#   inTrain <- createDataPartition(y = data$Is_Resigning,
#                                  p = training_pct, list = FALSE)
#   training <- data[ inTrain,]
#   validation_and_testing <- data[ -inTrain,]
#   
#   inTest <- createDataPartition(y = validation_and_testing$Is_Resigning, p = 0.5, list = FALSE)
#   validation <- validation_and_testing[ -inTest,]
#   
#   testing <- validation_and_testing[ inTest,]
#   
#   return(list(training = training, validation = validation, testing = testing))
# }

fixNAs <- function(data_frame){
  integer_reac <- 0
  factor_reac <- "Other"
  character_reac<-"Other"
  
  for (i in 1 : ncol(data_frame)){
    if (class(data_frame[,i]) %in% c("numeric","integer")) {
      if (any(is.na(data_frame[,i]))){
        # sets numeric values to the median value of the dataset
        data_frame[is.na(data_frame[,i]),i] <- median(data_frame[,i], na.rm = TRUE)
      }
    } else
      if (class(data_frame[,i]) %in% c("factor")) {
        if (any(is.na(data_frame[,i]))){
          data_frame[,i]<-as.character(data_frame[,i])
          data_frame[is.na(data_frame[,i]),i]<-factor_reac
          data_frame[,i]<-as.factor(data_frame[,i])
          
        } 
      } else {
        if (class(data_frame[,i]) %in% c("character")) {
          if (any(is.na(data_frame[,i]))){
            data_frame[is.na(data_frame[,i]),i]<-character_reac
          }  
        }       
      }
  } 
  return(data_frame) 
}

combineRareCategories <- function(data_frame, mincount) { 
  for (i in 1 : ncol(data_frame)){
    a <- data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-"Other"
    data_frame[,i]<-a }
  return(data_frame)
}

numeric_factor <- function (x) as.factor(as.numeric(x))

clean <- function(data_frame) {
  #Deleting Unused variables
  data_frame$EmployeeCount <- NULL
  data_frame$Application.ID <- NULL
  data_frame$EmployeeNumber <- NULL
  data_frame$Over18 <- NULL
  data_frame$StandardHours <- NULL
  
  # Deleting bad data
  data_frame$HourlyRate <- NULL
  data_frame$DailyRate <- NULL
  data_frame$MonthlyRate <- NULL
  
  if (!is.null(data_frame$Attrition)) {
    data_frame <- within(data_frame, Is_Resigning <- numeric_factor(Attrition %in% c("Voluntary Resignation")) )
    data_frame <- data_frame[,c(ncol(data_frame),1:(ncol(data_frame)-1))]
  }
  data_frame$Attrition <- NULL

  # Fixing Rare Categories & NAs
  data_frame <- fixNAs(data_frame)
  data_frame <- combineRareCategories(data_frame, 10)

  return(data_frame)
}

### Analyze clusters
# files <- c("ProjectData_with_hclust_membership_all_factors.csv",
#             "ProjectData_with_hclust_membership_compressed_factors.csv",
#             "ProjectData_with_kmeans_membership_all_factors.csv",
#             "ProjectData_with_kmeans_membership_compressed_factors.csv")
# for (file in files) {
#   data <- read.csv(file, na.strings=c(""," ","NA"), header=TRUE) # Loading data
#   data <- clean(data)
#   desc <- describeBy(data, data$cluster, mat = TRUE)
#   write.csv(desc, paste(file, "_RESULTS.csv"))
# }
# 


