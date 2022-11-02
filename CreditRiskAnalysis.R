# Load data - Modify the path of the file according to your system

loan_data <- read.csv("loans.csv")

#Install Packages required

install.packages("gmodels")
install.packages("DataExplorer")
install.packages("caTools")
install.packages("ROCR")

# View the structure of loan_data

str(loan_data)
head(loan_data)


#EDA

library("DataExplorer")
library("gmodels")
library("caTools")
library("ROCR")

plot_str(loan_data)

head(loan_data)

# Call CrossTable() on status

CrossTable(loan_data$status)

# Call CrossTable() on salary and loan status

CrossTable(loan_data$married_status , loan_data$status, prop.r = TRUE, prop.c = FALSE, prop.chisq = FALSE, prop.t = FALSE)


plot_histogram(loan_data)


plot(loan_data$status, loan_data$cibil, xlab = "default", ylab = "cibil")


#Check for missing data

plot_missing(loan_data)


#Create a report of EDA

create_report(loan_data)


#Now let us start the process of modeling

#First step is to create training and test data sets

# Set seed to generate the same sample and reproduce the results of this tutorial

set.seed(567)


#Create train and test data sets to fit your model on training set and test on your test data set

loansample <- sample.split(Y = loan_data$status, SplitRatio = 0.8)

train_data <- loan_data[loansample,]
test_data <- loan_data[!loansample,]


#Check the data distribution in Training and Testing data sets

CrossTable(train_data$status)

CrossTable(test_data$status)


# Run GLM Model with one variable

library("glm2")

log_model <- glm(status ~ civil, family = "binomial", data = train_data)

log_model


# Run GLM Model with all variables]

log_model <- glm(status ~ gender+married_status+salary+civil+appcount+phonegrade+simstrength, family = "binomial", data = train_data)

#view results of the model

summary(log_model)

# Use the model to predict probability of default


predict <- predict(log_model, test_data, type = 'response')


# Construct a confusion matrix

table(test_data$status, predict > 0.5)

#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, test_data$status)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))












