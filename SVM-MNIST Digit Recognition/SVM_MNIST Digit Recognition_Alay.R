################################################### Loading Packages #######################################################
library(gridExtra)
library(readr)
library(car)
library(dplyr)
library(caret)
library(kernlab)
library(ggplot2)
library(MASS)
library(caTools)
library(e1071)
library(doParallel)
library(foreach)
library(cowplot)

############################################ SVM Handwritten Digit Recognition ############################################################################

#1. Business Understanding
#2. Data Understanding
#3. Data Preparation
#4. Model Building
#5. Hyper Parameter Tuning and Cross Validation

############################################## 1). Business Understanding #####################################################################

# We have an image of a digit submitted by a user via a scanner, a tablet, or other digital devices. 
# The goal is to develop a model using Support Vector Machine which should correctly classify the handwritten digits  
# based on the pixel values given as features.

############################################### 2). Data Understanding ###################################################################

### There are already two Seperate Data Files provided for training and testing. Hence we are not required to separate the 
### data into training and testing by ourselves.

### Loading the data files:
train <- read.csv("mnist_train.csv", header = FALSE)
test <- read.csv("mnist_test.csv", header = FALSE)

## Understanding Dimensions:
dim(train)

## Train DataSet:
# Number of Instances : 60000
# Number of Attributes : 784 + 1 dependent variable i.e. digits to be recognized

## Test Dataset :
# Number of Instances : 10000
# Number of Attributes : 784 + 1 dependent variable i.e. digits to be recognized

## Structure of Data set
str(train)
str(test)

# Going deeper to understand the number of attributes, we see that one attribute, i.e. the first column is the 
# dependent column, known as labels or digits to be recognized. Excluding that, we are left with 784 variables. And 
# basically, these 784 variables are nothing but the pixels (28 X 28) flattened out as features.

############################################### 3). Data Preparation ###################################################################

# With 60000 instances and 784 attributes, it is not advisable to perform a model taking everything into consideration,
# since doing so will be very expensive computationally. Hence, we need to bring the data to reasonable levels by 
# reducing the dimensions by a large extent. Basically, we can reduce the dimension with following methods:

# 1) Removing the columns/attributes with 0 or very low variance
# 2) Performing PCA (Principal Component Analysis)

# Now before moving into Dimension Reduction techniques as mentioned above, we need to check for some data quality
# issues like missing values, duplication of data etc.

## Checking for missing values
ifelse(anyNA(train) == FALSE, "There are no missing values in training dataset", "There are some missing values in training dataset")
ifelse(anyNA(test) == FALSE, "There are no missing values in testing dataset", "There are some missing values in testing dataset")

## Changing the dependent column name to digits
colnames(train)[1] <- "digits"
colnames(test)[1] <- "digits"

## Converting dependent variable i.e. target class to factor
train$digits <- as.factor(train$digits)
test$digits <- as.factor((test$digits))

## Checking for duplication
which(duplicated(train) == TRUE) # Duplication doesnot exists in training dataset
which(duplicated(test) == TRUE) # Duplicatation doesnot exists in testing dataset

ggplot(train, aes(x = digits, fill = digits)) + geom_bar()

## Removing columns having zero or near to zero variation from training dataset and testing dataset

# Training Data
variation <- nearZeroVar(train[-1],saveMetrics = TRUE)
head(variation)
sum(variation$nzv) # Removing 535 columns, since they have nearly zero variation
remove_cols <- rownames(variation[which(variation$nzv == TRUE), ])
newtrain <- train[, setdiff(names(train), remove_cols)]

# Testing Data
newtest <- test[, setdiff(names(test), remove_cols)]

# Dimensions of data after removing the columns with zero and near to zero variance
dim(newtrain)


### Applying Principle Component Analysis Technique (PCA) to further reduce the dimensions

# Normalizing training and testing data
trainreduced <- newtrain[, -1] / 255
testreduced <- newtest[, -1] / 255

pcov <- cov(trainreduced)
pcaX <- prcomp(pcov)

# Compute standard deviation of each principal component
std_dev <- pcaX$sdev

# Compute Variance
pr_var <- std_dev^2

# Proportion of Variance explained
prop_varex <- pr_var/sum(pr_var)

par(mfrow=c(1,1),mar=c(2.1,2.1,2.1,2.1))

plot(cumsum(prop_varex)*100, ylab = "Cumulative proportion of variance explained", xlab = "Principal Component", type = 'b')
axis(side = 1, at = seq(0,250, by = 25))

## It is clearly visible from the plot that most of the variance is explained by approximately first 35 components.
## Hence, keeping the first 35 components and discarding the rest.
rotation <- pcaX$rotation[, 1:35]

# Training Dataset Final
train_final <- as.matrix(trainreduced) %*% rotation
train_final <- cbind(newtrain$digits, as.data.frame(train_final))
colnames(train_final)[1] <- "digits"

# Testing Dataset Final
test_final <- as.matrix(testreduced) %*% rotation
test_final <- cbind(newtest$digits, as.data.frame(test_final))
colnames(test_final)[1] <- "digits"

################################################ Model Building #############################################################

## Setting up Parallel Processing
c1 <- makeCluster(detectCores()) 
registerDoParallel(c1)

##1. Using Linear Kernel
model_linear <- ksvm(digits~., data = train_final, scale = FALSE, kernel = "vanilladot")
eval_linear <- predict(model_linear, test_final)

# Confustion Matrix - Linear Kernel
confusionMatrix(eval_linear, test_final$digits) # Accuracy : 92.12%
                
##2. Using RBF Kernel
model_rbf <- ksvm(digits~., data = train_final, scale = FALSE, kernel = "rbfdot")
eval_rbf <- predict(model_rbf, test_final)

# Confusion Matrix - RBF Kernel
confusionMatrix(eval_rbf, test_final$digits) # Accuracy : 97.55%

stopCluster(c1)

################################# Hyper Parameter Tuning and Cross Validation #########################################

## Setting up Parallel Processing
c1 <- makeCluster(detectCores()) 
registerDoParallel(c1)
pt<-proc.time()
set.seed(7)

## Setting up Hyperparameters
train_control <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"
grid <- expand.grid(.sigma = seq(0.01, 0.05, by = 0.01), .C = seq(1, 5, by = 1))

# It will take a large amount of computational time and power to perform 5 fold cross validation on complete dataset with 
# 60,000 instances, hence to get results in reasonable time and power, we will take out 30% of the dataset and use that 
# as sample to create the models.

sample_indices <- sample(1:nrow(train_final), 0.3*nrow(train_final))
train_final_sample <- train_final[sample_indices, ]

### Comparing the distribution of data among various classes after sampling, with the original complete dataset
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                   legend.position="right")

plot_grid(ggplot(train, aes(x = digits, fill = digits)) + geom_bar() + ggtitle("Original Dataset") + bar_theme1,
          ggplot(train_final_sample, aes(x = digits, fill = digits)) + geom_bar() + ggtitle("Sampled Dataset") +  bar_theme1,
          align = 'h')

# Comparing the graphs, it is clearly visible that the data distribution among various classes in sampled dataset is 
# nearly similar to the data distribution of original dataset. Hence, we can proceed with the sampled dataset to carry out
# modelling.
          
# Model building
fit.svm <- train(digits~., data=train_final_sample, method="svmRadial", metric=metric, tuneGrid=grid, trControl=train_control)

### Time taken to run the 30% dataset
proc.time() - pt

# User  System  elapsed
# 56.09  1.56   2071.57

# Stopping the cluster cores
stopCluster(c1)

# Calling the model
plot(fit.svm)
print(fit.svm)

# Validating the model results on test data
evaluate_svm <- predict(fit.svm, test_final)
confusionMatrix(evaluate_svm, test_final$digits) 

# 97.48 % Accuracy on test data.

# Even after sampling the data set and taking only 30% of it into training, we got nearly same testing accuracy of 97.5% 
# as we obtained in the simple RBF model, with hyperparameters sigma = 0.04 and c = 5. The results were obtained after
# performing 5-fold cross validation

####################################################### END ################################################################