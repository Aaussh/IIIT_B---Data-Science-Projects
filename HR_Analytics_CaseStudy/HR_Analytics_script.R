### Importing Libraries
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(dplyr)
library(stringr)
library(chron)
library(scales)
library(ROCR)

#######################################################################################################################

################################################## DATA CLEANING ##################################################################

#######################################################################################################################

### Loading the data
emp_survey_data <- read.csv("employee_survey_data.csv")
general_data <- read.csv("general_data.csv")
manager_survey_data <- read.csv("manager_survey_data.csv")
in_time <- read.csv("in_time.csv", stringsAsFactors = FALSE)
out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE)

### Finding out the time spent by an employee at work through in_time and out_time data set  

# Confirming the uniqueness of in_time data and out_time data through primary column i.e. employee ID
length(unique(in_time$X))
length(unique(out_time$X))
setdiff(in_time$X, out_time$X) ### No duplication exists

# Calculating the difference between out_time and in_time to find the num of hours worked by employee on each day.
emp_wrk_hrs <- data.frame(sapply(colnames(in_time[-1]), 
                                 function(i) as.numeric(as.POSIXct(out_time[,i], format = "%d-%m-%Y %H:%M") - as.POSIXct(in_time[,i], format = "%d-%m-%Y %H:%M"))), stringsAsFactors = FALSE)
emp_wrk_hrs <- cbind(in_time[1], round(emp_wrk_hrs,0))

### Removing all the columns with all NA's in emp_wrk_hrs dataset as they can be interpreted as days the offices were closed due to holidays.
emp_wrk_hrs_final <- emp_wrk_hrs
for (i in colnames(emp_wrk_hrs)){
  if(sum(is.na(emp_wrk_hrs[,i])) == nrow(emp_wrk_hrs)){
    emp_wrk_hrs_final <- emp_wrk_hrs_final[, -which(colnames(emp_wrk_hrs_final) %in% c(i))]
  }
}
# Counting the remaining NA's
sum(is.na(emp_wrk_hrs_final)) 

# There are still 56,160 NA's even after removing the columns with all NA's i.e. holidays. 
# Also, these NA's can be interpreted as the leave taken by an employee was on that particular day. 
# So the next step will be to count the number of NA's recorded on each individual employee ID as these count will actually be the nukber of leaves taken by an employee

num_leaves <- apply(is.na(emp_wrk_hrs_final), 1, sum)
sum(num_leaves) # All Na's are now converted into leaves

### Creating a new data frame emp_wrd_data which has two variable: average hours worked by an employee and num_leaves taken by employee 
emp_wrk_data <- cbind(in_time[1], round(rowMeans(emp_wrk_hrs_final[-1], na.rm = TRUE),0))
emp_wrk_data <- cbind(emp_wrk_data, num_leaves)
colnames(emp_wrk_data)[2] <- "Average Hours worked by employee"

### Collating all the data together in one single file
length(unique(emp_survey_data$EmployeeID))
length(unique(general_data$EmployeeID))
length(unique(manager_survey_data$EmployeeID))
length(unique(emp_wrk_data$X))

setdiff(general_data$EmployeeID, emp_survey_data$EmployeeID)
setdiff(general_data$EmployeeID, manager_survey_data$EmployeeID)
setdiff(general_data$EmployeeID, emp_wrk_data$X)

hr_merge <- merge(general_data, emp_survey_data, by.x = "EmployeeID", by.y = "EmployeeID", all = FALSE)
hr_merge <- merge(hr_merge, manager_survey_data, by.x = "EmployeeID", by.y = "EmployeeID", all = FALSE)
hr_merge <- merge(hr_merge, emp_wrk_data, by.x = "EmployeeID", by.y = "X", all = FALSE)
View(hr_merge)

sapply(hr_merge, function(x) sum(is.na(x)))


### So, as we see, the NA's are found at following columns :
# 1) NumCompanies Worked - 19 
# 2) Total Working years - 9
# 3) Environment Satisfaction - 25
# 4) Job Satisfaction - 20
# 5) Work Life Balance - 38

sum(is.na(hr_merge)) # Hence, the total NA's accounts to 111. 
sum(is.na(hr_merge))/nrow(hr_merge) * 100 # Since removing 111 items from the data frame will be equivalent to removing around 2.5% of the data

# Hence we will try missing value imputation method for some columns, where it is reasonable to replace the missing values with the mean or median.
# Looking at the columns, it appears that missing value (NA's) in 3 variable columns i.e. Environment Satisfaction, Job Satisfaction and Work Life balance, can be replaced by their median
# Also, doing so will not really exploit the data to any large extent. 

### Replacing NA's with median value for following three columns:
typeof(hr_merge$EnvironmentSatisfaction)
typeof(hr_merge$JobSatisfaction) 
typeof(hr_merge$WorkLifeBalance)

hr_merge[which(is.na(hr_merge$EnvironmentSatisfaction)), "EnvironmentSatisfaction"] <- median(hr_merge$EnvironmentSatisfaction, na.rm = TRUE)
hr_merge[which(is.na(hr_merge$JobSatisfaction)), "JobSatisfaction"] <- median(hr_merge$JobSatisfaction, na.rm = TRUE)
hr_merge[which(is.na(hr_merge$WorkLifeBalance)), "WorkLifeBalance"] <- median(hr_merge$WorkLifeBalance, na.rm = TRUE)

# Counting the remaining NA values
sum(is.na(hr_merge)) 


# After replacing the majority of the values, we see that there are only 28 NA values left in the data set. 
# Tampering these NA's will exploit the data set. Also, the remaining NA's constitute around 0.6% of the dataset. 
# Hence, the best move will be to straight away omit the NA's instead of interpreting it.

# Removing the remaining NA's from the data set
hr_merge <- na.omit(hr_merge)
sum(is.na(hr_merge)) # All missing values treated.
View(hr_merge)

# Verifying for duplication, though it is not really needed.
sum(duplicated(hr_merge)) # No duplication found

str(hr_merge)

### Some columns are loaded as integers instead of factors. Fixing that columns.
hr_merge$Education <- as.factor(hr_merge$Education)
hr_merge$PerformanceRating <- as.factor(hr_merge$PerformanceRating)
hr_merge$EnvironmentSatisfaction <- as.factor(hr_merge$EnvironmentSatisfaction)
hr_merge$JobSatisfaction <- as.factor(hr_merge$JobSatisfaction)
hr_merge$WorkLifeBalance <- as.factor(hr_merge$WorkLifeBalance)
hr_merge$JobInvolvement <- as.factor(hr_merge$JobInvolvement)
hr_merge$JobLevel <- as.factor(hr_merge$JobLevel)
hr_merge$StockOptionLevel <- as.factor(hr_merge$StockOptionLevel)

### We can see in the dataset that we have two columns from which we can derive a useful variable. They are:
# 1) Standard hours
# 2) Average hours worked by employee which we derived from in_time and out_time dataset.
### Calculating the difference between these two columns can lead us to another column which reflects whether an employee is working
# overtime, or undertime or is just completing the standard hours.
str(hr_merge)
hr_merge$emphrs <- factor(ifelse((hr_merge$`Average Hours worked by employee` - hr_merge$StandardHours) == 0, "Standard Hours", ifelse((hr_merge$`Average Hours worked by employee` - hr_merge$StandardHours) > 0, "Over Time", "Under Time")))
levels(hr_merge$emphrs)

# Having this derived variable, we can remove the variables from which the diff_avghrs_stdhrs was calculated i.e.
# Removing "Standard hours" and "Average Hours worked by emloyee columns"
# In addition to that, the following variable columns are also unwanted because of reasons stated below:
# 1) Over18 - Only one factor level
# 3) Employee count - Only one value i.e. 1
hr_merge <- subset(hr_merge, select = -c(Over18, StandardHours,EmployeeCount,`Average Hours worked by employee`))

### Assigning names to levels as given in the data dictionary
levels(hr_merge$Education) <- c('Below College','College','Bachelor', 'Master', 'Doctor')
levels(hr_merge$EnvironmentSatisfaction) <- c('Low', 'Medium', 'High','Very High')
levels(hr_merge$JobInvolvement) <- c('Low', 'Medium', 'High','Very High')
levels(hr_merge$JobSatisfaction) <- c('Low', 'Medium','High', 'Very High')
levels(hr_merge$WorkLifeBalance) <- c('Bad', 'Good', 'Better', 'Best')
levels(hr_merge$PerformanceRating) <- c('Excellent', 'Outstanding')

str(hr_merge)


#######################################################################################################################

########################################### Exploratory data analysis ###############################################################

#######################################################################################################################

### Barcharts for categorical features with stacked attrition information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                   legend.position="right")

### PLOT_1
plot_grid(ggplot(hr_merge, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()) + bar_theme1,
          ggplot(hr_merge, aes(x=Department,fill=Attrition))+ geom_bar(position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()) +bar_theme1,
          ggplot(hr_merge, aes(x=Gender,fill=Attrition))+ geom_bar(position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()) +bar_theme1,
          ggplot(hr_merge, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()) +bar_theme1,
          align = "h")

### PLOT_2
plot_grid(ggplot(hr_merge, aes(x=Education,fill=Attrition))+ geom_bar(position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()) +bar_theme1,
          ggplot(hr_merge, aes(x = EducationField, fill = Attrition)) + geom_bar(position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()) + bar_theme1,
          ggplot(hr_merge, aes(x = emphrs, fill = Attrition)) + geom_bar(position = "fill") + ylab("Percentage") + scale_y_continuous(labels = percent_format()) + bar_theme1,
          ggplot(hr_merge, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()) +bar_theme1,
          align = "h")

### PLOT_3
plot_grid(ggplot(hr_merge, aes(x=JobLevel,fill=Attrition))+ geom_bar(position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()) +bar_theme1,
          ggplot(hr_merge, aes(x=JobRole,fill=Attrition))+ geom_bar(position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()) +bar_theme1,
          ggplot(hr_merge, aes(x=PerformanceRating,fill=Attrition))+ geom_bar(position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()) +bar_theme1,
          align = "h")

### PLOT_4
plot_grid(ggplot(hr_merge, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()) +bar_theme1,
          ggplot(hr_merge, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(position = "fill")+ ylab("Percentage")+ scale_y_continuous(labels = percent_format()) +bar_theme1,
          ggplot(hr_merge, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()) +bar_theme1,
          ggplot(hr_merge, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()) +bar_theme1,
          align = "h")


### Histogram and Boxplots for numeric variables

# Setting up box theme
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(),
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(),
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

### PLOT_1
plot_grid((plot_grid(ggplot(hr_merge, aes(Age))+ geom_histogram(binwidth = 20),
          ggplot(hr_merge, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)),
(plot_grid(ggplot(hr_merge, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(hr_merge, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)),
(plot_grid(ggplot(hr_merge, aes(MonthlyIncome))+ geom_histogram(binwidth = 25000),
          ggplot(hr_merge, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)),
(plot_grid(ggplot(hr_merge, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 5),
          ggplot(hr_merge, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)),
align = "h")

### PLOT_2
plot_grid((plot_grid(ggplot(hr_merge, aes(PercentSalaryHike))+ geom_histogram(binwidth = 5),
          ggplot(hr_merge, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)),
(plot_grid(ggplot(hr_merge, aes(TotalWorkingYears))+ geom_histogram(binwidth = 5),
          ggplot(hr_merge, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)),
(plot_grid(ggplot(hr_merge, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 1),
          ggplot(hr_merge, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)),
(plot_grid(ggplot(hr_merge, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(hr_merge, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)),
align = "h")

### PLOT_3
plot_grid((plot_grid(ggplot(hr_merge, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 5),
          ggplot(hr_merge, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)),
(plot_grid(ggplot(hr_merge, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 5),
          ggplot(hr_merge, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)),
(plot_grid(ggplot(hr_merge, aes(num_leaves))+ geom_histogram(binwidth = 5),
          ggplot(hr_merge, aes(x="",y=num_leaves))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          align = "v",ncol = 1)),
align = "h")


### Also from the plots, we see that no major outliers exists in the data except some random variation which is quite explainable and within permissible limits.
# Hence, we do not need to worry much about outliers as it cannot distort our predictions in any significant manner.

### Histogram plots filled Attrition factor for continuous variable
plot_grid(ggplot(hr_merge, aes(x = Age, fill = Attrition)) + geom_histogram(binwidth = 10, position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()),
          ggplot(hr_merge, aes(x = DistanceFromHome, fill = Attrition)) + geom_histogram(binwidth = 7, position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()),
          ggplot(hr_merge, aes(x = NumCompaniesWorked, fill = Attrition)) + geom_histogram(binwidth = 4, position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()),
          ggplot(hr_merge, aes(x = TrainingTimesLastYear, fill = Attrition)) + geom_histogram(binwidth = 1, position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()), align = 'h')

plot_grid(ggplot(hr_merge, aes(x = num_leaves, fill = Attrition)) + geom_histogram(binwidth = 5, position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()),
          ggplot(hr_merge, aes(x = hr_merge$YearsSinceLastPromotion, fill = Attrition)) + geom_histogram(binwidth = 4, position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()),
          ggplot(hr_merge, aes(x = YearsWithCurrManager, fill = Attrition)) + geom_histogram(binwidth = 4, position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()),
          ggplot(hr_merge, aes(x = YearsAtCompany, fill = Attrition)) + geom_histogram(binwidth = 10, position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()),
          ggplot(hr_merge, aes(x = TotalWorkingYears, fill = Attrition)) + geom_histogram(binwidth = 10, position = "fill") + ylab("Percentage")+ scale_y_continuous(labels = percent_format()), 
          align = 'h')

### Please Note : The Insights derived from the EDA are stated in detail in the Presentation. Hence we thought not to 
### repeat that here. Also, the final Conclusion is nearly similar to the insights that we found from EDA. And hence
### we have mentioned the Conclusion at the end.

#######################################################################################################################

############################################### DATA PREPARATION ########################################################

########################################################################################################################

### We are left with the following data preparation tasks:
# 1) Feature Standardisation
# 2) Dummy Variables Creation
# 3) Splitting the data set into training and testing data.

# We will begin by dividing the data sets into features file and category/factors file.
str(hr_merge)
hr_features <- subset(hr_merge, select = c(2,6,13,14,15,17,18,19,20,21,27))
hr_factors <- subset(hr_merge, select = -c(2,6,13,14,15,17,18,19,20,21,27,1,3))
str(hr_features)
str(hr_factors)

# Normalising continuous features
hr_features <- data.frame(sapply(hr_features, function(x) scale(x)))

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(hr_factors, 
                            function(x) data.frame(model.matrix(~x-1,data =hr_factors))[,-1]))

### Merging the above prepared files to create a master file for model building
hr_master <- cbind(hr_features, dummies)
hr_master <- cbind(hr_merge$Attrition, hr_master) # Adding target variable i.e. Attrition to master frame.
colnames(hr_master)[1] <- "Attrition"

# converting target variable Attrition from No/Yes character to factorwith levels 0/1
hr_master$Attrition <- ifelse(hr_merge$Attrition == "Yes", 1, 0)

str(hr_master)
View(hr_master)


### Splitting the data between train and test dataset
set.seed(100)

indices = sample.split(hr_master$Attrition, SplitRatio = 0.7)

train <- hr_master[indices, ]
test <- hr_master[!(indices), ]

#######################################################################################################################

########################################### Logistic Regression - MODEL BUILDING ############################################################

#######################################################################################################################

# Initial Model
model_1 <- glm(Attrition ~ ., family = "binomial", data = train)

## Stepwise Selection
model_2 <- stepAIC(model_1, direction = "both")

summary(model_2)
sort(vif(model_2))

### From here, we will removing the variables on the basis of  multicollinearity through VIF check and significance through
### pvalues. The general thumb rule is: Remove the variables with high VIF and high pvalues first. Hence we will look
### at the combination of both, and will keep moving forward till all VIF's are settled to the same range.


# Excluding YearsAtCompany

model_3 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Education.xDoctor + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                 JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xMedium + 
                 JobInvolvement.xHigh + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                 family = "binomial", data = train)

summary(model_3)
sort(vif(model_3))

# Although EducationField.xLife.Sciences is significant, it has a very HIGH VIF value as compared to other variables. 
# Also taking a view from busniess side, this variable doesnot seem to help much even if its kept. 
# And Educationfield.Medical and Education field.Life sciences both have high VIF's, which is a strong indication of 
# their correlation. Hence we have to remove one.

# Excluding EducationField.xLife.Sciences.
model_4 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Education.xDoctor +
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                 JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xMedium + 
                 JobInvolvement.xHigh + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                 family = "binomial", data = train)

summary(model_4)
sort(vif(model_4))

# Excluding BusinessTravel.xTravel_Rarely
model_5 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor +
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                 JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xMedium + 
                 JobInvolvement.xHigh + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                 family = "binomial", data = train)

summary(model_5)
sort(vif(model_5))

# Excluding MaritalStatus.xMarried
model_6 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor +
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                 JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xBest + JobInvolvement.xMedium + 
                 JobInvolvement.xHigh + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                 family = "binomial", data = train)

summary(model_6)
sort(vif(model_6))

# Excluding WorkLifeBalance.xBest
model_7 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor +
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                 JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                 WorkLifeBalance.xBetter + JobInvolvement.xMedium + 
                 JobInvolvement.xHigh + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                 family = "binomial", data = train)

summary(model_7)
sort(vif(model_7))

### Looking at the VIF's of remaining variables, we find out that all the VIF values are very low and pretty much in the same range.
### Hence VIF will no more be a deciding factor for further removal of variables.
### Instead, now we will proceed only on the basis of significance by just taking pvalues into consideration.
### Reminding the rule again, Higher pvalue means that variable should be removed first.

# Excluding EducationField.xMedical
model_8 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor +
                 EducationField.xMarketing + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                 JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                 WorkLifeBalance.xBetter + JobInvolvement.xMedium + 
                 JobInvolvement.xHigh + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                 family = "binomial", data = train)

summary(model_8)


# Excluding JobLevel.x5
model_9 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor +
                 EducationField.xMarketing + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x2 + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                 JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                 WorkLifeBalance.xBetter + JobInvolvement.xMedium + 
                 JobInvolvement.xHigh + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
               family = "binomial", data = train)

summary(model_9)

# Excluding JobRole.xSales.Executive
model_10 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor +
                  EducationField.xMarketing + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobLevel.x2 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                  JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                  WorkLifeBalance.xBetter + JobInvolvement.xMedium + 
                  JobInvolvement.xHigh + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                  family = "binomial", data = train)

summary(model_10)

# Excluding EducationField.xOther
model_11 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor +
                  EducationField.xMarketing + EducationField.xTechnical.Degree + JobLevel.x2 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                  JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                  WorkLifeBalance.xBetter + JobInvolvement.xMedium + 
                  JobInvolvement.xHigh + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                family = "binomial", data = train)

summary(model_11)

# Excluding MonthlyIncome
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + Education.xDoctor +
                  EducationField.xMarketing + EducationField.xTechnical.Degree + JobLevel.x2 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                  JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                  WorkLifeBalance.xBetter + JobInvolvement.xMedium + 
                  JobInvolvement.xHigh + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                family = "binomial", data = train)

summary(model_12)

# Excluding Education.xDoctor
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  EducationField.xMarketing + EducationField.xTechnical.Degree + JobLevel.x2 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                  JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                  WorkLifeBalance.xBetter + JobInvolvement.xMedium + 
                  JobInvolvement.xHigh + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                family = "binomial", data = train)

summary(model_13)

# Excluding EducationField.xMarketing
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  EducationField.xTechnical.Degree + JobLevel.x2 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                  JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                  WorkLifeBalance.xBetter + JobInvolvement.xMedium + 
                  JobInvolvement.xHigh + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                  family = "binomial", data = train)

summary(model_14)

# Excluding JobLevel.x2
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  EducationField.xTechnical.Degree +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                  JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                  WorkLifeBalance.xBetter + JobInvolvement.xMedium + 
                  JobInvolvement.xHigh + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                  family = "binomial", data = train)

summary(model_15)

# Excluding EducationField.xTechnical.Degree
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                  JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                  WorkLifeBalance.xBetter + JobInvolvement.xMedium + 
                  JobInvolvement.xHigh + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                family = "binomial", data = train)

summary(model_16)

# Excluding JobRole.xResearch.Director
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                  JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                  WorkLifeBalance.xBetter + JobInvolvement.xMedium + 
                  JobInvolvement.xHigh + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                  family = "binomial", data = train)

summary(model_17)

# Excluding JobInvolvement.xMedium 
model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                  JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                  WorkLifeBalance.xBetter + 
                  JobInvolvement.xHigh + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                  family = "binomial", data = train)

summary(model_18)

# Excluding JobInvolvement.xHigh 
model_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                  JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                  WorkLifeBalance.xBetter + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                  family = "binomial", data = train)

summary(model_19)

# Excluding StockOptionLevel.x1 
model_20 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                  JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                  WorkLifeBalance.xBetter + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                  family = "binomial", data = train)

summary(model_20)

# Excluding WorkLifeBalance.xGood
model_21 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                  JobSatisfaction.xHigh + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                  family = "binomial", data = train)

summary(model_21)

# Excluding WorkLifeBalance.xBetter 
model_22 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                  JobSatisfaction.xHigh + JobSatisfaction.xVery.High + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                  family = "binomial", data = train)

summary(model_22)


# Excluding TrainingTimesLastYear 
model_23 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                  JobSatisfaction.xHigh + JobSatisfaction.xVery.High + emphrs.xStandard.Hours + emphrs.xUnder.Time, 
                  family = "binomial", data = train)

summary(model_23)

###### Final Model ########

final_model <- model_23


########################################################################################################################

############################################### Model Evaluation ##############################################################

#######################################################################################################################

# P.S. - Majority of the code from here is written in the same way as lecture example script, since
# we are yet to learn some other efficient way to calculate these statistics in R.

# predicted probabilities of Attrition = 1 on test data
test_pred <- predict(final_model, type = "response", newdata = test[,-1])
summary(test_pred)

test$probability <- test_pred

# Using the probability standard cutoff of 50%
test_predicted_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition == 1, "Yes", "No"))

table(test_actual_attrition, test_predicted_attrition)

# Trying lower cutoff i.e. 0.4
test_predicted_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition == 1, "Yes", "No"))

table(test_actual_attrition, test_predicted_attrition)

### Choosing optimal cutoff value

perform_fn <- function(cutoff)
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# Optimal cutoff
cutoff_optimal <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff_optimal

### COnfussion Matrix for different cutoff's probability

### CUTOFF - 50%
test_cutoff_attrition <- factor(ifelse(test_pred >= 0.5, "Yes", "No"))

confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

### CUTOFF - 40%
test_cutoff_attrition <- factor(ifelse(test_pred >= 0.4, "Yes", "No"))

confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

### CUTOFF - 30%
test_cutoff_attrition <- factor(ifelse(test_pred >= 0.3, "Yes", "No"))

confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")


# Choosing Optimal cutoff for final model
test_cutoff_attrition <- factor(ifelse(test_pred >= cutoff_optimal, "Yes", "No"))
cutoff_optimal

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
conf_final

# Accuracy
acc <- conf_final$overall[1]
acc

# Sensitivity
sens <- conf_final$byClass[1]
sens

# Specificity
spec <- conf_final$byClass[2]
spec

View(test)

##### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition =="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition =="Yes",1,0)

# KS Table Test
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


##### Lift and Gain Charts ######

# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

# Attrition decile
Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
Attrition_decile

# Lift Chart
lift_chart <- ggplot(Attrition_decile, aes(bucket, Cumlift)) + geom_line() + ylab("Lift")
lift_chart

# Gain Chart

gain_chart <- ggplot(Attrition_decile, aes(bucket, Gain)) + geom_line()
gain_chart


#######################################################################################################################

################################################# CONCLUSION #############################################################################

###############################################################################################################################

#1 An older employee is less likely to leave the company than a younger person.
#2 An experienced person has lesser chances of leaving the company. 
#3 Larger the number of companies a person has worked for in the past, more likely he is to leave this company as well. 
#4 An employee who has not been promoted for many years may leave the company. 
#5 More the years spent by an employee with the same/current manager, lesser are his chances of attrition.
#6 A person who travels frequently for the business work will probably resign soon.
#7 Out of all the Job Roles, the job role of manufacturing director observed the least attrition. 
#8 An unmarried person/employee is more likely to switch the company.
#9 In General, an employee who is satisfied with environment and Job will not leave the company.
#10 An employee who works just for standard hours or works under time is less likely to leave the company as compared to the ones who work over time.

