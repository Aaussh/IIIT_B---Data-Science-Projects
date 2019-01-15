## Set Working Directory

## Clear all the variables from R Environment
rm(list = ls())


# ## Packages
#install.packages("psych")
#install.packages('Information')
#install.packages('woe')
#install.packages("GGally")
#devtools::install_git("https://github.com/klarsen1/Information.git")
#devtools::install_git("https://github.com/prk327/AtConP.git")
#install.packages("scorecard")

## Libraries
library(psych)
library(ggplot2)
library(reshape2)
library(scales)
library(cowplot)
library(Information)
library(caret)
library(plyr)
library(woe)
library(AtConP)
library(stringr)
library(tidyr)
library(dplyr)
library(GGally)
library(woeBinning)
library(gmodels)
library(smbinning)
library(acepack)
library(htmlTable)
library(RSQLite)
library(caret)
library(caTools)
library(dummies)
library(MASS)
library(car)
library(neuralnet)
library(scorecard)



## Read Credit Bureau Data
# We want the data to take blank values as NA as it will be easier to treat all of them at once later!
creditBureau <- read.csv("Credit Bureau data.csv", header = TRUE, na.strings=c(""," ","NA"))

## First few and Last few rows of Credit Bureau data
head(creditBureau)
tail(creditBureau)

## Saving the column names of credit bureau data as a variable credcolumns
credcolumns <- colnames(creditBureau)
credcolumns

## Structure of Credit Bureau data
str(creditBureau)

## Summary of Credit Bureau Data
summary(creditBureau)

## Description of Credit Bureau Data
describe(creditBureau)

## Read Demographic Data
# We want the data to take blank values as NA as it will be easier to treat all of them at once later!
demographic <- read.csv("Demographic data.csv", header = TRUE, na.strings=c(""," ","NA"))

## First Few and Last Few rows of Demographic data
head(demographic)
tail(demographic)

## Saving the column names of credit bureau data as a variable credcolumns
democolumns <- colnames(demographic)
democolumns

## Structure of Demographic data
str(demographic)

## Summary of Demographic Data
summary(demographic)

## Description of Demographic Data
describe(demographic)

## Checking for duplication in Credit Bureau Data
dup_cred <- duplicated(creditBureau$Application.ID)
sum(dup_cred)
nrow(creditBureau)

# We find three duplicated entries in Credit Bureau Data.

## Removing duplicated data
creditBureau <- creditBureau[!dup_cred, ]
nrow(creditBureau)
str(creditBureau)

## Checking for duplication in Demographic Data
dup_demogs <- duplicated(demographic$Application.ID)
sum(dup_demogs)
nrow(demographic)

# We find three duplicated entries in Demographic Data as well.

## Removing duplicated data
demographic <- demographic[!dup_demogs, ]
nrow(demographic)
str(demographic)

# This checks whether the same entries are duplicated in Credit Bureau and Demographic Data, or not!
identical(dup_cred, dup_demogs)  #TRUE

## Merging both the datasets

# Checking if we can merge both data sets by taking Application ID as our primary column
identical(creditBureau$Application.ID, demographic$Application.ID)  # TRUE

# Since the Application ID's or both data sets are identical, hence, we can merge both the datasets
# taking the Application ID as our primary key column

master_v1 <- merge(creditBureau, demographic, by = "Application.ID")
nrow(master_v1)
str(master_v1)

# Since both data sets have Performance Tag column, we can drop one of the two columns, if they are identical.
identical(master_v1$Performance.Tag.x, master_v1$Performance.Tag.y)  # TRUE

# Since Performance.Tag.x is identical to Performance.Tag.y, we can drop that column
master_v1 <- master_v1[, -which(colnames(master_v1) == "Performance.Tag.x")]

# Changing the column name of Performance.Tag.y to Performance.Tag
colnames(master_v1)[which(colnames(master_v1) == "Performance.Tag.y")] <- "Performance.Tag"

## Although we have added the command to read the blank values as NA in the beginning itself, still we will be verifying
## if any other format of blank values have managed to enter the dataset
sapply(master_v1, function(x) sum(x == ''))
str(master_v1)

# And we can see that all blank/empty cells have been already converted into NA's

## So the next step is to fix these NA values first.

## Checking for NA's from all variables
sapply(master_v1, function(x) sum(is.na(x)))

### The Breakdown of NA's is as follows:

# Performance.Tag = 1425
# Avgas.CC.Utilization.in.last.12.months = 1058
# Presence.of.open.home.loan = 272
# Outstanding.Balance = 272
# No.of.dependents = 3
# No.of.trades.opened.in.last.6.months = 1
# Education = 119
# Profession = 14
# Marital.Status..at.the.time.of.application. = 6
# Type.of.residence = 8
# Gender = 2

# Total Num of NA's
sum(is.na(master_v1))   # 3180

## Looking from the business perspective, NA's in the Performance Tag can be interpreted as customers whose Performance is 
## not recorded because they did not have any i.e. They were just not granted loan at the first place or their loan application
## were rejected. And that is why their data of Performance is "Not Available". Now, getting a loan application rejected
## itself signals that the loan granting organization thought this customers would default in the near future, maybe because
## they might be having some clear red signals in their profile. So, in a way, these customers having NA in their performance
## Tag column can be assumed no better then the customers who defaulted i.e. having values "1" in the Performance Tag Column
## (Since 1 is the value of Default in our column). So instead of dropping the data of these customers, it can be helpful
## if we take them into account by replacing their NA values with the Default value i.e. 1. 
# By doing that, our model is prepared for such bad profiles as well, whose application were rejected because of some clear
# obvious bad signs in their profiles.

# Replacing the NA values with Default value i.e. "1" in Performance_Tag column
master_v1$Performance.Tag[which(is.na(master_v1$Performance.Tag))] = 1
sum(is.na(master_v1))  # 1755

## After fixing the NA values of Performance Tag Columns, we will count the number of rows having one or more NA's of
## other variables. And if the number is not significant enough, we will straight away drop them.

# Total rows in dataset = 71292
Total_rows <- nrow(master_v1)
Total_rows

# Num of rows having one or more NA
na_rows <- Total_rows - sum(complete.cases(master_v1))
na_rows  # 1208

# Percentage of NA rows = 1.69%
(na_rows/Total_rows)*100

# Hence, the rows having one or more NA's accounts to little above 1.5% of the Total Data set.
# That is a minimal amount of data, definitely not worth interpreteting, hence we will just drop that bad data.
master_v1 <- na.omit(master_v1)

which(is.na(master_v1))   # All NA's have been removed
sapply(master_v1, function(x) sum(x == ''))   # Also, the data set have no remaining blank values either
str(master_v1)

## Checking for Inavlid Values:

# Checking for negative values
sapply(master_v1, function(x) sum(x < 0))

# Counting num of -ve values, which should be invaliud
length(which(master_v1 < 0))  # 80

## So there are 80 -ve values, which are invalid, and the breakdown is as follows:

# Age -- 1
# Income -- 79

# Checking what are the negative values
master_v1$Age[which(master_v1$Age < 0)]
master_v1$Income[which(master_v1$Income < 0)]

# Since Age cannot be "-3" and Income cannot be "-0.5", Hence clearly that is a data quality issue
# Also, again, the data is not significant enough to treat it by taking some mean or median.
# So we will drop that bad entries and get going.

# Dropping all the values having negative Age or Income value
master_v1 <- master_v1[!(master_v1$Age < 0 | master_v1$Income < 0), ]

# Now, looking from the business perspective, we know that loan cannot be granted to someone whose age is less than 18.
# So all entries of customer, whose Age is less than 18 are considered as malicious entries, and we need to drop that.

# Checking for Age < 18
unique(master_v1$Age[which(master_v1$Age < 18)])
sum(master_v1$Age < 18)  # 55 entries

# There are 55 such entries with following unique values: 0,15,16,17

# Dropping all these 55 entries
master_v1 <- master_v1[!(master_v1$Age < 18), ]

describe(master_v1)

# Looking at the describe(data), we see that the max value of the variable; "No_of_times_30_DPD_or_worse_in_last_6_months"
# is 7, whereas it cannot exceed 6 i.e. [30,60,90,120,150,180]. Hence, that is clearly an error.
# However, instead of dropping that, it is more advisable to cap that variable to 6.

# Max value of No.of.times.30.DPD.or.worse.in.last.6.months
max(master_v1$No.of.times.30.DPD.or.worse.in.last.6.months) # 7
sum(master_v1$No.of.times.30.DPD.or.worse.in.last.6.months > 6) # 16 such invalid values

# Capping those values to 6
master_v1$No.of.times.30.DPD.or.worse.in.last.6.months[which(master_v1$No.of.times.30.DPD.or.worse.in.last.6.months > 6)] = 6

str(master_v1)
describe(master_v1)
summary(master_v1)

# And we are done here with basic data quality checks. Moving on to EDA now!
# However, before that, we will be dividing the merged data frame back to demographic and credit data,
# And will be doing individual data analysis on both the divided datasets


demo_data <- master_v1[, colnames(master_v1) %in% democolumns]
credit_data <- master_v1[, colnames(master_v1) %in% credcolumns]

##############################################################################
#************************* DEMOGRAPHIC ANALYSIS ******************************
##############################################################################

# Dimensions of Data
dim(demo_data)  ## 69949 obs, 12 variables
names(demo_data)

# Structure of Data
str(demo_data)

# Rename the lengthy cloumn 

colnames(demo_data)[4] <- "Maritalstatus"
colnames(demo_data)[5] <- "NoOfDep"
colnames(demo_data)[9] <- "TypeOfResi"
colnames(demo_data)[10] <- "NoOfMthsInCurResi"
colnames(demo_data)[11] <- "NoOfMthsInCurJob"


# Change the columns into factor 
demo_data$Gender <- as.factor(demo_data$Gender)
demo_data$Maritalstatus <- as.factor(demo_data$Maritalstatus)
demo_data$Education <-  as.factor(demo_data$Education)
demo_data$Profession <- as.factor(demo_data$Profession)
demo_data$TypeOfResi <- as.factor(demo_data$TypeOfResi)
demo_data$Performance.Tag <- as.factor(demo_data$Performance.Tag)
str(demo_data)


# ============================================================================
########## Univariate Analysis ###############################################
# ============================================================================


Plot_A = ggplot(demo_data %>% group_by(Gender) %>% summarise(Count = n())) + 
  geom_bar(aes(Gender, Count), stat = "identity", fill = "deepskyblue2") +
  xlab("") +
  geom_label(aes(Gender, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Gender")

Plot_A

Plot_B = ggplot(demo_data %>% group_by(Maritalstatus) %>% summarise(Count = n())) + 
  geom_bar(aes(Maritalstatus, Count), stat = "identity", fill = "deepskyblue2") +
  xlab("") +
  geom_label(aes(Maritalstatus, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Maritalstatus")

Plot_B

Plot_C = ggplot(demo_data %>% group_by(Education) %>% summarise(Count = n())) + 
  geom_bar(aes(Education, Count), stat = "identity", fill = "deepskyblue2") +
  xlab("") +
  geom_label(aes(Education, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Education")

Plot_C

Plot_D = ggplot(demo_data %>% group_by(Profession) %>% summarise(Count = n())) + 
  geom_bar(aes(Profession, Count), stat = "identity", fill = "deepskyblue2") +
  xlab("") +
  geom_label(aes(Profession, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Profession")

Plot_D

plot_grid(Plot_A, Plot_B,Plot_C,Plot_D)
str(demo_data)

# Boxplots of Continuous Variables

melt_demo_data <- melt(demo_data[-c(1,3,4,7:9)], id.var = "Performance.Tag")

ggplot(data = melt_demo_data, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Performance.Tag)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  +
  ggtitle('Boxplots of Demographic Continuous Variables')


# ============================================================================
############## Bivariate Analysis ############################################
# ============================================================================


# Filter the defaulter data
default_demodata <- filter(demo_data, Performance.Tag == 1)

# data enrichment from Month to Year for two variables
default_demodata$NoOfMthsInCurResi <- round(default_demodata$NoOfMthsInCurResi/12,0)
default_demodata$NoOfMthsInCurJob <- round(default_demodata$NoOfMthsInCurJob/12,0)

# Plot 1
# Barcharts for 
# categorical variables
bar_theme <- theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) 

plot_grid(ggplot(default_demodata, aes(x=Performance.Tag,fill=Gender)) 
          + geom_bar(position="fill") + labs(x="Gender", y="Percentage") + scale_y_continuous( labels=function(x)x*100) + bar_theme,
          ggplot(default_demodata, aes(x=Performance.Tag,fill=Maritalstatus)) 
          + geom_bar(position="fill")+ labs(x="Marital Status",  y="Percentage") + scale_y_continuous( labels=function(x)x*100) + bar_theme,
          ggplot(default_demodata, aes(x=Performance.Tag,fill=Profession)) 
          + geom_bar(position="fill")+ labs(x="Profession", y="Percentage") + scale_y_continuous( labels=function(x)x*100) + bar_theme,
          ggplot(default_demodata, aes(x=Performance.Tag,fill=Education)) 
          + geom_bar(position="fill")+ labs(x="Education", y="Percentage") + scale_y_continuous( labels=function(x)x*100) + bar_theme,
          ggplot(default_demodata, aes(x=Performance.Tag,fill=TypeOfResi)) 
          + geom_bar(position="fill")+ labs(x="Type of Residence", y="Percentage") + scale_y_continuous( labels=function(x)x*100) + bar_theme + 
          ggtitle('Category wise distribution of Defaulters'), align="h")

########### Observation :

# Male geneder is more in percentage compared to female
# 20% of them is single and rest of them married in category of martial status
# SAL profession percentage is more compared to other two profession "SE" , "SE_PROF"
# Phd and others are low percent defaulters
# 75% of crowd rented compared to "Living with parents " and "company provided"

# ============================================================================
# Plot 2
# Histogram and Boxplots 
# Numeric variables
hist_theme <-  scale_fill_gradient("Legend",low = "black", high = "grey")

default_demodata$NoOfDep <- as.numeric(default_demodata$NoOfDep)
plot_grid(ggplot(default_demodata, aes(Age, fill=..count..)) + geom_histogram(binwidth = 12) + hist_theme,
          ggplot(default_demodata, aes(Income, fill=..count..)) + geom_histogram(binwidth=12) + hist_theme,
          ggplot(default_demodata, aes(NoOfMthsInCurResi, fill=..count..)) + geom_histogram(binwidth=1) + 
            labs(x = 'NoOfYrsInCurResi', y = 'count') + hist_theme + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)),
          ggplot(default_demodata, aes(NoOfMthsInCurJob, fill=..count..)) + 
            labs(x = 'NoOfYrsInCurJob', y = 'count') + geom_histogram(binwidth=1) + hist_theme,    
          ggplot(default_demodata, aes(NoOfDep, fill=..count..)) + geom_histogram(binwidth=1) + hist_theme,    
          align = "v",nrow = 3)


############ Observation: 
# Age group btw 40 to 50  are having more default
# Higher income are less defaulter
# shorter duration are high defaulters
# Those worked for shorter duration are with high defaulter
# Those having num of dependents = 3 are the high defaulters


##############################################################################
#************************* Credit Bureau data ANALYSIS ***********************
##############################################################################
credit_data <- master_v1[, colnames(master_v1) %in% credcolumns]
dim(credit_data)  #69949 obs of 19 variables

names(credit_data)

# Structure of Data
str(credit_data)

# Column Name - rename : credit_data
colnames(credit_data)[2] <- "no90DPD6Months"
colnames(credit_data)[3] <- "no60DPD6Months"
colnames(credit_data)[4] <- "no30DPD6Months"
colnames(credit_data)[5] <- "no90DPD12Months"
colnames(credit_data)[6] <- "no60DPD12Months"
colnames(credit_data)[7] <- "no30DPD12Months"
colnames(credit_data)[8] <- "AvgCCin12Months"
colnames(credit_data)[9] <- "Tradesin6Months"
colnames(credit_data)[10] <- "Tradesin12Months"
colnames(credit_data)[11] <- "PLTradesin6Months"
colnames(credit_data)[12] <- "PLTradesin12Months"
colnames(credit_data)[13] <- "Inq6Months"
colnames(credit_data)[14] <- "Inq12Months"
colnames(credit_data)[15] <- "OpenHomeLoan"
colnames(credit_data)[17] <- "TotalNoOfTrades"
colnames(credit_data)[18] <- "OpenAutoLoan"

# Change numeric to factor
summary(credit_data)

# ============================================================================
############# Correlation among all the numeric variables ###################
# ============================================================================

## Correlation MAtrix
demonumericVariables <- c("Age","NoOfDep", "Income","NoOfMthsInCurResi", "NoOfMthsInCurJob")
demo_numeric <- demo_data[, append('Application.ID', demonumericVariables)]
correlation_data <- merge(demo_numeric, credit_data, by = 'Application.ID')
correlation_data$Performance.Tag <- as.factor(correlation_data$Performance.Tag)
ggcorr(correlation_data[2:23], label = TRUE, label_alpha = TRUE) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1))
#ggpairs(correlation_data, columns = 2:23, mapping = ggplot2::aes(color = Performance.Tag))

## Boxplots of Continuous Variables
melt_credit_data <- melt(credit_data[-c(1,8,16)], id.var = "Performance.Tag")

ggplot(data = melt_credit_data, aes(x=variable, y=value)) + geom_boxplot(aes(fill=as.factor(Performance.Tag))) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  + ggtitle('Boxplots of Credit Bureau Continuous Variables')

BoxPlot_A <- ggplot(data = credit_data, aes(x=colnames(credit_data)[8], y=credit_data[,8])) + geom_boxplot(aes(fill=as.factor(Performance.Tag))) + 
  labs(x = 'Variable', y = 'Value')

BoxPlot_B <- ggplot(data = credit_data, aes(x=colnames(credit_data)[16], y=credit_data[,16])) + geom_boxplot(aes(fill=as.factor(Performance.Tag))) + 
  labs(x = 'Variable', y = 'Value')


plot_grid(BoxPlot_A, BoxPlot_B)

credit_data$no90DPD6Months <- as.factor(credit_data$no90DPD6Months)
credit_data$no60DPD6Months <- as.factor(credit_data$no60DPD6Months)
credit_data$no30DPD6Months <- as.factor(credit_data$no30DPD6Months)
credit_data$no90DPD12Months <- as.factor(credit_data$no90DPD12Months)
credit_data$no60DPD12Months <- as.factor(credit_data$no60DPD12Months)
credit_data$no30DPD12Months <- as.factor(credit_data$no30DPD12Months)
credit_data$OpenHomeLoan <- as.factor(credit_data$OpenHomeLoan)
credit_data$OpenAutoLoan <- as.factor(credit_data$OpenAutoLoan)
credit_data$Performance.Tag <- as.factor(credit_data$Performance.Tag)

summary(credit_data)

# ============================================================================
########## Univariate Analysis ###############################################
# ============================================================================

##SECTION A #########

# plot for no90DPD6Months
Plot_A = ggplot(credit_data %>% group_by(no90DPD6Months) %>% summarise(Count = n())) + 
  geom_bar(aes(no90DPD6Months, Count), stat = "identity", fill = "gold4") +
  xlab("") +
  geom_label(aes(no90DPD6Months, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("no90DPD6Months")

Plot_A

Plot_B = ggplot(credit_data %>% group_by(no60DPD6Months) %>% summarise(Count = n())) + 
  geom_bar(aes(no60DPD6Months, Count), stat = "identity", fill = "gold4") +
  xlab("") +
  geom_label(aes(no60DPD6Months, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("no60DPD6Months")

Plot_B

Plot_C = ggplot(credit_data %>% group_by(no30DPD6Months) %>% summarise(Count = n())) + 
  geom_bar(aes(no30DPD6Months, Count), stat = "identity", fill = "gold4") +
  xlab("") +
  geom_label(aes(no30DPD6Months, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("no30DPD6Months")
Plot_C

Plot_D = ggplot(credit_data %>% group_by(no60DPD12Months) %>% summarise(Count = n())) + 
  geom_bar(aes(no60DPD12Months, Count), stat = "identity", fill = "gold4") +
  xlab("") +
  geom_label(aes(no60DPD12Months, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("no60DPD12Months")
Plot_D

Plot_E = ggplot(credit_data %>% group_by(no60DPD12Months) %>% summarise(Count = n())) + 
  geom_bar(aes(no60DPD12Months, Count), stat = "identity", fill = "gold4") +
  xlab("") +
  geom_label(aes(no60DPD12Months, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("no60DPD12Months")
Plot_E

Plot_F = ggplot(credit_data %>% group_by(no30DPD12Months) %>% summarise(Count = n())) + 
  geom_bar(aes(no30DPD12Months, Count), stat = "identity", fill = "gold4") +
  xlab("") +
  geom_label(aes(no30DPD12Months, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("no30DPD12Months")

Plot_F

plot_grid(Plot_A, Plot_B,Plot_C,Plot_D,Plot_E,Plot_F)

##SECTION B #########

Plot_G = ggplot(credit_data %>% group_by(Tradesin6Months) %>% summarise(Count = n())) + 
  geom_bar(aes(Tradesin6Months, Count), stat = "identity", fill = "gold4") +
  xlab("") +
  geom_label(aes(Tradesin6Months, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Tradesin6Months")
Plot_G 


Plot_H = ggplot(credit_data %>% group_by(Tradesin12Months) %>% summarise(Count = n())) + 
  geom_bar(aes(Tradesin12Months, Count), stat = "identity", fill = "gold4") +
  xlab("") +
  geom_label(aes(Tradesin12Months, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Tradesin12Months")

Plot_H

Plot_I = ggplot(credit_data %>% group_by(PLTradesin6Months) %>% summarise(Count = n())) + 
  geom_bar(aes(PLTradesin6Months, Count), stat = "identity", fill = "gold4") +
  xlab("") +
  geom_label(aes(PLTradesin6Months, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("PLTradesin6Months")
Plot_I 


Plot_J = ggplot(credit_data %>% group_by(PLTradesin12Months) %>% summarise(Count = n())) + 
  geom_bar(aes(PLTradesin12Months, Count), stat = "identity", fill = "gold4") +
  xlab("") +
  geom_label(aes(PLTradesin12Months, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("PLTradesin12Months")

Plot_J


Plot_K = ggplot(credit_data %>% group_by(Inq6Months) %>% summarise(Count = n())) + 
  geom_bar(aes(Inq6Months, Count), stat = "identity", fill = "gold4") +
  xlab("") +
  geom_label(aes(Inq6Months, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Inq6Months")
Plot_K 


Plot_L = ggplot(credit_data %>% group_by(Inq12Months) %>% summarise(Count = n())) + 
  geom_bar(aes(Inq12Months, Count), stat = "identity", fill = "gold4") +
  xlab("") +
  geom_label(aes(Inq12Months, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Inq12Months")
Plot_L


plot_grid (Plot_G,Plot_H,Plot_I,Plot_J,Plot_K,Plot_L)

str(credit_data)


# ============================================================================
############## Bivariate Analysis ############################################
# ============================================================================

hist_theme <-  scale_fill_gradient("Legend",low = "black", high = "blue")
bar_theme <- theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) 
creditdata_default <- filter(credit_data, Performance.Tag==1)

# 90DPD, 60DPD, 30DPD are strong indicators of default
plot_grid(ggplot(credit_data, aes(no90DPD6Months, fill=Performance.Tag)) + geom_bar(position="fill") + labs(x="no90DPD6Months", y="Percentage") + 
            scale_y_continuous(labels=function(x)x*100) + bar_theme,
          ggplot(credit_data, aes(no60DPD6Months, fill=Performance.Tag)) + geom_bar(position="fill") + labs(x="no60DPD6Months", y="Percentage") + 
            scale_y_continuous( labels=function(x)x*100) + bar_theme,
          ggplot(credit_data, aes(no30DPD6Months, fill=Performance.Tag)) + geom_bar(position="fill") + labs(x="no30DPD6Months", y="Percentage") + 
            scale_y_continuous( labels=function(x)x*100) + bar_theme,
          ggplot(credit_data, aes(no90DPD12Months, fill=Performance.Tag)) + geom_bar(position="fill") + labs(x="no90DPD12Months", y="Percentage") + 
            scale_y_continuous( labels=function(x)x*100) + bar_theme,
          ggplot(credit_data, aes(no60DPD12Months, fill=Performance.Tag)) + geom_bar(position="fill") + labs(x="no60DPD12Months", y="Percentage") 
          + scale_y_continuous( labels=function(x)x*100) + bar_theme,
          ggplot(credit_data, aes(no30DPD12Months, fill=Performance.Tag)) + geom_bar(position="fill") + labs(x="no30DPD12Months", y="Percentage") + 
            scale_y_continuous( labels=function(x)x*100) + bar_theme,
          align = "v",nrow = 2)


# Histogram distribution of Defaulters for Continuous variables 
# AvgCCin12Months,Tradesin6Months, Tradesin12Months, PLTradesin6Months, PLTradesin12Months, Inq6Months
plot_grid(ggplot(creditdata_default, aes(AvgCCin12Months, fill=..count..)) + geom_histogram(binwidth = 10) + hist_theme,
          ggplot(creditdata_default, aes(Tradesin6Months, fill=..count..)) + geom_histogram(binwidth=1) + hist_theme,
          ggplot(creditdata_default, aes(Tradesin12Months, fill=..count..)) + geom_histogram(binwidth=5) + hist_theme + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)),
          ggplot(creditdata_default, aes(PLTradesin6Months, fill=..count..)) + geom_histogram(binwidth=1) + hist_theme,    
          ggplot(creditdata_default, aes(PLTradesin12Months, fill=..count..)) + geom_histogram(binwidth=1) + hist_theme, 
          ggplot(creditdata_default, aes(Inq6Months, fill=..count..)) + geom_histogram(binwidth=1) + hist_theme, 
          align = "v",nrow = 2)



# # AvgCCin12Months,Tradesin6Months, Tradesin12Months, PLTradesin6Months, PLTradesin12Months, Inq6Months
# plot_grid(ggplot(creditdata_default, aes(Inq12Months, fill=..count..)) + geom_histogram(binwidth = 10) + hist_theme,
#           ggplot(creditdata_default, aes(Outstanding.Balance, fill=..count..)) + geom_histogram(binwidth=1) + hist_theme,
#           ggplot(creditdata_default, aes(TotalNoOfTrades, fill=..count..)) + geom_histogram(binwidth=5) + hist_theme,
#           align = "v",nrow = 2)
# 
# colnames(creditdata_default)
# 
# creditdata_default$AvgCCin12Months <- as.factor(creditdata_default$AvgCCin12Months)
# creditdata_default$Tradesin6Months <- as.factor(creditdata_default$Tradesin6Months)
# creditdata_default$Tradesin12Months <- as.factor(creditdata_default$Tradesin12Months)
# creditdata_default$PLTradesin6Months <- as.factor(creditdata_default$PLTradesin6Months)
# creditdata_default$PLTradesin12Months <- as.factor(creditdata_default$PLTradesin12Months)
# creditdata_default$Inq6Months <- as.factor(creditdata_default$Inq6Months)
# creditdata_default$Inq12Months <- as.factor(creditdata_default$Inq12Months)
# creditdata_default$TotalNoOfTrades <- as.factor(creditdata_default$TotalNoOfTrades)
# 

plot_grid(ggplot(credit_data, aes(as.factor(AvgCCin12Months), fill=Performance.Tag)) + geom_bar(position="fill")  
          + scale_y_continuous(labels=function(x)x*100) + bar_theme + labs(x = 'AvgCCin12Months' , y = 'Percentage'),
          ggplot(credit_data, aes(as.factor(Tradesin6Months), fill=Performance.Tag)) + geom_bar(position="fill")
          + scale_y_continuous(labels=function(x)x*100) + bar_theme + labs(x = 'Tradesin6Months' , y = 'Percentage'),
          ggplot(credit_data, aes(as.factor(Tradesin12Months), fill=Performance.Tag)) + geom_bar(position="fill")
          + scale_y_continuous(labels=function(x)x*100) + bar_theme + labs(x = 'Tradesin12Months' , y = 'Percentage'),
          ggplot(credit_data, aes(as.factor(PLTradesin6Months), fill=Performance.Tag)) + geom_bar(position="fill")
          + scale_y_continuous(labels=function(x)x*100) + bar_theme + labs(x = 'PLTradesin6Months' , y = 'Percentage'),
          ggplot(credit_data, aes(as.factor(PLTradesin12Months), fill=Performance.Tag)) + geom_bar(position="fill")
          + scale_y_continuous(labels=function(x)x*100) + bar_theme + labs(x = 'PLTradesin12Months' , y = 'Percentage'),
          ggplot(credit_data, aes(as.factor(Inq6Months), fill=Performance.Tag)) + geom_bar(position="fill")
          + scale_y_continuous(labels=function(x)x*100) + bar_theme + labs(x = 'Inq6Months' , y = 'Percentage'),
          ggplot(credit_data, aes(as.factor(Inq12Months), fill=Performance.Tag)) + geom_bar(position="fill")
          + scale_y_continuous(labels=function(x)x*100) + bar_theme + labs(x = 'Inq12Months' , y = 'Percentage'),
          ggplot(credit_data, aes(as.factor(TotalNoOfTrades), fill=Performance.Tag)) + geom_bar(position="fill")
          + scale_y_continuous(labels=function(x)x*100) + bar_theme + labs(x = 'TotalNoOfTrades' , y = 'Percentage'),
          align = "v",nrow = 4)




plot_grid(ggplot(credit_data, aes(as.factor(OpenAutoLoan), fill=Performance.Tag)) + geom_bar(position="fill")
          + scale_y_continuous(labels=function(x)x*100) + bar_theme + labs(x = 'OpenAutoLoan' , y = 'Percentage'),
          ggplot(credit_data, aes(as.factor(OpenHomeLoan), fill=Performance.Tag)) + geom_bar(position="fill")
          + scale_y_continuous(labels=function(x)x*100) + bar_theme + labs(x = 'OpenHomeLoan' , y = 'Percentage'),
          ggplot(creditdata_default, aes(Outstanding.Balance, fill=..count..)) + geom_histogram(binwidth = 50000) + hist_theme,
          align = "v",nrow = 3)
          



# ============================================================================
######  DEMOGRAPHIC- WOE and Information Value(IV) analysis  ################
# ============================================================================

str(master_v1)

#Creating Information value table
Info_table <- create_infotables(data = master_v1[-1],y = "Performance.Tag",parallel = TRUE)

head(Info_table$Summary)
Info_table$Summary

## Plotting Info_Tables
names <- names(Info_table$Tables)
names
# plots <- list()
# for (i in 1:length(names)){
#   plots[[i]] <- plot_infotables(Info_table, names[i])
# }

plot_grid(plot_infotables(Info_table, names[1], show_values = TRUE),
          plot_infotables(Info_table, names[2], show_values = TRUE),
          plot_infotables(Info_table, names[3], show_values = TRUE),
          plot_infotables(Info_table, names[4], show_values = TRUE),
          plot_infotables(Info_table, names[5], show_values = TRUE),
          plot_infotables(Info_table, names[6], show_values = TRUE),
          align = "h",nrow = 2)

plot_grid(plot_infotables(Info_table, names[7], show_values = TRUE),
          plot_infotables(Info_table, names[8], show_values = TRUE),
          plot_infotables(Info_table, names[9], show_values = TRUE),
          plot_infotables(Info_table, names[10], show_values = TRUE),
          plot_infotables(Info_table, names[11], show_values = TRUE),
          plot_infotables(Info_table, names[12], show_values = TRUE),
          align = "h",nrow = 2)

plot_grid(plot_infotables(Info_table, names[13], show_values = TRUE),
          plot_infotables(Info_table, names[14], show_values = TRUE),
          plot_infotables(Info_table, names[15], show_values = TRUE),
          plot_infotables(Info_table, names[16], show_values = TRUE),
          plot_infotables(Info_table, names[17], show_values = TRUE),
          plot_infotables(Info_table, names[18], show_values = TRUE),
          align = "h",nrow = 2)

plot_grid(plot_infotables(Info_table, names[19], show_values = TRUE),
          plot_infotables(Info_table, names[20], show_values = TRUE),
          plot_infotables(Info_table, names[21], show_values = TRUE),
          plot_infotables(Info_table, names[22], show_values = TRUE),
          plot_infotables(Info_table, names[23], show_values = TRUE),
          plot_infotables(Info_table, names[24], show_values = TRUE),
          align = "h",nrow = 2)

plot_grid(plot_infotables(Info_table, names[25], show_values = TRUE),
          plot_infotables(Info_table, names[26], show_values = TRUE),
          plot_infotables(Info_table, names[27], show_values = TRUE),
          align = "v",nrow = 3)



head(Info_table)

## Taking a copy of both datasets before treating them further

credit_raw <- credit_data
demo_raw <- demo_data

str(credit_raw)
str(demo_raw)

### Treating Outliers:

# Rule of thumb: 
# Q1 - 1.5 * IQR
# Q3 + 1.5 * IQR 
## Sample Code

#########Credit_raw
#AvgCCin12Months
outlier_cutoff_high_AvgCC <- quantile(credit_raw$AvgCCin12Months, 0.75) + 1.5 * IQR(credit_raw$AvgCCin12Months)
outlier_cutoff_low_AvgCC <- quantile(credit_raw$AvgCCin12Months, 0.25) - 1.5 * IQR(credit_raw$AvgCCin12Months)
outlier_cutoff_high_AvgCC
outlier_cutoff_low_AvgCC

quantile(credit_raw$AvgCCin12Months, seq(0,1,0.01))
box_AvgCC <- boxplot(credit_raw$AvgCCin12Months)

# Clearly, there are some outliers, which needs to be capped
sum(credit_raw$AvgCCin12Months > outlier_cutoff_high_AvgCC) 
sum(credit_raw$AvgCCin12Months < outlier_cutoff_low_AvgCC)

## There are around 3473 outliers above the upper quartile. Since that is a significant amount, hence we cannot drop them,
## But we can treat it by capping them.

credit_raw$AvgCCin12Months[which(credit_raw$AvgCCin12Months > outlier_cutoff_high_AvgCC)] <- outlier_cutoff_high_AvgCC

#Tradesin6Months
outlier_cutoff_high_Tradesin6Months <- quantile(credit_raw$Tradesin6Months, 0.75) + 1.5 * IQR(credit_raw$Tradesin6Months)
outlier_cutoff_low_Tradesin6Months <- quantile(credit_raw$Tradesin6Months, 0.25) - 1.5 * IQR(credit_raw$Tradesin6Months)
outlier_cutoff_high_Tradesin6Months
outlier_cutoff_low_Tradesin6Months

quantile(credit_raw$Tradesin6Months, seq(0,1,0.01))
box_Tradesin6Months <- boxplot(credit_raw$Tradesin6Months)

# Clearly, there are some outliers, which needs to be capped
sum(credit_raw$Tradesin6Months > outlier_cutoff_high_Tradesin6Months) 
sum(credit_raw$Tradesin6Months < outlier_cutoff_low_Tradesin6Months)

## There are around 3731 outliers above the upper quartile. Since that is a significant amount, hence we cannot drop them,
## But we can treat it by capping them.

credit_raw$Tradesin6Months[which(credit_raw$Tradesin6Months > outlier_cutoff_high_Tradesin6Months)] <- outlier_cutoff_high_Tradesin6Months


## Since there are no outliers below the lower quartile, we do not need to treat them.

boxplot(credit_raw$Tradesin6Months)

#Tradesin12Months
outlier_cutoff_high_Tradesin12Months <- quantile(credit_raw$Tradesin12Months, 0.75) + 1.5 * IQR(credit_raw$Tradesin12Months)
outlier_cutoff_low_Tradesin12Months <- quantile(credit_raw$Tradesin12Months, 0.25) - 1.5 * IQR(credit_raw$Tradesin12Months)
outlier_cutoff_high_Tradesin12Months
outlier_cutoff_low_Tradesin12Months

quantile(credit_raw$Tradesin12Months, seq(0,1,0.01))
box_Tradesin12Months <- boxplot(credit_raw$Tradesin12Months)

# Clearly, there are some outliers, which needs to be capped
sum(credit_raw$Tradesin12Months > outlier_cutoff_high_Tradesin12Months) 
sum(credit_raw$Tradesin12Months < outlier_cutoff_low_Tradesin12Months)

credit_raw$Tradesin12Months[which(credit_raw$Tradesin12Months > outlier_cutoff_high_Tradesin12Months)] <- outlier_cutoff_high_Tradesin12Months


## Since there are no outliers below the lower quartile, we do not need to treat them.

boxplot(credit_raw$Tradesin12Months)

#PLTradesin6Months
outlier_cutoff_high_PLTradesin6Months <- quantile(credit_raw$PLTradesin6Months, 0.75) + 1.5 * IQR(credit_raw$PLTradesin6Months)
outlier_cutoff_low_PLTradesin6Months <- quantile(credit_raw$PLTradesin6Months, 0.25) - 1.5 * IQR(credit_raw$PLTradesin6Months)
outlier_cutoff_high_PLTradesin6Months
outlier_cutoff_low_PLTradesin6Months

quantile(credit_raw$PLTradesin6Months, seq(0,1,0.01))
box_PLTradesin6Months <- boxplot(credit_raw$PLTradesin6Months)

# Clearly, there are some outliers, which needs to be capped
sum(credit_raw$PLTradesin6Months > outlier_cutoff_high_PLTradesin6Months) 
sum(credit_raw$PLTradesin6Months < outlier_cutoff_low_PLTradesin6Months)

## There are around 295 outliers above the upper quartile. Since that is a moderaltely significant amount, hence we cannot drop them,
## But we can treat it by capping them.

credit_raw$PLTradesin6Months[which(credit_raw$PLTradesin6Months > outlier_cutoff_high_PLTradesin6Months)] <- outlier_cutoff_high_PLTradesin6Months


## Since there are no outliers below the lower quartile, we do not need to treat them.

boxplot(credit_raw$PLTradesin6Months)

#PLTradesin12Months
outlier_cutoff_high_PLTradesin12Months <- quantile(credit_raw$PLTradesin12Months, 0.75) + 1.5 * IQR(credit_raw$PLTradesin12Months)
outlier_cutoff_low_PLTradesin12Months <- quantile(credit_raw$PLTradesin6Months, 0.25) - 1.5 * IQR(credit_raw$PLTradesin12Months)
outlier_cutoff_high_PLTradesin12Months
outlier_cutoff_low_PLTradesin12Months

quantile(credit_raw$PLTradesin12Months, seq(0,1,0.01))
box_PLTradesin12Months <- boxplot(credit_raw$PLTradesin12Months)

# Clearly, there are some outliers, which needs to be capped
sum(credit_raw$PLTradesin12Months > outlier_cutoff_high_PLTradesin12Months) 
sum(credit_raw$PLTradesin12Months < outlier_cutoff_low_PLTradesin12Months)

## There are around 76 outliers above the upper quartile. Since that is moderately significant amount, hence we cannot drop them,
## But we can treat it by capping them.

credit_raw$PLTradesin12Months[which(credit_raw$PLTradesin12Months > outlier_cutoff_high_PLTradesin12Months)] <- outlier_cutoff_high_PLTradesin12Months


## Since there are no outliers below the lower quartile, we do not need to treat them.

boxplot(credit_raw$PLTradesin12Months)

#Inq6Months
outlier_cutoff_high_Inq6Months <- quantile(credit_raw$Inq6Months, 0.75) + 1.5 * IQR(credit_raw$Inq6Months)
outlier_cutoff_low_Inq6Months <- quantile(credit_raw$PLTradesin6Months, 0.25) - 1.5 * IQR(credit_raw$Inq6Months)
outlier_cutoff_high_Inq6Months
outlier_cutoff_low_Inq6Months

quantile(credit_raw$Inq6Months, seq(0,1,0.01))
box_Inq6Months <- boxplot(credit_raw$Inq6Months)

# Clearly, there are some outliers, which needs to be capped
sum(credit_raw$Inq6Months > outlier_cutoff_high_Inq6Months) 
sum(credit_raw$Inq6Months < outlier_cutoff_low_Inq6Months)

## There are around 1367 outliers above the upper quartile. Since that is a significant amount, hence we cannot drop them,
## But we can treat it by capping them.

credit_raw$Inq6Months[which(credit_raw$Inq6Months > outlier_cutoff_high_Inq6Months)] <- outlier_cutoff_high_Inq6Months


## Since there are no outliers below the lower quartile, we do not need to treat them.

boxplot(credit_raw$Inq6Months)

#Inq12Months
outlier_cutoff_high_Inq12Months <- quantile(credit_raw$Inq12Months, 0.75) + 1.5 * IQR(credit_raw$Inq12Months)
outlier_cutoff_low_Inq12Months <- quantile(credit_raw$PLTradesin12Months, 0.25) - 1.5 * IQR(credit_raw$Inq12Months)
outlier_cutoff_high_Inq12Months
outlier_cutoff_low_Inq12Months

quantile(credit_raw$Inq12Months, seq(0,1,0.01))
box_Inq12Months <- boxplot(credit_raw$Inq12Months)

# Clearly, there are some outliers, which needs to be capped
sum(credit_raw$Inq12Months > outlier_cutoff_high_Inq12Months) 
sum(credit_raw$Inq12Months < outlier_cutoff_low_Inq12Months)

## There are around 2055 outliers above the upper quartile. Since that is a significant amount, hence we cannot drop them,
## But we can treat it by capping them.

credit_raw$Inq12Months[which(credit_raw$Inq12Months > outlier_cutoff_high_Inq12Months)] <- outlier_cutoff_high_Inq12Months


## Since there are no outliers below the lower quartile, we do not need to treat them.

boxplot(credit_raw$Inq12Months)


#Outstanding.Balance
outlier_cutoff_high_Outstanding.Balance <- quantile(credit_raw$Outstanding.Balance, 0.75) + 1.5 * IQR(credit_raw$Outstanding.Balance)
outlier_cutoff_low_Outstanding.Balance <- quantile(credit_raw$Outstanding.Balance, 0.25) - 1.5 * IQR(credit_raw$Outstanding.Balance)
outlier_cutoff_high_Outstanding.Balance
outlier_cutoff_low_Outstanding.Balance

quantile(credit_raw$Outstanding.Balance, seq(0,1,0.01))
box_Outstanding.Balance <- boxplot(credit_raw$Outstanding.Balance)

# Clearly, there are no outliers

#TotalNoOfTrades
outlier_cutoff_high_TotalNoOfTrades <- quantile(credit_raw$TotalNoOfTrades, 0.75) + 1.5 * IQR(credit_raw$TotalNoOfTrades)
outlier_cutoff_low_TotalNoOfTrades <- quantile(credit_raw$TotalNoOfTrades, 0.25) - 1.5 * IQR(credit_raw$TotalNoOfTrades)
outlier_cutoff_high_TotalNoOfTrades
outlier_cutoff_low_TotalNoOfTrades

quantile(credit_raw$TotalNoOfTrades, seq(0,1,0.01))
box_TotalNoOfTrades <- boxplot(credit_raw$TotalNoOfTrades)

# Clearly, there are some outliers, which needs to be capped
sum(credit_raw$TotalNoOfTrades > outlier_cutoff_high_TotalNoOfTrades) 
sum(credit_raw$TotalNoOfTrades < outlier_cutoff_low_TotalNoOfTrades)

## There are around 6812 outliers above the upper quartile. Since that is a significant amount, hence we cannot drop them,
## But we can treat it by capping them.

credit_raw$TotalNoOfTrades[which(credit_raw$TotalNoOfTrades > outlier_cutoff_high_TotalNoOfTrades)] <- outlier_cutoff_high_TotalNoOfTrades


## Since there are no outliers below the lower quartile, we do not need to treat them.

boxplot(credit_raw$TotalNoOfTrades)

####demo_raw#####
#Age
outlier_cutoff_high_Age <- quantile(demo_raw$Age, 0.75) + 1.5 * IQR(demo_raw$Age)
outlier_cutoff_low_Age <- quantile(demo_raw$Age, 0.25) - 1.5 * IQR(demo_raw$Age)
outlier_cutoff_high_Age
outlier_cutoff_low_Age

quantile(demo_raw$Age, seq(0,1,0.01))
box_Age <- boxplot(demo_raw$Age)

# Clearly, there are no outliers

boxplot(demo_raw$NoOfDep) #there are no outliers
boxplot(demo_raw$Income) #there are no outliers
boxplot(demo_raw$NoOfMthsInCurResi) #there are no outliers

boxplot(demo_raw$NoOfMthsInCurJob)

#Clearly there are some outliers,which needs to be capped
outlier_cutoff_high_NoOfMthsInCurJob <- quantile(demo_raw$NoOfMthsInCurJob, 0.75) + 1.5 * IQR(demo_raw$NoOfMthsInCurJob)
outlier_cutoff_low_NoOfMthsInCurJob <- quantile(demo_raw$NoOfMthsInCurJob, 0.25) - 1.5 * IQR(demo_raw$NoOfMthsInCurJob)
outlier_cutoff_high_NoOfMthsInCurJob
outlier_cutoff_low_NoOfMthsInCurJob

quantile(demo_raw$NoOfMthsInCurJob, seq(0,1,0.01))
box_NoOfMthsInCurJob <- boxplot(demo_raw$NoOfMthsInCurJob)

sum(demo_raw$NoOfMthsInCurJob > outlier_cutoff_high_NoOfMthsInCurJob) 
sum(demo_raw$NoOfMthsInCurJob < outlier_cutoff_low_NoOfMthsInCurJob)

## There are around 6 outliers above the upper quartile. Instead of dropping them, we will just cap them

demo_raw$NoOfMthsInCurJob[which(demo_raw$NoOfMthsInCurJob > outlier_cutoff_high_NoOfMthsInCurJob)] <- outlier_cutoff_high_NoOfMthsInCurJob


## Since there are no outliers below the lower quartile, we do not need to treat them.
boxplot(demo_raw$NoOfMthsInCurJob)

#Creating the final datasets
demo_ds<-demo_raw
master_ds<-merge(demo_raw,credit_raw,by = 'Application.ID')
# Since both data sets have Performance Tag column, we can drop one of the two columns, if they are identical.
identical(master_ds$Performance.Tag.x, master_ds$Performance.Tag.y)  # TRUE

# Since Performance.Tag.x is identical to Performance.Tag.y, we can drop that column
master_ds <- master_ds[, -which(colnames(master_ds) == "Performance.Tag.x")]

# Changing the column name of Performance.Tag.y to Performance.Tag
colnames(master_ds)[which(colnames(master_ds) == "Performance.Tag.y")] <- "Performance.Tag"


master_ds$no90DPD6Months <- as.numeric(levels(master_ds$no90DPD6Months))[master_ds$no90DPD6Months]
master_ds$no90DPD12Months <- as.numeric(levels(master_ds$no90DPD12Months))[master_ds$no90DPD12Months]
master_ds$no60DPD6Months <- as.numeric(levels(master_ds$no60DPD6Months))[master_ds$no60DPD6Months]
master_ds$no60DPD12Months <- as.numeric(levels(master_ds$no60DPD12Months))[master_ds$no60DPD12Months]
master_ds$no30DPD6Months <- as.numeric(levels(master_ds$no30DPD6Months))[master_ds$no30DPD6Months]
master_ds$no30DPD12Months <- as.numeric(levels(master_ds$no30DPD12Months))[master_ds$no30DPD12Months]



str(demo_ds)
str(master_ds)

# Final Datasets for model building
demo_ds
master_ds

### Creating a woe interpreted demographic dataset

demo_woe <- demo_data

app_id_demo <- demo_woe[,1]
demo_woe <- demo_woe[-1]
str(demo_woe)

# Converting Dependent Column into numeric
demo_woe$Performance.Tag <- as.numeric(levels(demo_woe$Performance.Tag))[demo_woe$Performance.Tag]
demo_IV <- create_infotables(data = demo_woe, y = 'Performance.Tag', parallel = TRUE)
demo_woe <- DF.Replace.WOE(X = demo_woe, y = demo_IV, Dependent = 'Performance.Tag')
demo_woe <- cbind(app_id_demo, demo_woe)
colnames(demo_woe)[1] <- colnames(demo_data)[1]

str(demo_woe)


### Creating a woe interpreted master dataset

master_woe <- master_v1

str(master_woe)
app_id_master <- master_woe[,1]
master_woe <- master_woe[-1]

# Converting Dependent Column into numeric

master_IV <- create_infotables(data = master_woe, y = 'Performance.Tag', parallel = TRUE)
master_woe <- DF.Replace.WOE(X = master_woe, y = master_IV, Dependent = 'Performance.Tag')
master_woe <- cbind(app_id_master, master_woe)
colnames(master_woe)[1] <- colnames(master_v1)[1]

str(master_woe)

## Final WoE Interpreted Datasets for Model building
demo_woe
master_woe


###########################################################################################################################
### Tasks to be done:
###########################################################################################################################


############################################################################################################################
###MODEL EVALUATOR FUNCTION####
## We have applied the concept of encapsulation to evaluate the models by creating a model_evaluator() function.

### Why model_evaluator() function?
# Below we have created a model_evaluator() function to incorporate the concept of encapsulation and reduce code repeatence, 
# since the model evaluation code remains more or less the same for majority of the models

### Arguments for model_evaluator()

# predicted_logits - A logistic score between 0 and 1 which is generated after running the model on test dataset
# actual_response - It is a ("yes", "no") variable column generated from the actual values of Performance.Tag variable, where
#                   '0' = 'No' i.e. Not Defaulted and '1' = 'Yes' i.e. Defaulted
# evaluation_for - name of the dataset for which we are evaluating the models


### What model_evaluator() does or what outputs does this function gives?
## The model evaluator does the following tasks:

#-- Evaluator_Graph: Creates an evaluator graph for different cutoff values

#-- Optimal_Cutoff : Finds an optimal cutoff value, where all the three values i.e. Accuracy, Sensitivity and Specificity are balanced.

#-- Confusion_Matrix : Generates a confusion matrix for the optimal cutoff value

#-- Accuracy: Finds the total Accuracy the model achieves at optimal_cutoff_value

#-- Sensitivity: Finds the Sensitivity that the model achieves at optimal_cutoff_value

#-- Specificity: Finds the Specificity that the model achieves at optimal_cutoff_value

# Thus, model_evaluator() function encapsulates the above repetitive task and creates unique variables with unique variable names, into 
# Global Environmet by appending evaluator name at the end of each variable created. 
# This variables can then be called to straight away to view the evaluated results


# Function Inputs: predicted logistic score (between 0 to 1); actual response variable ("yes", "no")
# Function Outputs: Optimal_Cutoff, Confusion Matrix, Accuracy, Sensitivity, Specificity, Evaluator_Graph, Output_Matrix

model_evaluator <- function(predicted_logits, actual_response, evaluation_for)
{
  
  self.predicted_logits <- predicted_logits
  self.actual_response <- actual_response
  
  perform_fn <- function(cutoff) 
  {
    predicted_response <- factor(ifelse(self.predicted_logits >= cutoff, "yes", "no"))
    conf <- confusionMatrix(predicted_response, self.actual_response, positive = "yes")
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    out <- t(as.matrix(c(sens, spec, acc))) 
    colnames(out) <- c("sensitivity", "specificity", "accuracy")
    return(out)
  }
  
  s_min <- min(self.predicted_logits) + 0.001
  s_max <- max(self.predicted_logits) - 0.001
  
  s = seq(s_min, s_max,length=100)
  
  OUT = matrix(0,100,3)
  
  
  for(i in 1:100)
  {
    OUT[i,] = perform_fn(s[i])
  } 
  
  colnames(OUT) <- c('Sensitivity', 'Specificity', 'Accuracy')
  
  firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  
  eval_title <- firstup(evaluation_for)
  
  eval_data <- as.data.frame(cbind(s, OUT))
  colnames(eval_data) <- c('Cutoff', 'Sensitivity', 'Specificity', 'Accuracy')
  eval_new <- melt(eval_data, id = "Cutoff")
  evaluator_graph <- ggplot(data=eval_new,aes(x=Cutoff, y=value, colour=variable)) + geom_line() + scale_y_continuous(labels = scales::percent) + xlab('Cutoff') + ylab('Percentage') + ggtitle(paste(eval_title, "Cutoffs Evaluator Graph", sep = " "))
  
  
  optimal_cutoff <- c()
  cutoff_equaliser <- 0.00
  
  isoptimal <- length(optimal_cutoff)
  
  while(isoptimal == 0)
  {
    optimal_cutoff <- s[which(abs(OUT[,1]-OUT[,2]) <= cutoff_equaliser)]
    isoptimal <- length(optimal_cutoff)
    cutoff_equaliser <- cutoff_equaliser + 0.01
  }
  
  optimal_cutoff <- optimal_cutoff[1]
  
  predicted_response <- factor(ifelse(self.predicted_logits >= optimal_cutoff, "yes", "no"))
  
  conf_final <- confusionMatrix(predicted_response, self.actual_response, positive = "yes")
  
  Accuracy <- conf_final$overall[1]
  
  Sensitivity <- conf_final$byClass[1]
  
  Specificity <- conf_final$byClass[2]
  
  evaluator_graph_name <- paste("evaluator_graph", evaluation_for, sep = "_")
  optimal_cutoff_name <- paste('optimal_cutoff', evaluation_for, sep = '_')
  confusion_matrix_name <- paste('confusion_matrix', evaluation_for, sep = "_")
  accuracy_name <- paste("accuracy", evaluation_for, sep = "_")
  sensitivity_name <- paste("sensitivity", evaluation_for, sep = "_")
  specificity_name <- paste("specificity", evaluation_for,  sep = "_")
  
  
  assign(evaluator_graph_name, evaluator_graph, .GlobalEnv)
  assign(optimal_cutoff_name, optimal_cutoff, .GlobalEnv)
  assign(confusion_matrix_name, conf_final, .GlobalEnv)
  assign(accuracy_name, Accuracy, .GlobalEnv)
  assign(sensitivity_name, Sensitivity, .GlobalEnv)
  assign(specificity_name, Specificity, .GlobalEnv)
}

#############################################################################################
#### DEMOGRAPHIC DATASET LOGISTIC REGRESSION-------------------------------------------
#############################################################################################
#Logistic regression on demo_ds dataset
#######Data Preparation########
demo_features <- subset(demo_ds, select = c(2,5,6,10,11))
demo_factors <- subset(demo_ds, select = c(3,4,7,8,9))
str(demo_features)
str(demo_factors)

# feature standardization is done on numeric variables in order to nullify the impact caused due to the difference of ranges in case of numeric variables.
#example : range of age and range of income
demo_features <- data.frame(sapply(demo_features, function(x) scale(x)))

# creating dummy variables for factor variables
demo_dummies<- data.frame(sapply(demo_factors,function(x) data.frame(model.matrix(~x-1,data =demo_factors))[,-1]))

### Merging the above prepared files to create a master file for model building
demo_ds_master <- cbind(demo_features, demo_dummies)
demo_ds_master <- cbind(demo_ds$Performance.Tag, demo_ds_master) # Adding target variable i.e. Performance tag to master dataset.
demo_ds_master <- cbind(demo_ds_master, demo_ds$Application.ID)  # Adding Application ID to master dataset.
colnames(demo_ds_master)[1] <- "Performance Tag"
demo_ds_master$`Performance Tag`<-as.numeric(levels(demo_ds_master$`Performance Tag`))[demo_ds_master$`Performance Tag`]
str(demo_ds_master)
View(demo_ds_master)

# splitting into train and test data
set.seed(1)
split_indices <- sample.split(demo_ds_master$`Performance Tag`, SplitRatio = 0.70)
train_demo <- demo_ds_master[split_indices, ]
test_demo <- demo_ds_master[!split_indices, ]
nrow(train_demo)/nrow(demo_ds_master)
nrow(test_demo)/nrow(demo_ds_master)

# Removing Application ID from train demo
colnames(train_demo)
train_demo <- train_demo[, -ncol(train_demo)]
str(train_demo)
  
#Initial model building
demo_logistic_1 <- glm(`Performance Tag` ~ ., family = "binomial", data = train_demo)
summary(demo_logistic_1)

# Using stepwise algorithm for removing insignificant variables 
#demo_logistic_2 <- stepAIC(demo_logistic_1, direction = "both")
# stepAIC has removed some variables and only the following ones remain
demo_logistic_2<-glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                       NoOfMthsInCurJob + Education.xMasters + Education.xPhd + 
                       Education.xProfessional + Profession.xSE + TypeOfResi.xOthers,family = "binomial",data=train_demo)
# checking vif for logistic_2 
vif(demo_logistic_2)
summary(demo_logistic_2)

# removing "NoOfMthsInCurResi" the variable is not significant 
demo_logistic_3<-glm(`Performance Tag` ~ Age + NoOfDep + Income + 
                       NoOfMthsInCurJob + Education.xMasters + Education.xPhd + 
                       Education.xProfessional + Profession.xSE + TypeOfResi.xOthers,family = "binomial",data=train_demo)
vif(demo_logistic_3)

summary(demo_logistic_3)

# removing "TypeOfResi.xOthers" the variable is not significant 
demo_logistic_4<-glm(`Performance Tag` ~ Age + NoOfDep + Income + 
                       NoOfMthsInCurJob + Education.xMasters + Education.xPhd + 
                       Education.xProfessional + Profession.xSE,family = "binomial",data=train_demo)
vif(demo_logistic_4)

summary(demo_logistic_4)

# removing "Education.xMasters" the variable is not significant 
demo_logistic_5<-glm(`Performance Tag` ~ Age + NoOfDep + Income + 
                       NoOfMthsInCurJob + Education.xPhd + 
                       Education.xProfessional + Profession.xSE,family = "binomial",data=train_demo)
vif(demo_logistic_5)

summary(demo_logistic_5)

# removing "Education.xPhd" the variable is not significant 
demo_logistic_6<-glm(`Performance Tag` ~ Age + NoOfDep + Income + 
                       NoOfMthsInCurJob +
                       Education.xProfessional + Profession.xSE,family = "binomial",data=train_demo)
vif(demo_logistic_6)
summary(demo_logistic_6)

#removing "Education.xProfessional" the variable is not significant
demo_logistic_7<-glm(`Performance Tag` ~ Age + NoOfDep + Income + 
                       NoOfMthsInCurJob + Profession.xSE,family = "binomial",data=train_demo)
vif(demo_logistic_7)
summary(demo_logistic_7)

#Hence all the insignificant variables removed

demo_ds_logistic_model<-demo_logistic_7

#####Model Evaluation : DEMOGRAPHIC DATASET#####
# Predicting probabilities of responding for the test data
str(test_demo[, -c(1,ncol(test_demo))])       # Dropping PerformanceTag and Application ID before testing
predictions_logit_demo_ds <- predict(demo_ds_logistic_model,newdata = test_demo[, -c(1,ncol(test_demo))],type = "response")
summary(predictions_logit_demo_ds)

actual_response_lr_demo_ds <- factor(ifelse(test_demo$`Performance Tag` == 1, "yes", "no"))
str(actual_response_lr_demo_ds)

####Calling model evaluation function
model_evaluator(predicted_logits = predictions_logit_demo_ds, actual_response =  actual_response_lr_demo_ds, evaluation_for = "demo_ds")

## Evaluator Outputs:
# Evaluator Graph
evaluator_graph_demo_ds

# Optimal_Cutoff :0.061
optimal_cutoff_demo_ds

# COnfusion_Matrix
confusion_matrix_demo_ds

# Accuracy --- 58.3%
accuracy_demo_ds

# Sensitivity --- 58.5%
sensitivity_demo_ds

#Specificity --- 58.2%
specificity_demo_ds

###Performance Graphs : demo_ds
#roc curve 
perf_eva(label = test_demo$`Performance Tag`,pred=predictions_logit_demo_ds,type='roc',title="Demographic dataset")
#KS 
perf_eva(label = test_demo$`Performance Tag`,pred=predictions_logit_demo_ds,type='ks',title="Demographic dataset")
#LIFT
perf_eva(label = test_demo$`Performance Tag`,pred=predictions_logit_demo_ds,type='lift',title="Demographic dataset") 

#############################################################################################
#### MASTER DATASET LOGISTIC REGRESSION-------------------------------------------
#############################################################################################
#Logistic regression on master_ds dataset
#######Data Preparation########
str(master_ds)
master_features <- subset(master_ds, select = c(2,5,6,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,27))
master_factors <- subset(master_ds, select = c(3,4,7,8,9,25,28))
str(master_features)
str(master_factors)

# feature standardization is done on numeric variables in order to nullify the impact caused due to the difference of ranges in case of numeric variables.
#example : range of age and range of income
master_features <- data.frame(sapply(master_features, function(x) scale(x)))

# creating dummy variables for factor variables
master_dummies<- data.frame(sapply(master_factors,function(x) data.frame(model.matrix(~x-1,data =master_factors))[,-1]))

### Merging the above prepared files to create a master file for model building
master_ds_final <- cbind(master_features, master_dummies)
master_ds_final <- cbind(master_ds$Performance.Tag, master_ds_final) # Adding target variable i.e. Performance tag to master dataset.
master_ds_final <- cbind(master_ds_final, master_ds$Application.ID) # Adding Application ID to master dataset.
colnames(master_ds_final)[1] <- "Performance Tag"
master_ds_final$`Performance Tag`<-as.numeric(levels(master_ds_final$`Performance Tag`))[master_ds_final$`Performance Tag`]

str(master_ds_final)
View(master_ds_final)

# splitting into train and test data

set.seed(1)

split_indices <- sample.split(master_ds_final$`Performance Tag`, SplitRatio = 0.70)

train_master <- master_ds_final[split_indices, ]

test_master <- master_ds_final[!split_indices, ]

nrow(train_master)/nrow(master_ds_final)

nrow(test_master)/nrow(master_ds_final)

# Removing Application ID from train_master
colnames(train_master)
train_master <- train_master[, -ncol(train_master)]
str(train_master)

#removing the variables on the basis of  multicollinearity through VIF check and significance through
#pvalues. The general thumb rule is: Remove the variables with high VIF and high pvalues first. Hence we will look
#at the combination of both, and will keep moving forward till all VIF's are settled to the same range.
master_logistic_1 <- glm(`Performance Tag` ~ ., family = "binomial", data = train_master)

summary(master_logistic_1)

# Using stepwise algorithm for removing insignificant variables 

master_logistic_2 <- stepAIC(master_logistic_1, direction = "both")

# stepAIC has removed some variables and only the following ones remain

master_logistic_2<-glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                         NoOfMthsInCurJob + no90DPD6Months + no60DPD6Months + no30DPD6Months + 
                         no90DPD12Months + no60DPD12Months + no30DPD12Months + AvgCCin12Months + 
                         Tradesin12Months + PLTradesin12Months + Outstanding.Balance + 
                         Education.xMasters + Education.xPhd + Education.xProfessional + 
                         Profession.xSE + TypeOfResi.xOthers + OpenHomeLoan + OpenAutoLoan,family = "binomial",data=train_master)

# checking vif  

vif(master_logistic_2)
summary(master_logistic_2)

#Even though significant, VIF of Outstanding.Balance and OpenHomeLoan is high, denotes correlation
#Therefore removing OpenHomeLoan since it is less significant comparatively
master_logistic_3<-glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                         NoOfMthsInCurJob + no90DPD6Months + no60DPD6Months + no30DPD6Months + 
                         no90DPD12Months + no60DPD12Months + no30DPD12Months + AvgCCin12Months + 
                         Tradesin12Months + PLTradesin12Months + Outstanding.Balance + 
                         Education.xMasters + Education.xPhd + Education.xProfessional + 
                         Profession.xSE + TypeOfResi.xOthers + OpenAutoLoan,family = "binomial",data=train_master)
vif(master_logistic_3)

summary(master_logistic_3)

#VIF of no30DPD6Months is high and it is not significant
#removing no30DPD6Months

master_logistic_4<-glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                         NoOfMthsInCurJob + no90DPD6Months + no60DPD6Months +
                         no90DPD12Months + no60DPD12Months + no30DPD12Months + AvgCCin12Months + 
                         Tradesin12Months + PLTradesin12Months + Outstanding.Balance + 
                         Education.xMasters + Education.xPhd + Education.xProfessional + 
                         Profession.xSE + TypeOfResi.xOthers + OpenAutoLoan,family = "binomial",data=train_master)
vif(master_logistic_4)

summary(master_logistic_4)

#VIF of no60DPD6Months is high and it is not significant
#Removing no60DPD6Months
master_logistic_5<-glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                         NoOfMthsInCurJob + no90DPD6Months +
                         no90DPD12Months + no60DPD12Months + no30DPD12Months + AvgCCin12Months + 
                         Tradesin12Months + PLTradesin12Months + Outstanding.Balance + 
                         Education.xMasters + Education.xPhd + Education.xProfessional + 
                         Profession.xSE + TypeOfResi.xOthers + OpenAutoLoan,family = "binomial",data=train_master)
vif(master_logistic_5)

summary(master_logistic_5)

#Excluding Tradesin12Months
master_logistic_6<-glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                         NoOfMthsInCurJob + no90DPD6Months +
                         no90DPD12Months + no60DPD12Months + no30DPD12Months + AvgCCin12Months + 
                         PLTradesin12Months + Outstanding.Balance + 
                         Education.xMasters + Education.xPhd + Education.xProfessional + 
                         Profession.xSE + TypeOfResi.xOthers + OpenAutoLoan,family = "binomial",data=train_master)
vif(master_logistic_6)

summary(master_logistic_6)
cor(train_master$no90DPD6Months,train_master$no90DPD12Months)
#89% correlation. therefore removing no90DPD6Months due to less significance comparatively
#Removing no90DPD6Months
master_logistic_7<-glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                         NoOfMthsInCurJob +
                         no90DPD12Months + no60DPD12Months + no30DPD12Months + AvgCCin12Months + 
                         PLTradesin12Months + Outstanding.Balance + 
                         Education.xMasters + Education.xPhd + Education.xProfessional + 
                         Profession.xSE + TypeOfResi.xOthers + OpenAutoLoan,family = "binomial",data=train_master)
vif(master_logistic_7)
cor(train_master$no30DPD12Months,train_master$no60DPD12Months)
summary(master_logistic_7)
#90%, therefore removing no60DPD12Months since it is comparatively less significant

master_logistic_8<-glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                         NoOfMthsInCurJob +
                         no90DPD12Months + no30DPD12Months + AvgCCin12Months + 
                         PLTradesin12Months + Outstanding.Balance + 
                         Education.xMasters + Education.xPhd + Education.xProfessional + 
                         Profession.xSE + TypeOfResi.xOthers + OpenAutoLoan,family = "binomial",data=train_master)
vif(master_logistic_8)

summary(master_logistic_8)

cor(train_master$no90DPD12Months,train_master$no30DPD12Months)

#Removing no90DPD6Months

master_logistic_9<-glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                         NoOfMthsInCurJob +
                         no90DPD12Months + AvgCCin12Months + 
                         PLTradesin12Months + Outstanding.Balance + 
                         Education.xMasters + Education.xPhd + Education.xProfessional + 
                         Profession.xSE + TypeOfResi.xOthers + OpenAutoLoan,family = "binomial",data=train_master)
sort(vif(master_logistic_9))

#all the VIF values are very low and pretty much in the same range.
# Hence VIF will no more be a deciding factor for further removal of variables.
#now we will proceed only on the basis of significance by just taking pvalues into consideration.
summary(master_logistic_9)
#Removing OpenAutoLoan
master_logistic_10<-glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                          NoOfMthsInCurJob +
                          no90DPD12Months + AvgCCin12Months + 
                          PLTradesin12Months + Outstanding.Balance + 
                          Education.xMasters + Education.xPhd + Education.xProfessional + 
                          Profession.xSE + TypeOfResi.xOthers,family = "binomial",data=train_master)
summary(master_logistic_10)


#Removing TypeOfResi.xOthers
master_logistic_11<-glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                          NoOfMthsInCurJob +
                          no90DPD12Months + AvgCCin12Months + 
                          PLTradesin12Months + Outstanding.Balance + 
                          Education.xMasters + Education.xPhd + Education.xProfessional + 
                          Profession.xSE,family = "binomial",data=train_master)
summary(master_logistic_11)

#Removing Education.xMasters
master_logistic_12<-glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                          NoOfMthsInCurJob +
                          no90DPD12Months + AvgCCin12Months + 
                          PLTradesin12Months + Outstanding.Balance + 
                          Education.xPhd + Education.xProfessional + 
                          Profession.xSE,family = "binomial",data=train_master)
summary(master_logistic_12)

#Remove Education.xPhd
master_logistic_13<-glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                          NoOfMthsInCurJob +
                          no90DPD12Months + AvgCCin12Months + 
                          PLTradesin12Months + Outstanding.Balance + 
                          Education.xProfessional + 
                          Profession.xSE,family = "binomial",data=train_master)
summary(master_logistic_13)

#Remove Education.xProfessional
master_logistic_14<-glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                          NoOfMthsInCurJob +
                          no90DPD12Months + AvgCCin12Months + 
                          PLTradesin12Months + Outstanding.Balance + 
                          Profession.xSE,family = "binomial",data=train_master)
summary(master_logistic_14)


#No more insignificant variables, therefore final model
master_ds_logistic_model<-master_logistic_14

######Model Evaluation for master_ds dataset
# Predicting probabilities of responding for the test data
str(test_master[, -c(1,ncol(test_master))])     # Dropping PerformanceTag and Application ID before testing
predictions_logit_master_ds <- predict(master_ds_logistic_model,newdata = test_master[, -c(1,ncol(test_master))],type = "response")
summary(predictions_logit_master_ds)

actual_response_lr_master_ds <- factor(ifelse(test_master$`Performance Tag` == 1, "yes", "no"))
str(actual_response_lr_master_ds)

model_evaluator(predicted_logits = predictions_logit_master_ds, actual_response =  actual_response_lr_master_ds, evaluation_for = "master_ds")

## Evaluator Outputs:
# Evaluator Graph
evaluator_graph_master_ds

# Optimal_Cutoff : 0.052
optimal_cutoff_master_ds

# COnfusion_Matrix
confusion_matrix_master_ds

# Accuracy --- 67.1%
accuracy_master_ds

# Sensitivity --- 69%
sensitivity_master_ds

#Specificity --- 67%
specificity_master_ds

###Performance Graphs : master_ds
#roc curve 
perf_eva(label = test_master$`Performance Tag`,pred=predictions_logit_master_ds,type='roc',title="Master dataset")
#KS 
perf_eva(label = test_master$`Performance Tag`,pred=predictions_logit_master_ds,type='ks',title="Master dataset")
#LIFT
perf_eva(label = test_master$`Performance Tag`,pred=predictions_logit_master_ds,type='lift',title="Master dataset") 


###########################################################################################################################

### Weighted Logistic Regression --- Demographic ###

###########################################################################################################################

#######Data splitting
set.seed(1)

split_indices <- sample.split(demo_ds_master$`Performance Tag`, SplitRatio = 0.70)

train_demo_wt <- demo_ds_master[split_indices, ]

test_demo_wt <- demo_ds_master[!split_indices, ]

nrow(train_demo_wt)/nrow(demo_ds_master)

nrow(test_demo_wt)/nrow(demo_ds_master)

# Removing Application ID from train_demo_wt
colnames(train_demo_wt)
train_demo_wt <- train_demo_wt[, -ncol(train_demo_wt)]
str(train_demo_wt)


# Count % of Defaults and Non-Defaults

round(prop.table(table(train_demo_wt$`Performance Tag`)),2)

#   0     1 
# 0.94  0.06 

# Clearly, the data is highly biased towards Non-Defaulters, which accounts to nearly 94%. And only 6% of the total
# data are defaulters. 

# Hence, to get balanced results, we have to find a way around this unbalanced data. And we can do that by applying the
# idea of weighting which is related to sampling. All we need is sufficient number of 1's for the maximum likelihood to converge.

# So we will proceed in the following manner:

# We will Sample 100 % of the 1's (i.e. all the default transactions) and 10% of the 0's and use a weight of 1 for defaults 
# (fraud) transaction and a weight of 10 for non-default (good) transactions. 

str(train_demo_wt)

# Subsetting the data as defaults and non-defaults
train_demo_Perf0 <- subset(train_demo_wt, `Performance Tag` == 0)
train_demo_Perf1 <- subset(train_demo_wt, `Performance Tag` == 1)

# Verifying the subsets
unique(train_demo_Perf0$`Performance Tag`)
unique(train_demo_Perf1$`Performance Tag`)


# Sampling 10% of 0's
Perf0_index_demo <- sample(1:nrow(train_demo_Perf0), nrow(train_demo_Perf0)*0.1, replace = FALSE)

# Checking if all indexes are unique
sum(duplicated(Perf0_index_demo))

train_demo_Perf0 <- train_demo_Perf0[Perf0_index_demo, ]

# Structure of subsets
str(train_demo_Perf0)
str(train_demo_Perf1)

train_demo_weighted <- rbind(train_demo_Perf0, train_demo_Perf1)
str(train_demo_weighted)

## Adding a weights variable, having weights = 1 for defaulters and weights = 10 for non-defaulters
train_demo_weighted$weights <- ifelse(train_demo_weighted$`Performance Tag` == 0, 10, 1)

## Shuffling the dataset
train_demo_weighted <- train_demo_weighted[sample(nrow(train_demo_weighted), replace = FALSE), ]

## Checking if we incorporated any duplication during data preparation for weighted Logistic Regression
sum(duplicated(train_demo_weighted))

## Final Training Data for Weighted Logistic Regression
str(train_demo_weighted)
View(train_demo_weighted)

round(prop.table(table(train_demo_weighted$`Performance Tag`)),2) ## The data is reasonably balanced now

# Model Building through weighted logistic Regression Method


lr_wt_demo_1 <- glm(`Performance Tag` ~ .-weights, family = "binomial", data = train_demo_weighted, weights = weights)

summary(lr_wt_demo_1)

# # STEPAIC method for quickly removing some of the insignificant variables
# 
lr_wt_demo_2 <- stepAIC(lr_wt_demo_1, direction = "both") #AIC= 21879.62

# The following variables were found to be significant as per STEPAIC Algorithm
lr_wt_demo_2 <- glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                      NoOfMthsInCurJob + Education.xMasters + Education.xPhd + 
                      Education.xProfessional + Profession.xSE + Profession.xSE_PROF,family = "binomial", data = train_demo_weighted, weights = weights)

summary(lr_wt_demo_2)
vif(lr_wt_demo_2)

# Since all vif's are in the same considerable range i.e. below 2, hence we will remove variables just on the basis of
# significance level

# Removing NoOfMthsInCurResi
lr_wt_demo_3 <- glm(`Performance Tag` ~ Age + NoOfDep + Income +
                      NoOfMthsInCurJob + Education.xMasters + Education.xPhd + Education.xProfessional + Profession.xSE,
                    family = "binomial", data = train_demo_weighted, weights = weights)

summary(lr_wt_demo_3)

# Removing Education.xMasters
lr_wt_demo_4 <- glm(`Performance Tag` ~ Age + NoOfDep + Income +
                      NoOfMthsInCurJob + Education.xPhd + Education.xProfessional + Profession.xSE,
                    family = "binomial", data = train_demo_weighted, weights = weights)

summary(lr_wt_demo_4)

# Removing Education.xPhd
lr_wt_demo_5 <- glm(`Performance Tag` ~ Age + NoOfDep + Income +
                      NoOfMthsInCurJob + Education.xProfessional + Profession.xSE,
                    family = "binomial", data = train_demo_weighted, weights = weights)

summary(lr_wt_demo_5)

## All the remaining variables are quite significant. Hence we would keep that in our final model

final_lr_wt_demo <- lr_wt_demo_5

#---------------------------------------------------------    

# Predicting probabilities of responding for the test data
str(test_demo_wt[, -c(1,ncol(test_demo_wt))])            # Dropping PerformanceTag and Application ID before testing
predictions_logit_wt_demo <- predict(final_lr_wt_demo, newdata = test_demo_wt[, -c(1,ncol(test_demo_wt))], type = "response")
summary(predictions_logit_wt_demo)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

#---------------------------------------------------------    

actual_response_lr_demo_wt_ds <- factor(ifelse(test_demo_wt$`Performance Tag` == 1, "yes", "no"))
str(actual_response_lr_demo_wt_ds)
model_evaluator(predicted_logits = predictions_logit_wt_demo, actual_response =  actual_response_lr_demo_wt_ds, evaluation_for = "demo_wt_ds")

### Evaluator Outputs:

# Evaluator Graph
evaluator_graph_demo_wt_ds

# Optimal_Cutoff --- 0.0621
optimal_cutoff_demo_wt_ds

# COnfusion_Matrix
confusion_matrix_demo_wt_ds

#Accuracy : ~60%
accuracy_demo_wt_ds

# Sensitivity --- ~58%
sensitivity_demo_wt_ds

#Specificity --- ~60%
specificity_demo_wt_ds

###Performance Graphs : demo dataset weighted
#roc curve 
perf_eva(label = test_demo_wt$`Performance Tag`,pred=predictions_logit_wt_demo,type='roc',title="Demographic dataset weighted")
#KS 
perf_eva(label = test_demo_wt$`Performance Tag`,pred=predictions_logit_wt_demo,type='ks',title="Demographic dataset weighted")
#LIFT
perf_eva(label = test_demo_wt$`Performance Tag`,pred=predictions_logit_wt_demo,type='lift',title="Demographic dataset weighted") 



###########################################################################################################################

### Weighted Logistic Regression --- Master ###

###########################################################################################################################

set.seed(1)

split_indices <- sample.split(master_ds_final$`Performance Tag`, SplitRatio = 0.70)

train_master_wt <- master_ds_final[split_indices, ]

test_master_wt <- master_ds_final[!split_indices, ]

nrow(train_master_wt)/nrow(master_ds_final)

nrow(test_master_wt)/nrow(master_ds_final)

# Removing Application ID from train_master_wt
colnames(train_master_wt)
train_master_wt <- train_master_wt[, -ncol(train_master_wt)]
str(train_master_wt)


# Count % of Defaults and Non-Defaults

round(prop.table(table(train_master_wt$`Performance Tag`)),2)

#   0     1 
# 0.94  0.06 

# Clearly, the data is highly biased towards Non-Defaulters, which accounts to nearly 94%. And only 6% of the total
# data are defaulters. 

# Hence, to get balanced results, we have to find a way around this unbalanced data. And we can do that by applying the
# idea of weighting which is related to sampling. All we need is sufficient number of 1's for the maximum likelihood to converge.

# So we will proceed in the following manner:

# We will Sample 100 % of the 1's (i.e. all the default transactions) and 10% of the 0's and use a weight of 1 for defaults 
# (fraud) transaction and a weight of 10 for non-default (good) transactions. 



# Subsetting the data as defaults and non-defaults
train_master_Perf0 <- subset(train_master_wt, `Performance Tag` == 0)
train_master_Perf1 <- subset(train_master_wt, `Performance Tag` == 1)

# Verifying the subsets
unique(train_master_Perf0$`Performance Tag`)
unique(train_master_Perf1$`Performance Tag`)


# Sampling 10% of 0's
Perf0_index_master <- sample(1:nrow(train_master_Perf0), nrow(train_master_Perf0)*0.1, replace = FALSE)

# Checking if all indexes are unique
sum(duplicated(Perf0_index_master))

train_master_Perf0 <- train_master_Perf0[Perf0_index_master, ]

# Structure of subsets
str(train_master_Perf0)
str(train_master_Perf1)

train_master_weighted <- rbind(train_master_Perf0, train_master_Perf1)

## Adding a weights variable, having weights = 1 for defaulters and weights = 10 for non-defaulters
train_master_weighted$weights <- ifelse(train_master_weighted$`Performance Tag` == 0, 10, 1)

## Shuffling the dataset
train_master_weighted <- train_master_weighted[sample(nrow(train_master_weighted), replace = FALSE), ]

## Checking if we incorporated any duplication during data preparation for weighted Logistic Regression
sum(duplicated(train_master_weighted))

## Final Training Data for Weighted Logistic Regression
str(train_master_weighted)
View(train_master_weighted)

round(prop.table(table(train_master_weighted$`Performance Tag`)),2) ## The data is reasonably balanced now

# Model Building through weighted logistic Regression Method

lr_wt_master_1 <- glm(`Performance Tag` ~ .-weights, family = "binomial", data = train_master_weighted, weights = weights)

summary(lr_wt_master_1)

#STEPAIC method for quickly removing some of the insignificant variables
# set.seed(1)
# lr_wt_master_2 <- stepAIC(lr_wt_master_1, direction = "both") #19417.63

# The following variables were found to be significant as per STEPAIC Algorithm
lr_wt_master_2 <- glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                        NoOfMthsInCurJob + no90DPD6Months + no60DPD6Months + no30DPD6Months + 
                        no90DPD12Months + no60DPD12Months + no30DPD12Months + AvgCCin12Months + 
                        Tradesin6Months + PLTradesin6Months + PLTradesin12Months + 
                        Outstanding.Balance + Education.xOthers + Education.xProfessional + 
                        Profession.xSE + Profession.xSE_PROF + OpenHomeLoan + OpenAutoLoan, 
                      family = "binomial", data = train_master_weighted, weights = weights)

summary(lr_wt_master_2)
sort(vif(lr_wt_master_2))

# It appears that Outstanding.Balance and OpenHomeLoan are highly correlated, since they have very high VIF values.
# Hence we will remove one of them, which has lower significance.

# Removing OpenHomeLoan
lr_wt_master_3 <- glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                        NoOfMthsInCurJob + no90DPD6Months + no60DPD6Months + no30DPD6Months + 
                        no90DPD12Months + no60DPD12Months + no30DPD12Months + AvgCCin12Months + 
                        Tradesin6Months + PLTradesin6Months + PLTradesin12Months + 
                        Outstanding.Balance + Education.xOthers + Education.xProfessional + 
                        Profession.xSE + Profession.xSE_PROF + OpenAutoLoan, 
                      family = "binomial", data = train_master_weighted, weights = weights)

summary(lr_wt_master_3)
sort(vif(lr_wt_master_3))

# Removing no30DPD6months since it has a high VIF and comparatively lower significance
lr_wt_master_4 <- glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                        NoOfMthsInCurJob + no90DPD6Months + no60DPD6Months +
                        no90DPD12Months + no60DPD12Months + no30DPD12Months + AvgCCin12Months + 
                        Tradesin6Months + PLTradesin6Months + PLTradesin12Months + 
                        Outstanding.Balance + Education.xOthers + Education.xProfessional + 
                        Profession.xSE + Profession.xSE_PROF + OpenAutoLoan, 
                      family = "binomial", data = train_master_weighted, weights = weights)

summary(lr_wt_master_4)
sort(vif(lr_wt_master_4))

# Removing no60DPD6Months since it has a high VIF and comparatively lower significance
lr_wt_master_5 <- glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                        NoOfMthsInCurJob + no90DPD6Months + no90DPD12Months + no60DPD12Months + no30DPD12Months + 
                        AvgCCin12Months + Tradesin6Months + PLTradesin6Months + PLTradesin12Months + 
                        Outstanding.Balance + Education.xOthers + Education.xProfessional + 
                        Profession.xSE + Profession.xSE_PROF + OpenAutoLoan, 
                      family = "binomial", data = train_master_weighted, weights = weights)

summary(lr_wt_master_5)
sort(vif(lr_wt_master_5))

#cor(train_master_weighted$no30DPD12Months,train_master_weighted$no60DPD12Months)
#91% #therefore there is high level collinearity, 
#hence removing no60DPD12Months as it is less significant comparatively
lr_wt_master_6 <- glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                        NoOfMthsInCurJob + no90DPD6Months + no90DPD12Months + no30DPD12Months + 
                        AvgCCin12Months + Tradesin6Months + PLTradesin6Months + PLTradesin12Months + 
                        Outstanding.Balance + Education.xOthers + Education.xProfessional + 
                        Profession.xSE + Profession.xSE_PROF + OpenAutoLoan, 
                      family = "binomial", data = train_master_weighted, weights = weights)

summary(lr_wt_master_6)
sort(vif(lr_wt_master_6))

cor(train_master_weighted$no30DPD12Months,train_master_weighted$no90DPD12Months)
#82% collinearity, removing no90DPD12Months as it is less significant comparatively

lr_wt_master_7 <- glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                        NoOfMthsInCurJob + no90DPD6Months + no30DPD12Months + 
                        AvgCCin12Months + Tradesin6Months + PLTradesin6Months + PLTradesin12Months + 
                        Outstanding.Balance + Education.xOthers + Education.xProfessional + 
                        Profession.xSE + Profession.xSE_PROF + OpenAutoLoan, 
                      family = "binomial", data = train_master_weighted, weights = weights)

summary(lr_wt_master_7)
sort(vif(lr_wt_master_7))

cor(train_master_weighted$no30DPD12Months,train_master_weighted$no90DPD6Months)
#82%, high collinarity, so removing no90DPD6Months
lr_wt_master_8 <- glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                        NoOfMthsInCurJob + no30DPD12Months + 
                        AvgCCin12Months + Tradesin6Months + PLTradesin6Months + PLTradesin12Months + 
                        Outstanding.Balance + Education.xOthers + Education.xProfessional + 
                        Profession.xSE + Profession.xSE_PROF + OpenAutoLoan, 
                      family = "binomial", data = train_master_weighted, weights = weights)

summary(lr_wt_master_8)
sort(vif(lr_wt_master_8))

cor(train_master_weighted$PLTradesin6Months,train_master_weighted$Tradesin6Months)
#~88%, so remove lesser significant variable

# Removing Tradesin6Months since it has slightly high VIF and less significant
lr_wt_master_9 <- glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                        NoOfMthsInCurJob + no30DPD12Months + 
                        AvgCCin12Months + PLTradesin6Months + PLTradesin12Months + 
                        Outstanding.Balance + Education.xOthers + Education.xProfessional + 
                        Profession.xSE + Profession.xSE_PROF + OpenAutoLoan, 
                      family = "binomial", data = train_master_weighted, weights = weights)

summary(lr_wt_master_9)
sort(vif(lr_wt_master_9))

cor(train_master_weighted$PLTradesin6Months,train_master_weighted$PLTradesin12Months)
#~88%, so removing lesser signofocant variable
#Removing PLTradesin6Months
lr_wt_master_10 <- glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                         NoOfMthsInCurJob + no30DPD12Months + 
                         AvgCCin12Months + PLTradesin12Months + 
                         Outstanding.Balance + Education.xOthers + Education.xProfessional + 
                         Profession.xSE + Profession.xSE_PROF + OpenAutoLoan, 
                       family = "binomial", data = train_master_weighted, weights = weights)

summary(lr_wt_master_10)
sort(vif(lr_wt_master_10))


# All VIF's are in considerable range except 4 variables i.e. "no90DPD12Months", "no90DPD6Months", "no30DPD6Months", "no60DPD12Months"
# At the same time, all of these variables are highly significant. This scenario usually happens when some of the 
# variables are significant but at the same time, correlated. Hence we will remove the above variables one by one 
# in order of their insgnificance to reduce multicollinearity

# Removing OpenAutoLoan since it is insignificant
lr_wt_master_11 <- glm(`Performance Tag` ~ Age + NoOfDep + Income + NoOfMthsInCurResi + 
                         NoOfMthsInCurJob + no30DPD12Months + 
                         AvgCCin12Months + PLTradesin12Months + 
                         Outstanding.Balance + Education.xOthers + Education.xProfessional + 
                         Profession.xSE + Profession.xSE_PROF, 
                       family = "binomial", data = train_master_weighted, weights = weights)

summary(lr_wt_master_11)
sort(vif(lr_wt_master_11))

# Removing Age since it is insignificant
lr_wt_master_12 <- glm(`Performance Tag` ~ NoOfDep + Income + NoOfMthsInCurResi + 
                         NoOfMthsInCurJob + no30DPD12Months + 
                         AvgCCin12Months + PLTradesin12Months + 
                         Outstanding.Balance + Education.xOthers + Education.xProfessional + 
                         Profession.xSE + Profession.xSE_PROF, 
                       family = "binomial", data = train_master_weighted, weights = weights)

summary(lr_wt_master_12)
sort(vif(lr_wt_master_12))

# Removing Education.xProfessional since it is correlated
lr_wt_master_13 <- glm(`Performance Tag` ~ NoOfDep + Income + NoOfMthsInCurResi + 
                         NoOfMthsInCurJob + no30DPD12Months + 
                         AvgCCin12Months + PLTradesin12Months + 
                         Outstanding.Balance + Education.xOthers +  
                         Profession.xSE + Profession.xSE_PROF, 
                       family = "binomial", data = train_master_weighted, weights = weights)

summary(lr_wt_master_13)
sort(vif(lr_wt_master_13))


#Removeing Profession.xSE_PROF
lr_wt_master_14 <- glm(`Performance Tag` ~ NoOfDep + Income + NoOfMthsInCurResi + 
                         NoOfMthsInCurJob + no30DPD12Months + 
                         AvgCCin12Months + PLTradesin12Months + 
                         Outstanding.Balance + Education.xOthers +  
                         Profession.xSE , 
                       family = "binomial", data = train_master_weighted, weights = weights)

summary(lr_wt_master_14)
sort(vif(lr_wt_master_14))

#Removing Education.xOthers
lr_wt_master_15 <- glm(`Performance Tag` ~ NoOfDep + Income + NoOfMthsInCurResi + 
                         NoOfMthsInCurJob + no30DPD12Months + 
                         AvgCCin12Months + PLTradesin12Months + 
                         Outstanding.Balance +  
                         Profession.xSE , 
                       family = "binomial", data = train_master_weighted, weights = weights)

summary(lr_wt_master_15)
sort(vif(lr_wt_master_15))

#Removing NoOfDep
lr_wt_master_16 <- glm(`Performance Tag` ~ Income + NoOfMthsInCurResi + 
                         NoOfMthsInCurJob + no30DPD12Months + 
                         AvgCCin12Months + PLTradesin12Months + 
                         Outstanding.Balance +  
                         Profession.xSE , 
                       family = "binomial", data = train_master_weighted, weights = weights)

summary(lr_wt_master_16)
sort(vif(lr_wt_master_16))



## All the remaining variables are quite significant. Hence we would keep that in our final model

final_lr_wt_master <- lr_wt_master_16

#---------------------------------------------------------    

# Predicting probabilities of responding for the test data
str(test_master_wt[, -c(1,ncol(test_master_wt))])            # Dropping PerformanceTag and Application ID before testing
predictions_logit_wt_master <- predict(final_lr_wt_master, newdata = test_master_wt[, -c(1,ncol(test_master_wt))], type = "response")
summary(predictions_logit_wt_master)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

#---------------------------------------------------------    

actual_response_lr_test_master_wt <- factor(ifelse(test_master_wt$`Performance Tag` == 1, "yes", "no"))
str(actual_response_lr_test_master_wt)
View(test_master)
model_evaluator(predicted_logits = predictions_logit_wt_master, actual_response =  actual_response_lr_test_master_wt, evaluation_for = "master_wt")

### Evaluator Outputs:


# Evaluator Graph
evaluator_graph_master_wt

# Optimal_Cutoff --- 0.46
optimal_cutoff_master_wt

# COnfusion_Matrix
confusion_matrix_master_wt

# Accuracy --- ~67%
accuracy_master_wt

# Sensitivity --- 70.1%
sensitivity_master_wt

#Specificity --- ~67%
specificity_master_wt

###Performance Graphs : master dataset weighted
#roc curve 
perf_eva(label = test_master_wt$`Performance Tag`,pred=predictions_logit_wt_master,type='roc',title="Master dataset weighted")
#KS 
perf_eva(label = test_master_wt$`Performance Tag`,pred=predictions_logit_wt_master,type='ks',title="Master dataset weighted")
#LIFT
perf_eva(label = test_master_wt$`Performance Tag`,pred=predictions_logit_wt_master,type='lift',title="Master dataset weighted") 



############################################################################################################################

### Logistic Regression : WOE Demographic Dataset ###

############################################################################################################################

str(demo_woe)
demo_woe_final <- demo_woe[-1]

colnames(demo_woe_final)

# # Making the column names more reasonable
colnames(demo_woe_final)[1] <- "Age"
colnames(demo_woe_final)[2] <- "Gender"
colnames(demo_woe_final)[3] <- "Maritalstatus"
colnames(demo_woe_final)[4] <- "NoOfDep"
colnames(demo_woe_final)[5] <- "Income"
colnames(demo_woe_final)[6] <- "Education"
colnames(demo_woe_final)[7] <- "Profession"
colnames(demo_woe_final)[8] <- "TypeOfResi"
colnames(demo_woe_final)[9] <- "NoOfMthsInCurResi"
colnames(demo_woe_final)[10] <- "NoOfMthsInCurJob"
colnames(demo_woe_final)[11] <- "PerformanceTag"

str(demo_woe_final)
demo_woe_final <- cbind(subset(demo_woe_final, select = c(PerformanceTag)), subset(demo_woe_final, select = -c(PerformanceTag)))
demo_woe_final <- cbind(demo_woe_final, demo_woe$Application.ID)  # Adding Application ID to refer to the dataset later

str(demo_woe_final)


# splitting the data between train and test
set.seed(1)

split_indices <- sample.split(demo_woe_final$PerformanceTag, SplitRatio = 0.70)

train_demo_woe <- demo_woe_final[split_indices, ]

test_demo_woe <- demo_woe_final[!split_indices, ]

nrow(train_demo_woe)/nrow(demo_woe_final)

nrow(test_demo_woe)/nrow(demo_woe_final)

# Removing Application ID from train_demo_woe
colnames(train_demo_woe)
train_demo_woe <- train_demo_woe[, -ncol(train_demo_woe)]
str(train_demo_woe)



str(train_demo_woe)
str(test_demo_woe)
########################################################################

## Model Building: 

#Initial model
lr_demo_woe_1 = glm(PerformanceTag ~ ., data = train_demo_woe, family = "binomial")

summary(lr_demo_woe_1) #AIC 21484

# # STEPAIC method for quickly removing some of the insignificant variables
set.seed(1)

lr_demo_woe_2 <- stepAIC(lr_demo_woe_1, direction = "both") #AIC= 21479.77

# The following variables were found to be significant as per STEPAIC Algorithm
lr_demo_woe_2 <- glm(PerformanceTag ~ Age + NoOfDep + Income + Education + Profession + NoOfMthsInCurResi + NoOfMthsInCurJob, 
                     family = "binomial", data = train_demo_woe)

summary(lr_demo_woe_2)
sort(vif(lr_demo_woe_2))

# Since all vif's are in the same considerable range i.e. below 2, hence we will remove variables just on the basis of
# significance level

# Removing Education
lr_demo_woe_3 <- glm(PerformanceTag ~ Age + NoOfDep + Income + Profession + NoOfMthsInCurResi + NoOfMthsInCurJob, 
                     family = "binomial", data = train_demo_woe)

summary(lr_demo_woe_3)


## All the remaining variables are quite significant. Hence we would keep that in our final model

final_lr_demo_woe <- lr_demo_woe_3

#--------------------------------------------------------------------------------------------------------------------------

## Model Evaluation
str(test_demo_woe[, -c(1,ncol(test_demo_woe))])            # Dropping PerformanceTag and Application ID before testing
predictions_logit_demo_woe <- predict(final_lr_demo_woe, newdata = test_demo_woe[, -c(1,ncol(test_demo_woe))], type = "response")
summary(predictions_logit_demo_woe)

actual_response_lr_demo_woe <- factor(ifelse(test_demo_woe$PerformanceTag == 1, "yes", "no"))
str(actual_response_lr_demo_woe)

model_evaluator(predicted_logits = predictions_logit_demo_woe, actual_response =  actual_response_lr_demo_woe, evaluation_for = "demo_woe")

## Evaluator Outputs:


# Evaluator Graph
evaluator_graph_demo_woe

# Optimal_Cutoff --- 0.06
optimal_cutoff_demo_woe

# COnfusion_Matrix
confusion_matrix_demo_woe

# Accuracy --- 63%
accuracy_demo_woe

# Sensitivity --- 61.6%
sensitivity_demo_woe

#Specificity --- 63%
specificity_demo_woe

###Performance Graphs : demo woe dataset
#roc curve 
perf_eva(label = test_demo_woe$PerformanceTag,pred=predictions_logit_demo_woe,type='roc',title="demo woe dataset")
#KS 
perf_eva(label = test_demo_woe$PerformanceTag,pred=predictions_logit_demo_woe,type='ks',title="demo woe dataset")
#LIFT
perf_eva(label = test_demo_woe$PerformanceTag,pred=predictions_logit_demo_woe,type='lift',title="demo woe dataset") 



############################################################################################################################

### Logistic Regression : WOE Master Dataset ###

############################################################################################################################

str(master_woe)
master_woe_final <- master_woe[-1]
colnames(master_woe_final)

# Making the column names more reasonable

colnames(master_woe_final)[1] <- "no90DPD6Months"
colnames(master_woe_final)[2] <- "no60DPD6Months"
colnames(master_woe_final)[3] <- "no30DPD6Months"
colnames(master_woe_final)[4] <- "no90DPD12Months"
colnames(master_woe_final)[5] <- "no60DPD12Months"
colnames(master_woe_final)[6] <- "no30DPD12Months"
colnames(master_woe_final)[7] <- "AvgCCin12Months"
colnames(master_woe_final)[8] <- "Tradesin6Months"
colnames(master_woe_final)[9] <- "Tradesin12Months"
colnames(master_woe_final)[10] <- "PLTradesin6Months"
colnames(master_woe_final)[11] <- "PLTradesin12Months"
colnames(master_woe_final)[12] <- "Inq6Months"
colnames(master_woe_final)[13] <- "Inq12Months"
colnames(master_woe_final)[14] <- "OpenHomeLoan"
colnames(master_woe_final)[15] <- "OutstandingBal"
colnames(master_woe_final)[16] <- "TotalTrades"
colnames(master_woe_final)[17] <- "OpenAutoLoan"
colnames(master_woe_final)[18] <- "Age"
colnames(master_woe_final)[19] <- "Gender"
colnames(master_woe_final)[20] <- "MaritalStatus"
colnames(master_woe_final)[21] <- "NoofDep"
colnames(master_woe_final)[22] <- "Income"
colnames(master_woe_final)[23] <- "Education"
colnames(master_woe_final)[24] <- "Profession"
colnames(master_woe_final)[25] <- "TypeofResi"
colnames(master_woe_final)[26] <- "MonthsCurrResi"
colnames(master_woe_final)[27] <- "MonthsCurrComp"
colnames(master_woe_final)[28] <- "PerformanceTag"



str(master_woe_final)
master_woe_final <- cbind(subset(master_woe_final, select = c(PerformanceTag)), subset(master_woe_final, select = -c(PerformanceTag)))
master_woe_final <- cbind(master_woe_final, master_woe$Application.ID)  # Adding Application ID to refer to the dataset later

str(master_woe_final)


# splitting the data between train and test
set.seed(1)

split_indices <- sample.split(master_woe_final$PerformanceTag, SplitRatio = 0.70)

train_master_woe <- master_woe_final[split_indices, ]

test_master_woe <- master_woe_final[!split_indices, ]

nrow(train_master_woe)/nrow(master_woe_final)

nrow(test_master_woe)/nrow(master_woe_final)

# Removing Application ID from train_master_woe
colnames(train_master_woe)
train_master_woe <- train_master_woe[, -ncol(train_master_woe)]


str(train_master_woe)
str(test_master_woe)

########################################################################

## Model Building: 

#Initial model
lr_master_woe_1 = glm(PerformanceTag ~ ., data = train_master_woe, family = "binomial")

summary(lr_master_woe_1) #AIC 19550.313



# # STEPAIC method for quickly removing some of the insignificant variables
set.seed(1)

lr_master_woe_2 <- stepAIC(lr_master_woe_1, direction = "both") #AIC= 19535.71

# The following variables were found to be significant as per STEPAIC Algorithm
lr_master_woe_2 <- glm(PerformanceTag ~ no90DPD6Months + no60DPD6Months + no30DPD6Months + 
                         no90DPD12Months + no30DPD12Months + AvgCCin12Months + Tradesin12Months + 
                         PLTradesin12Months + Inq12Months + OpenHomeLoan + TotalTrades + 
                         Age + NoofDep + Income + Education + Profession + MonthsCurrResi + MonthsCurrComp, 
                       family = "binomial", data = train_master_woe)

summary(lr_master_woe_2)
sort(vif(lr_master_woe_2))


# Removing no30DPD6Months since it has a high VIF and low significance
lr_master_woe_3 <- glm(PerformanceTag ~ no90DPD6Months + no60DPD6Months +  
                         no90DPD12Months + no30DPD12Months + AvgCCin12Months + Tradesin12Months + 
                         PLTradesin12Months + Inq12Months + OpenHomeLoan + TotalTrades + 
                         Age + NoofDep + Income + Education + Profession + MonthsCurrResi + MonthsCurrComp, 
                       family = "binomial", data = train_master_woe)

summary(lr_master_woe_3)
sort(vif(lr_master_woe_3))

# Removing Tradesin12Months since it has a high VIF and low significance
lr_master_woe_4 <- glm(PerformanceTag ~ no90DPD6Months + no60DPD6Months +  
                         no90DPD12Months + no30DPD12Months + AvgCCin12Months +  PLTradesin12Months + Inq12Months + OpenHomeLoan + TotalTrades + 
                         Age + NoofDep + Income + Education + Profession + MonthsCurrResi + MonthsCurrComp, 
                       family = "binomial", data = train_master_woe)

summary(lr_master_woe_4)
sort(vif(lr_master_woe_4))

# Removing no60DPD6Months since it has a high VIF
lr_master_woe_5 <- glm(PerformanceTag ~ no90DPD6Months +  no90DPD12Months + no30DPD12Months + AvgCCin12Months +  
                         PLTradesin12Months + Inq12Months + OpenHomeLoan + TotalTrades + Age + NoofDep + Income + 
                         Education + Profession + MonthsCurrResi + MonthsCurrComp, family = "binomial", data = train_master_woe)

summary(lr_master_woe_5)
sort(vif(lr_master_woe_5))

# Removing OpenHomeLoan since it is highly insignificant
lr_master_woe_6 <- glm(PerformanceTag ~ no90DPD6Months +  no90DPD12Months + no30DPD12Months + AvgCCin12Months +  
                         PLTradesin12Months + Inq12Months + TotalTrades + Age + NoofDep + Income + 
                         Education + Profession + MonthsCurrResi + MonthsCurrComp, family = "binomial", data = train_master_woe)

summary(lr_master_woe_6)
sort(vif(lr_master_woe_6))

# Removing no90DPD6Months since it is correlated with other similar variable no90DPD12Months
lr_master_woe_7 <- glm(PerformanceTag ~ no90DPD12Months + no30DPD12Months + AvgCCin12Months +  
                         PLTradesin12Months + Inq12Months + TotalTrades + Age + NoofDep + Income + 
                         Education + Profession + MonthsCurrResi + MonthsCurrComp, family = "binomial", data = train_master_woe)

summary(lr_master_woe_7)
sort(vif(lr_master_woe_7))

# Note that all the VIF values are in considerable range. Hence, we will do further variable selection just on the basis of
# significance level

# Removing TotalTrades since it is insgnificant
lr_master_woe_8 <- glm(PerformanceTag ~ no90DPD12Months + no30DPD12Months + AvgCCin12Months +  
                         PLTradesin12Months + Inq12Months + Age + NoofDep + Income + 
                         Education + Profession + MonthsCurrResi + MonthsCurrComp, family = "binomial", data = train_master_woe)

summary(lr_master_woe_8)

# Removing Education since it is slightly insgnificant
lr_master_woe_9 <- glm(PerformanceTag ~ no90DPD12Months + no30DPD12Months + AvgCCin12Months +  PLTradesin12Months + 
                         Inq12Months + Age + NoofDep + Income + Profession + MonthsCurrResi + MonthsCurrComp, 
                       family = "binomial", data = train_master_woe)

summary(lr_master_woe_9)


## All the remaining variables are quite significant. Hence we would keep that in our final model

final_lr_master_woe <- lr_master_woe_9

#--------------------------------------------------------------------------------------------------------------------------

## Model Evaluation

str(test_master_woe[, -c(1,ncol(test_master_woe))])         # Dropping PerformanceTag and Application ID before testing
predictions_logit_master_woe <- predict(final_lr_master_woe, newdata = test_master_woe[, -c(1,ncol(test_master_woe))], type = "response")
summary(predictions_logit_master_woe)

actual_response_lr_master_woe <- factor(ifelse(test_master_woe$PerformanceTag == 1, "yes", "no"))
str(actual_response_lr_master_woe)

model_evaluator(predicted_logits = predictions_logit_master_woe, actual_response =  actual_response_lr_master_woe, evaluation_for = "master_woe")

### Evaluator Outputs:


# Evaluator Graph
evaluator_graph_master_woe

# Optimal_Cutoff --- 0.49
optimal_cutoff_master_woe

# COnfusion_Matrix
confusion_matrix_master_woe

# Accuracy --- 68.12%
accuracy_master_woe

# Sensitivity --- 69.6%
sensitivity_master_woe

#Specificity --- 68%
specificity_master_woe

###Performance Graphs : master woe dataset
#roc curve 
perf_eva(label = test_master_woe$PerformanceTag, pred=predictions_logit_master_woe,type='roc',title="Master woe dataset")
#KS 
perf_eva(label = test_master_woe$PerformanceTag, pred=predictions_logit_master_woe,type='ks',title="Master woe dataset")
#LIFT
perf_eva(label = test_master_woe$PerformanceTag, pred=predictions_logit_master_woe,type='lift',title="Master woe dataset") 


############################################################################################################################

### Weighted Logistic Regression : WOE Demographic Dataset ###

############################################################################################################################
# splitting the data between train and test
set.seed(1)

split_indices <- sample.split(demo_woe_final$PerformanceTag, SplitRatio = 0.70)

train_demo_woe_wt <- demo_woe_final[split_indices, ]

test_demo_woe_wt <- demo_woe_final[!split_indices, ]

nrow(train_demo_woe_wt)/nrow(demo_woe_final)

nrow(test_demo_woe_wt)/nrow(demo_woe_final)

# Removing Application ID from train_demo_woe_wt
colnames(train_demo_woe_wt)
train_demo_woe_wt <- train_demo_woe_wt[, -ncol(train_demo_woe_wt)]

str(train_demo_woe_wt)
str(test_demo_woe_wt)

# Count % of Defaults and Non-Defaults

round(prop.table(table(train_demo_woe_wt$PerformanceTag)),2)

#   0     1 
# 0.94  0.06 

# Clearly, the data is highly biased towards Non-Defaulters, which accounts to nearly 94%. And only 6% of the total
# data are defaulters. 

# Hence, to get balanced results, we have to find a way around this unbalanced data. And we can do that by applying the
# idea of weighting which is related to sampling. All we need is sufficient number of 1's for the maximum likelihood to converge.

# So we will proceed in the following manner:

# We will Sample 100 % of the 1's (i.e. all the default transactions) and 10% of the 0's and use a weight of 1 for defaults 
# (fraud) transaction and a weight of 10 for non-default (good) transactions. 



# Subsetting the data as defaults and non-defaults
train_demo_woe_Perf0 <- subset(train_demo_woe_wt, PerformanceTag == 0)
train_demo_woe_Perf1 <- subset(train_demo_woe_wt, PerformanceTag == 1)

# Verifying the subsets
unique(train_demo_woe_Perf0$PerformanceTag)
unique(train_demo_woe_Perf1$PerformanceTag)


# Sampling 10% of 0's
Perf0_index_demo_woe <- sample(1:nrow(train_demo_woe_Perf0), nrow(train_demo_woe_Perf0)*0.1, replace = FALSE)

# Checking if all indexes are unique
sum(duplicated(train_demo_woe_Perf0))

train_demo_woe_Perf0 <- train_demo_woe_Perf0[Perf0_index_demo_woe, ]

# Structure of subsets
str(train_demo_woe_Perf0)
str(train_demo_woe_Perf1)

train_demo_woe_weighted <- rbind(train_demo_woe_Perf0, train_demo_woe_Perf1)

## Adding a weights variable, having weights = 1 for defaulters and weights = 10 for non-defaulters
train_demo_woe_weighted$weights <- ifelse(train_demo_woe_weighted$PerformanceTag == 0, 10, 1)

## Shuffling the dataset
train_demo_woe_weighted <- train_demo_woe_weighted[sample(nrow(train_demo_woe_weighted), replace = FALSE), ]

## Checking if we incorporated any duplication during data preparation for weighted Logistic Regression
sum(duplicated(train_demo_woe_weighted))

## Final Training Data for Weighted Logistic Regression
str(train_demo_woe_weighted)
View(train_demo_woe_weighted)

round(prop.table(table(train_demo_woe_weighted$PerformanceTag)),2) ## The data is reasonably balanced now

# Model Building through weighted logistic Regression Method

########################################################################

## Model Building: 

#Initial model
lr_demo_woe_wt_1 <- glm(PerformanceTag ~ .-weights, family = "binomial", data = train_demo_woe_weighted, weights = weights)

summary(lr_demo_woe_wt_1)


# # STEPAIC method for quickly removing some of the insignificant variables
set.seed(1)

lr_demo_woe_wt_2 <- stepAIC(lr_demo_woe_wt_1, direction = "both")

# The following variables were found to be significant as per STEPAIC Algorithm
lr_demo_woe_wt_2 <- glm(PerformanceTag ~ Age + NoOfDep + Income + Education + Profession + 
                          NoOfMthsInCurResi + NoOfMthsInCurJob, 
                        family = "binomial", data = train_demo_woe_weighted,weights=weights)

summary(lr_demo_woe_wt_2)
sort(vif(lr_demo_woe_wt_2))

# Since all vif's are in the same considerable range i.e. below 2, and all variables are significant

final_lr_demo_woe_wt <- lr_demo_woe_wt_2

# Predicting probabilities of responding for the test data
str(test_demo_woe_wt[, -c(1,ncol(test_demo_woe_wt))])            # Dropping PerformanceTag and Application ID before testing
predictions_logit_wt_demo_woe <- predict(final_lr_demo_woe_wt, newdata = test_demo_woe_wt[, -c(1,ncol(test_demo_woe_wt))], type = "response")
summary(predictions_logit_wt_demo_woe)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

#---------------------------------------------------------    

actual_response_lr_test_demo_woe_wt <- factor(ifelse(test_demo_woe_wt$PerformanceTag == 1, "yes", "no"))
str(actual_response_lr_test_demo_woe_wt)
model_evaluator(predicted_logits = predictions_logit_wt_demo_woe, actual_response =  actual_response_lr_test_demo_woe_wt, evaluation_for = "demo_woe_wt")

### Evaluator Outputs:


# Evaluator Graph
evaluator_graph_demo_woe_wt

# Optimal_Cutoff --- 0.05838604
optimal_cutoff_demo_woe_wt

# COnfusion_Matrix
confusion_matrix_demo_woe_wt

# Accuracy --- 0.6122
accuracy_demo_woe_wt

# Sensitivity --- 0.62461
sensitivity_demo_woe_wt

#Specificity --- 0.61144 
specificity_demo_woe_wt

###Performance Graphs : demo woe dataset weighted
#roc curve 
perf_eva(label = test_demo_woe_wt$PerformanceTag, pred=predictions_logit_wt_demo_woe,type='roc',title="Demo woe dataset weighted")
#KS 
perf_eva(label = test_demo_woe_wt$PerformanceTag, pred=predictions_logit_wt_demo_woe,type='ks',title="Demo woe dataset weighted")
#LIFT
perf_eva(label = test_demo_woe_wt$PerformanceTag, pred=predictions_logit_wt_demo_woe,type='lift',title="Demo woe dataset weighted") 


############################################################################################################################

### Weighted Logistic Regression : WOE MASTER Dataset ###

############################################################################################################################
# splitting the data between train and test
set.seed(1)

split_indices <- sample.split(master_woe_final$PerformanceTag, SplitRatio = 0.70)

train_master_woe_wt <- master_woe_final[split_indices, ]

test_master_woe_wt <- master_woe_final[!split_indices, ]

nrow(train_master_woe_wt)/nrow(master_woe_final)

nrow(test_master_woe_wt)/nrow(master_woe_final)

# Removing Application ID from train_master_woe
colnames(train_master_woe_wt)
train_master_woe_wt <- train_master_woe[, -ncol(train_master_woe_wt)]


str(train_master_woe_wt)
str(test_master_woe_wt)

# Count % of Defaults and Non-Defaults

round(prop.table(table(train_master_woe_wt$PerformanceTag)),2)

#   0     1 
# 0.94  0.06 

# Clearly, the data is highly biased towards Non-Defaulters, which accounts to nearly 94%. And only 6% of the total
# data are defaulters. 

# Hence, to get balanced results, we have to find a way around this unbalanced data. And we can do that by applying the
# idea of weighting which is related to sampling. All we need is sufficient number of 1's for the maximum likelihood to converge.

# So we will proceed in the following manner:

# We will Sample 100 % of the 1's (i.e. all the default transactions) and 10% of the 0's and use a weight of 1 for defaults 
# (fraud) transaction and a weight of 10 for non-default (good) transactions. 



# Subsetting the data as defaults and non-defaults
train_master_woe_Perf0 <- subset(train_master_woe_wt, PerformanceTag == 0)
train_master_woe_Perf1 <- subset(train_master_woe_wt, PerformanceTag == 1)

# Verifying the subsets
unique(train_master_woe_Perf0$PerformanceTag)
unique(train_master_woe_Perf1$PerformanceTag)


# Sampling 10% of 0's
Perf0_index_master_woe <- sample(1:nrow(train_master_woe_Perf0), nrow(train_master_woe_Perf0)*0.1, replace = FALSE)

# Checking if all indexes are unique
sum(duplicated(train_master_woe_Perf0))

train_master_woe_Perf0 <- train_master_woe_Perf0[Perf0_index_master_woe, ]

# Structure of subsets
str(train_master_woe_Perf0)
str(train_master_woe_Perf1)

train_master_woe_weighted <- rbind(train_master_woe_Perf0, train_master_woe_Perf1)

## Adding a weights variable, having weights = 1 for defaulters and weights = 10 for non-defaulters
train_master_woe_weighted$weights <- ifelse(train_master_woe_weighted$PerformanceTag == 0, 10, 1)

## Shuffling the dataset
train_master_woe_weighted <- train_master_woe_weighted[sample(nrow(train_master_woe_weighted), replace = FALSE), ]

## Checking if we incorporated any duplication during data preparation for weighted Logistic Regression
sum(duplicated(train_master_woe_weighted))

## Final Training Data for Weighted Logistic Regression
str(train_master_woe_weighted)
View(train_master_woe_weighted)

round(prop.table(table(train_master_woe_weighted$PerformanceTag)),2) ## The data is reasonably balanced now

# Model Building through weighted logistic Regression Method

########################################################################

## Model Building: 

#Initial model
lr_master_woe_wt_1 <- glm(PerformanceTag ~ .-weights, family = "binomial", data = train_master_woe_weighted, weights = weights)

summary(lr_master_woe_wt_1)


# # STEPAIC method for quickly removing some of the insignificant variables
set.seed(1)

lr_master_woe_wt_2 <- stepAIC(lr_master_woe_wt_1, direction = "both")

# The following variables were found to be significant as per STEPAIC Algorithm
lr_demo_master_wt_2 <- glm(PerformanceTag ~ no90DPD6Months + no60DPD6Months + no90DPD12Months + 
                             no30DPD12Months + AvgCCin12Months + Tradesin12Months + PLTradesin6Months + 
                             PLTradesin12Months + Inq12Months + OpenHomeLoan + TotalTrades + 
                             Age + NoofDep + Income + Education + Profession + MonthsCurrResi + 
                             MonthsCurrComp,
                           family = "binomial", data = train_master_woe_weighted,weights=weights)

summary(lr_master_woe_wt_2)
sort(vif(lr_master_woe_wt_2))

#removing Tradesin12Months due to high vif and less significance
lr_master_woe_wt_3 <- glm(PerformanceTag ~ no90DPD6Months + no60DPD6Months + no90DPD12Months + 
                            no30DPD12Months + AvgCCin12Months + PLTradesin6Months + 
                            PLTradesin12Months + Inq12Months + OpenHomeLoan + TotalTrades + 
                            Age + NoofDep + Income + Education + Profession + MonthsCurrResi + 
                            MonthsCurrComp,
                          family = "binomial", data = train_master_woe_weighted,weights=weights)

summary(lr_master_woe_wt_3)
sort(vif(lr_master_woe_wt_3))

lr_master_woe_wt_4 <- glm(PerformanceTag ~ no90DPD6Months + no90DPD12Months + 
                            no30DPD12Months + AvgCCin12Months + PLTradesin6Months + 
                            PLTradesin12Months + Inq12Months + OpenHomeLoan + TotalTrades + 
                            Age + NoofDep + Income + Education + Profession + MonthsCurrResi + 
                            MonthsCurrComp,
                          family = "binomial", data = train_master_woe_weighted,weights=weights)

summary(lr_master_woe_wt_4)
sort(vif(lr_master_woe_wt_4))

cor(train_master_woe_weighted$no30DPD12Months,train_master_woe_weighted$no90DPD12Months)
#82% so removing no90DPD12Months
lr_master_woe_wt_5 <- glm(PerformanceTag ~ no90DPD6Months + 
                            no30DPD12Months + AvgCCin12Months + PLTradesin6Months + 
                            PLTradesin12Months + Inq12Months + OpenHomeLoan + TotalTrades + 
                            Age + NoofDep + Income + Education + Profession + MonthsCurrResi + 
                            MonthsCurrComp,
                          family = "binomial", data = train_master_woe_weighted,weights=weights)

summary(lr_master_woe_wt_5)
sort(vif(lr_master_woe_wt_5))

#Removing PLTradesin12Months
lr_master_woe_wt_6 <- glm(PerformanceTag ~ no90DPD6Months + 
                            no30DPD12Months + AvgCCin12Months + PLTradesin6Months + 
                            Inq12Months + OpenHomeLoan + TotalTrades + 
                            Age + NoofDep + Income + Education + Profession + MonthsCurrResi + 
                            MonthsCurrComp,
                          family = "binomial", data = train_master_woe_weighted,weights=weights)

summary(lr_master_woe_wt_6)
sort(vif(lr_master_woe_wt_6))

#Removing no90DPD6Months
lr_master_woe_wt_7 <- glm(PerformanceTag ~
                            no30DPD12Months + AvgCCin12Months + PLTradesin6Months + 
                            Inq12Months + OpenHomeLoan + TotalTrades + 
                            Age + NoofDep + Income + Education + Profession + MonthsCurrResi + 
                            MonthsCurrComp,
                          family = "binomial", data = train_master_woe_weighted,weights=weights)

summary(lr_master_woe_wt_7)
sort(vif(lr_master_woe_wt_7))

#Removing TotalTrades
lr_master_woe_wt_8 <- glm(PerformanceTag ~
                            no30DPD12Months + AvgCCin12Months + PLTradesin6Months + 
                            Inq12Months + OpenHomeLoan + 
                            Age + NoofDep + Income + Education + Profession + MonthsCurrResi + 
                            MonthsCurrComp,
                          family = "binomial", data = train_master_woe_weighted,weights=weights)

summary(lr_master_woe_wt_8)
sort(vif(lr_master_woe_wt_8))

#Removing MonthsCurrResi as it is less significant
lr_master_woe_wt_9 <- glm(PerformanceTag ~
                            no30DPD12Months + AvgCCin12Months + PLTradesin6Months + 
                            Inq12Months + OpenHomeLoan + 
                            Age + NoofDep + Income + Education + Profession + 
                            MonthsCurrComp,
                          family = "binomial", data = train_master_woe_weighted,weights=weights)

summary(lr_master_woe_wt_9)
sort(vif(lr_master_woe_wt_9))

#Removing Education
lr_master_woe_wt_10 <- glm(PerformanceTag ~
                             no30DPD12Months + AvgCCin12Months + PLTradesin6Months + 
                             Inq12Months + OpenHomeLoan + 
                             Age + NoofDep + Income + Profession + 
                             MonthsCurrComp,
                           family = "binomial", data = train_master_woe_weighted,weights=weights)

summary(lr_master_woe_wt_10)
sort(vif(lr_master_woe_wt_10))

#Removing Age
lr_master_woe_wt_11 <- glm(PerformanceTag ~
                             no30DPD12Months + AvgCCin12Months + PLTradesin6Months + 
                             Inq12Months + OpenHomeLoan + 
                             NoofDep + Income + Profession + 
                             MonthsCurrComp,
                           family = "binomial", data = train_master_woe_weighted,weights=weights)

summary(lr_master_woe_wt_11)
sort(vif(lr_master_woe_wt_11))
# Since all vif's are in the same considerable range i.e. below 2, and all variables are significant

final_lr_master_woe_wt <- lr_master_woe_wt_11

# Predicting probabilities of responding for the test data
str(test_master_woe_wt[, -c(1,ncol(test_master_woe_wt))])         # Dropping PerformanceTag and Application ID before testing
predictions_logit_wt_master_woe <- predict(final_lr_master_woe_wt, newdata = test_master_woe_wt[, -c(1,ncol(test_master_woe_wt))], type = "response")
summary(predictions_logit_wt_master_woe)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

#---------------------------------------------------------    

actual_response_lr_test_master_woe_wt <- factor(ifelse(test_master_woe_wt$PerformanceTag == 1, "yes", "no"))
str(actual_response_lr_test_master_woe_wt)
model_evaluator(predicted_logits = predictions_logit_wt_master_woe, actual_response =  actual_response_lr_test_master_woe_wt, evaluation_for = "master_woe_wt")

### Evaluator Outputs:


# Evaluator Graph
evaluator_graph_master_woe_wt

# Optimal_Cutoff --- 0.05305391
optimal_cutoff_master_woe_wt

# COnfusion_Matrix
confusion_matrix_master_woe_wt

# Accuracy --- 0.7013
accuracy_master_woe_wt

# Sensitivity --- 0.68458
sensitivity_master_woe_wt

#Specificity --- 0.70235  
specificity_master_woe_wt

###Performance Graphs : master woe dataset weighted
#roc curve 
perf_eva(label = test_master_woe_wt$PerformanceTag, pred=predictions_logit_wt_master_woe,type='roc',title="Master woe dataset weighted")
#KS 
perf_eva(label = test_master_woe_wt$PerformanceTag, pred=predictions_logit_wt_master_woe,type='ks',title="Master woe dataset weighted")
#LIFT
perf_eva(label = test_master_woe_wt$PerformanceTag, pred=predictions_logit_wt_master_woe,type='lift',title="Master woe dataset weighted") 



#############################################################################################
#### DEMO DATASET RANDOM FORSET-------------------------------------------
#############################################################################################
library(dummies)
library(devtools)
library(randomForest)
library(ROCR)

################# DEMO DATASET #####################################################

demo_data_final2 <- demo_ds

# standardisation
# Normalising continuous dataset
# Scaling variables 
# creating a dataframe of numerical datasets
demo_data_final2$Age<- scale(demo_data_final2$Age) 
demo_data_final2$Income<- scale(demo_data_final2$Income) 
demo_data_final2$NoOfMthsInCurResi <- scale(demo_data_final2$NoOfMthsInCurResi) 
demo_data_final2$NoOfMthsInCurJob <- scale(demo_data_final2$NoOfMthsInCurJob) 
demo_data_final2$NoOfDep<- scale(demo_data_final2$NoOfDep) 


# creating a dataframe - categorical features
demo_data_fact2<- demo_data_final2[, DemoData_factor_variables2]
str(demo_data_fact2)

# creating dummy variables - factor attributes
dummies<- data.frame(sapply(demo_data_fact2, 
                            function(x) data.frame(model.matrix(~x-1,data=demo_data_fact2))[,-1]))

str(demo_data_final2)

# Final Demographic dataset
demo_data_final3<- demo_data_final2[, c("Application.ID","Performance.Tag","Age","NoOfDep","Income", "NoOfMthsInCurResi","NoOfMthsInCurJob" )]
demo_ds_final<- cbind(demo_data_final3,dummies) 
View(demo_ds_final)

#Creating copy of demo data
demo_final <- demo_ds_final

numericcols <- c( 'Age', 'NoOfDep', 'Income',
                  'NoOfMthsInCurResi','NoOfMthsInCurJob')

factorcols <- c( 'Gender', 'Maritalstatus', 'Education.xMasters','Education.xOthers',
                 'Education.xPhd', 'Education.xProfessional',
                 'Profession.xSE','Profession.xSE_PROF',
                 'TypeOfResi.xLiving.with.Parents','TypeOfResi.xOthers',
                 'TypeOfResi.xOwned','TypeOfResi.xRented','Performance.Tag')

str(demo_ds_final)

demo_ds_final[, numericcols] <- lapply(numericcols, function(x) as.numeric(as.character(demo_ds_final[, x])))
demo_ds_final[, factorcols] <- lapply(factorcols, function(x) as.factor(as.character(demo_ds_final[, x])))

#Remove NA's from Performance Tag
demo_ds_final <- demo_ds_final[which(!is.na(demo_ds_final$Performance.Tag)),]

# Shuffle the data
shuffledata2 <- demo_ds_final[sample(nrow(demo_ds_final)), ]

# Split the data into train and test
set.seed(71)

ntrain <- as.integer(nrow(shuffledata2)*0.8)
traindata <- shuffledata2[1:ntrain, ]
testdata <- shuffledata2[(ntrain+1):nrow(shuffledata2), ]

################# Build RandomForest ###########################################
library(randomForest)
#memory.limit(56000)
data.rf <- randomForest(Performance.Tag ~ ., data=traindata, proximity=FALSE,
                        ntree=1000, mtry=5, do.trace=TRUE, na.action=na.omit)
summary(data.rf)


testPred <- predict(data.rf, newdata=testdata, type = "prob")
summary(testPred[,2])
summary(testPred[,1])

varImpPlot(data.rf)

# Cutoff for randomforest to assign yes or no
perform_fn_rf <- function(cutoff,tdata, predData) 
{
  predicted_response <- as.factor(ifelse(predData[, 2] >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, tdata$Performance.Tag, positive = "0")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}
# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
s = seq(.01,.99,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn_rf(s[i], testdata, testPred)
}  
#################################plotting cutoffs #################################

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.5,0.9,col=c(1.5,"darkgreen",3.5,"darkred"),lwd=c(1.5,1.5,1.5,1.5),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.08)]
cutoff
#cutoff value of 5.5% for final model
m_predicted_response_rf <- factor(ifelse(testPred[, 2] >= cutoff, "1", "0"))
summary(m_predicted_response_rf)
conf_final_rf <- confusionMatrix(m_predicted_response_rf, testdata$Performance.Tag, positive = "0")

acc <- conf_final_rf$overall[1]
sens <- conf_final_rf$byClass[1]
spec <- conf_final_rf$byClass[2]

acc
#Accuracy 
#0.587

sens
#Sensitivity 
#0.582

spec
#Specificity 
#0.658

################ Model Evaluation RANDOM FOREST ##########################

#probabilities and response variables to the test data
testdata$predicted_probs <- testPred[,2]
testdata$predicted_response <- m_predicted_response_rf

# Create new dataframe "test_predictions_rf"
test_predictions_rf <- testdata[, c("Performance.Tag", "predicted_probs", "predicted_response")]
str(test_predictions_rf)
summary(test_predictions_rf$Performance.Tag)
summary(test_predictions_rf$predicted_response)
response_rate <- table(testdata$Performance.Tag)[2]/(table(testdata$Performance.Tag)[1] + table(testdata$Performance.Tag)[2])

# sorting the probabilities in decreasing order 
test_predictions_rf <- test_predictions_rf[order(test_predictions_rf$predicted_probs, decreasing = T), ]

##################################### plotting the lift chart #####################################################
lift <- function(labels , predicted_prob, groups=10) {
  
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

View(test_predictions_rf)
LG = lift(test_predictions_rf$Performance.Tag, test_predictions_rf$predicted_probs, groups = 10)

####################################################### Plotting Gain Chart #############################################

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")
# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")

View(LG)
######################################## Area Under Curve ####################################
library("ROCR")
e<- as.numeric(testdata$predicted_response)
f<- as.numeric(testdata$Performance.Tag)

model_score_masterdataset <- prediction(e,f)
model_perf_master <- performance( model_score_masterdataset, "tpr", "fpr")

plot(model_perf_master, colorize=T , main= "ROC Curve" , ylab="Sensitivity" , xlab= "1-Specificity" )
abline(e=0,f=1)

# Area Under Curve (AUC)
auc <- performance(model_score_masterdataset,"auc")
auc <- unlist(slot(auc,"y.values"))
auc
# 0.6203994

#KS Statistics
ks_table_masterdataset <- attr(model_perf_master, "y.values")[[1]] - (attr(model_perf_master, "x.values")[[1]])
ks_masterdataset = max(ks_table_masterdataset)
ks_masterdataset 
# 0.2407988

#############################################################################################
####-- Master DataSet RANDOM FOREST-------------------------------------------
#############################################################################################
m_data_rf<-master_ds
str(m_data_rf)

numericcols <- c('no90DPD6Months', 'no60DPD6Months', 'no30DPD6Months', 'no90DPD12Months', 'no60DPD12Months', 'no30DPD12Months', 
                 'AvgCCin12Months', 'Tradesin6Months', 'Tradesin12Months', 'PLTradesin6Months', 'PLTradesin12Months',
                 'Inq6Months', 'Inq12Months', 'Age', 'NoOfDep', 'Income',
                 'NoOfMthsInCurResi','NoOfMthsInCurJob')

factorcols <- c('OpenAutoLoan', 'OpenHomeLoan', 'Gender', 'Maritalstatus', 'Education', 'Profession','TypeOfResi','Performance.Tag')

m_data_rf[, numericcols] <- lapply(numericcols, function(x) as.numeric(as.character(m_data_rf[, x])))
m_data_rf[, factorcols] <- lapply(factorcols, function(x) as.factor(as.character(m_data_rf[, x])))

#Remove NA's for Performance Tag
m_data_rf <- m_data_rf[which(!is.na(m_data_rf$Performance.Tag)),]

# Shuffle the data
shuffledata <- m_data_rf[sample(nrow(m_data_rf)), ]

# Split the data into train and test
set.seed(71)

ntrain <- as.integer(nrow(shuffledata)*0.8)
traindata <- shuffledata[1:ntrain, ]
testdata <- shuffledata[(ntrain+1):nrow(shuffledata), ]

########################### Build RandomForest ##############################################
library(randomForest)
data.rf <- randomForest(Performance.Tag ~ ., data=traindata, proximity=FALSE,
                        ntree=1000, mtry=5, do.trace=TRUE, na.action=na.omit)
summary(data.rf)
testPred <- predict(data.rf, newdata=testdata, type = "prob")
summary(testPred[,2])
summary(testPred[,1])

varImpPlot(data.rf)

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff,tdata, predData) 
{
  predicted_response <- as.factor(ifelse(predData[, 2] >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, tdata$Performance.Tag, positive = "0")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}
# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
s = seq(.01,.99,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn_rf(s[i], testdata, testPred)
}  
#################################plotting cutoffs #################################
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.5,0.9,col=c(1.5,"darkgreen",3.5,"darkred"),lwd=c(1.5,1.5,1.5,1.5),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<=0.03)]
cutoff
# Optimal cutoff value 
m_predicted_response_rf <- factor(ifelse(testPred[, 2] >= cutoff, "1", "0"))
summary(m_predicted_response_rf)
conf_final_rf <- confusionMatrix(m_predicted_response_rf, testdata$Performance.Tag, positive = "0")
conf_final_rf
acc <- conf_final_rf$overall[1]
sens <- conf_final_rf$byClass[1]
spec <- conf_final_rf$byClass[2]

acc
#Accuracy 
#0.6935

sens
#Sensitivity 
#0.6948 

spec
#Specificity 
#0.6740

# ============================================================================
# Model Evaluation RANDOM FOREST
# ============================================================================

# probabilities and response variables to the test data
testdata$predicted_probs <- testPred[,2]
testdata$predicted_response <- m_predicted_response_rf

# Create new dataframe "test_predictions_rf"
test_predictions_rf <- testdata[, c("Performance.Tag", "predicted_probs", "predicted_response")]

str(test_predictions_rf)
summary(test_predictions_rf$Performance.Tag)
summary(test_predictions_rf$predicted_response)

response_rate <- table(testdata$Performance.Tag)[2]/(table(testdata$Performance.Tag)[1] + table(testdata$Performance.Tag)[2])

# sorting the probabilities in decreasing order 
test_predictions_rf <- test_predictions_rf[order(test_predictions_rf$predicted_probs, decreasing = T), ]

##################################### plotting the lift chart #####################################################

lift <- function(labels , predicted_prob, groups=10) {
  
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

View(test_predictions_rf)
LG = lift(test_predictions_rf$Performance.Tag, test_predictions_rf$predicted_probs, groups = 10)

####################################################### Plotting Gain Chart #############################################

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")
# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")

View(LG)
# The Cumulative Lift is of avg 1.5992994 for top 5th deciles

library("ROCR")
a<- as.numeric(testdata$predicted_response)
b<- as.numeric(testdata$Performance.Tag)

model_score_masterdataset <- prediction(a,b)
model_perf_master <- performance( model_score_masterdataset, "tpr", "fpr")

plot(model_perf_master, colorize=T , main= "ROC Curve" , ylab="Sensitivity" , xlab= "1-Specificity" )
abline(a=0,b=1)

# Area Under Curve (AUC)
auc <- performance(model_score_masterdataset,"auc")
auc <- unlist(slot(auc,"y.values"))
auc
# 0.6843847

#KS Statistics
ks_table_masterdataset <- attr(model_perf_master, "y.values")[[1]] - (attr(model_perf_master, "x.values")[[1]])
ks_masterdataset = max(ks_table_masterdataset)
ks_masterdataset 
# 0.3687694


###########################################################################################################################

### Neural Network --- Demographic ###

###########################################################################################################################

#######Data Preparation########
demo_features_nn <- subset(demo_ds, select = c(2,5,6,10,11))
demo_factors_nn <- subset(demo_ds, select = c(3,4,7,8,9))
str(demo_features_nn)
str(demo_factors_nn)

# feature standardization is done on numeric variables in order to nullify the impact caused due to the difference of ranges in case of numeric variables.
#example : range of age and range of income
demo_features_nn <- data.frame(sapply(demo_features_nn, function(x) scale(x, center = min(x) , scale = max(x) - min(x))))

# creating dummy variables for factor variables
dummies_demo_nn <- data.frame(sapply(demo_factors_nn, function(x) data.frame(model.matrix(~x-1,data =demo_factors_nn))[,-1]))

### Merging the above prepared files to create a master file for model building
demo_ds_nn <- cbind(demo_features_nn, dummies_demo_nn)
demo_ds_nn <- cbind(demo_ds$Performance.Tag, demo_ds_nn) # Adding target variable i.e. Performance tag to master dataset.
colnames(demo_ds_nn)[1] <- "Performance Tag"

demo_ds_nn$`Performance Tag` <- as.numeric(levels(demo_ds_nn$`Performance Tag`))[demo_ds_nn$`Performance Tag`]

str(demo_ds_nn)
View(demo_ds_nn)


set.seed(1)

split_indices_nn <- sample.split(demo_ds_nn$`Performance Tag`, SplitRatio = 0.70)

train_demo_nn <- demo_ds_nn[split_indices_nn, ]

test_demo_nn <- demo_ds_nn[!split_indices_nn, ]

nrow(train_demo_nn)/nrow(demo_ds_nn)

nrow(test_demo_nn)/nrow(demo_ds_nn)

str(train_demo_nn)
str(test_demo_nn)

# Neural Networks

# vars <- paste(c(colnames(train_master_nn[-1])), collapse = "+")
# vars <- paste(c("`Performance Tag` ~", vars), collapes = "")
# f <- formula(vars)

n <- names(train_demo_nn)
f <- as.formula(paste("`Performance Tag` ~", paste(n[-1], collapse = " + ")))
f

set.seed(1)

## Setting up Parallel Processing
library(doParallel)
c1 <- makeCluster(detectCores())
registerDoParallel(c1)

##########################################################################################################################
# Different Models that we tried out. (We have only kept the final model uncommented)

### Out of the below tried neural networks, nn_master_v2 worked best with a reasonable training time.
### Hence, commenting out other models, and keeping nn_master_v6 as final neural network model.

# nn_demo_v1 <- neuralnet(f, data = train_demo_nn, algorithm = "rprop+", hidden = 1, threshold = 0.01, 
#                         lifesign = 'full', linear.output = FALSE)
# 
# 
# nn_demo_v2 <- neuralnet(f, data = train_demo_nn, algorithm = "rprop+", hidden = 2, threshold = 0.03,
#                          lifesign = 'full', linear.output = FALSE)
# 
# 
# nn_demo_v3 <- neuralnet(f, data = train_demo_nn, algorithm = "rprop+", hidden = 3, threshold = 0.05,
#                         lifesign = 'full', linear.output = FALSE)
# 
# 
# # Time Elapsed -- time: 1.4 hours  , n_iterations: 376661; error: 1372.03841
# nn_demo_v4 <- neuralnet(f, data = train_demo_nn, algorithm = "backprop", learningrate = 0.001,
#                         lifesign = 'full', linear.output = FALSE, stepmax = 3e6)   
# 
# nn_demo_v5 <- neuralnet(f, data = train_demo_nn, algorithm = "rprop+", hidden = 5, threshold = 0.05,
#                         lifesign = 'full', linear.output = FALSE)

nn_demo_v6 <- neuralnet(f, data = train_demo_nn, algorithm = "rprop+", hidden = 5, threshold = 0.01,
                        lifesign = 'full', linear.output = FALSE, startweights = c(), rep = 5, stepmax = 2e6)

# start_weights <- nn_demo_v6$weights
# 
# 
# nn_demo_v7 <- neuralnet(f, data = train_demo_nn, algorithm = "rprop+", hidden = 5, threshold = 0.001,
#                         lifesign = 'full', linear.output = FALSE, startweights = start_weights)
# 

# nn_demo_v5 <- neuralnet(f, data = train_master_nn, algorithm = "rprop+", hidden = 1, threshold = 0.01,
#                            +                           lifesign = 'full', linear.output = FALSE)

stopCluster(c1)
##########################################################################################################################

# Model which we finalized
nn_demo_final <- nn_demo_v6



#--------------------------------------------------------------------------------------------------------------------------

## Model Evaluation

#------------------------------------------------------------------------------------------------------------------------


str(train_demo_nn)
str(test_demo_nn)
nn_demo_final$result.matrix[1,]

# Error Results of different reps
# 1           2           3           4 
# 1356.835933 1356.846963 1342.677103 1353.003731 

# Computing Predcitions
nn_out_demo <- compute(nn_demo_final, test_demo_nn[-1], rep = which.min(nn_demo_final$result.matrix[1,]))
summary(nn_out_demo)

predicted_probs_demo_nn <- nn_out_demo$net.result
head(predicted_probs_demo_nn)

actual_response_demo_nn <- factor(ifelse(test_demo_nn$`Performance Tag` == 1, "yes", "no"))
str(actual_response_demo_nn)

# Evaluating Model
model_evaluator(predicted_logits = predicted_probs_demo_nn, actual_response =  actual_response_demo_nn, evaluation_for = "demo_nn")

## Evaluator Outputs:


# Evaluator Graph
evaluator_graph_demo_nn

# Optimal_Cutoff --- 0.05698736458
optimal_cutoff_demo_nn

# COnfusion_Matrix
confusion_matrix_demo_nn

# Accuracy --- 67%
accuracy_demo_nn

# Sensitivity --- 53%
sensitivity_demo_nn

#Specificity --- 68%
specificity_demo_nn

eval_matrix_demo_nn



###########################################################################################################################

### Neural Network --- Master ###

###########################################################################################################################

#######Data Preparation########
str(master_ds)

master_features_nn <- subset(master_ds, select = c(2,5,6,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,27))
master_factors_nn <- subset(master_ds, select = c(3,4,7,8,9,25,28))

str(master_features_nn)
str(master_factors_nn)

# feature standardization is done on numeric variables in order to nullify the impact caused due to the difference of ranges in case of numeric variables.
#example : range of age and range of income
master_features_nn <- data.frame(sapply(master_features_nn, function(x) scale(x, center = min(x) , scale = max(x) - min(x))))

# creating dummy variables for factor variables
dummies_nn <- data.frame(sapply(master_factors_nn,function(x) data.frame(model.matrix(~x-1,data =master_factors_nn))[,-1]))

### Merging the above prepared files to create a master file for model building
master_ds_nn <- cbind(master_features_nn, dummies_nn)
master_ds_nn <- cbind(master_ds$Performance.Tag, master_ds_nn) # Adding target variable i.e. Performance tag to master dataset.
colnames(master_ds_nn)[1] <- "Performance Tag"

master_ds_nn$`Performance Tag` <- as.numeric(levels(master_ds_nn$`Performance Tag`))[master_ds_nn$`Performance Tag`]

str(master_ds_nn)
View(master_ds_nn)


set.seed(1)

split_indices_nn <- sample.split(master_ds_nn$`Performance Tag`, SplitRatio = 0.70)

train_master_nn <- master_ds_nn[split_indices_nn, ]

test_master_nn <- master_ds_nn[!split_indices_nn, ]

nrow(train_master_nn)/nrow(master_ds_nn)

nrow(test_master_nn)/nrow(master_ds_nn)

str(train_master_nn)
str(test_master_nn)

# Neural Networks

# vars <- paste(c(colnames(train_master_nn[-1])), collapse = "+")
# vars <- paste(c("`Performance Tag` ~", vars), collapes = "")
# f <- formula(vars)

n <- names(train_master_nn)
f <- as.formula(paste("`Performance Tag` ~", paste(n[-1], collapse = " + ")))
f

set.seed(1)

##########################################################################################################################
# Different Models that we tried out. (We have only kept the final model uncommented)


### Out of the below tried neural networks, nn_master_v2 worked best with a reasonable training time.
### Hence, commenting out other models, and keeping nn_master_v2 as final neural network model.

# nn_master_v1 <- neuralnet(f, data = train_master_nn, algorithm = "rprop+", hidden = 1, threshold = 0.01, 
#                           lifesign = 'full', linear.output = FALSE)


# nn_master_v2 <- neuralnet(f, data = train_master_nn, algorithm = "rprop+", hidden = 2, threshold = 0.03, 
#                           lifesign = 'full', linear.output = FALSE)


# nn_master_v3 <- neuralnet(f, data = train_master_nn, algorithm = "rprop+", hidden = 3, threshold = 0.05, 
#                           lifesign = 'full', linear.output = FALSE)
# 
# 

# nn_master_v4 <- neuralnet(f, data = train_master_nn, algorithm = "backprop", learningrate = 0.005,
#                           lifesign = 'full', linear.output = FALSE, stepmax = 10e6, threshold = 0.02)
# 
# nn_master_v5 <- neuralnet(f, data = train_master_nn, algorithm = "rprop+", hidden = 1, threshold = 0.05, 
#                            +                           lifesign = 'full', linear.output = FALSE)

nn_master_v6 <- neuralnet(f, data = train_master_nn, algorithm = "rprop+", hidden = 3, threshold = 0.01, 
                          lifesign = 'full', linear.output = FALSE, rep = 5)

##########################################################################################################################

# Model which we finalized

nn_master_final <- nn_master_v6




#--------------------------------------------------------------------------------------------------------------------------

## Model Evaluation
nn_master_final$result.matrix[1,]

# Results of different reps
# 1           2           3           4           5 
# 1220.853463 1234.215708 1233.593700 1234.351253 1218.330979 

# Computing Predictions
str(test_master_nn)
nn_out_master <- compute(nn_master_final, test_master_nn[-1], rep = which.min(nn_master_final$result.matrix[1,]))
summary(nn_out_master)

predicted_probs_master_nn <- nn_out_master$net.result
head(predicted_probs_master_nn)

actual_response_master_nn <- factor(ifelse(test_master_nn$`Performance Tag` == 1, "yes", "no"))
str(actual_response_master_nn)

# Evaluating Model
model_evaluator(predicted_logits = predicted_probs_master_nn, actual_response =  actual_response_master_nn, evaluation_for = "master_nn")

## Evaluator Outputs:


# Evaluator Graph
evaluator_graph_master_nn

# Optimal_Cutoff --- 0.0465
optimal_cutoff_master_nn

# COnfusion_Matrix
confusion_matrix_master_nn

# Accuracy --- 66%
accuracy_master_nn

# Sensitivity --- 70%
sensitivity_master_nn

#Specificity --- 66%
specificity_master_nn

##########################################################################################################################

### Final Model -- Weighted Logist Regression on WOE Dataset

##########################################################################################################################

# Out of all the models, the Weighted Logistic Regression on WOE data set turned out to be the best for following reasons:

# 1) It has nearly 70.2% accuracy and specificity, and around 68.5% Sensitivity. These numbers are quite decent as compared to other models.
# 2) There are a few models which outperform the above in some of the parameters by an edge, but the trade off for that
#    little improvement is lot of computing time. Hence, if we take that into account, then again this model clearly wins.

# Hence, we will finalize this model as our final model to be deployed. And we will work on further things using 
# the same model i.e. Weighted Logistic Regression on WOE Dataset.

### Lets evaluate the final model with some other metrics like ROC Curve, and K-S Statistics


###Performance Graphs : master woe dataset weighted

#roc curve 
perf_eva(label = test_master_woe_wt$PerformanceTag, pred=predictions_logit_wt_master_woe,type='roc',title="Master woe dataset weighted")

# plotting the lift chart

lift <- function(labels , predicted_prob, groups=10) {
  
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



# Create a Table of cumulative gain and lift
LG = lift(as.factor(test_master_woe_wt$PerformanceTag), predictions_logit_wt_master_woe, groups = 10)
View(LG)

# Gain Chart 
plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Lift Chart 
plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")

# As we can see. We were able to predict 75% of the defaulters in the 4th decile itself, and were able to achieve a gain of
# nearly 2% by 4th decile. Thats a fairly considerable accuracy of a prediction model. 

# Thus we are done with model building and evaluation here. Lets move on to analyze what financial benefits the model 
# can provide. 

############################################################################################################################

### Application Scorecard

###########################################################################################################################

# Final_Model -- Master_WOE_Wt

# Evaluation Values
accuracy_master_woe_wt
sensitivity_master_woe_wt
specificity_master_woe_wt

final_logits <- predictions_logit_wt_master_woe
final_test_data <- test_master_woe_wt
final_optimal_prob <- optimal_cutoff_master_woe_wt

# Taking subsets of all the Application ID's present in the test_master_woe_wt from the master_ds

final_eval_data <- final_eval_data[]
str(master_ds)
final_evaluation_data <- master_ds[which(master_ds$Application.ID %in% final_test_data$`master_woe$Application.ID`),]

str(final_evaluation_data)

score_data <- final_evaluation_data
str(score_data)

# Changing column name of "Application.ID" and "Performance.Tag"
colnames(score_data)[c(1,ncol(score_data))] <- c("Application_ID", "PerformanceTag")

# Changing the Performance Tag from factors to numeric
score_data$PerformanceTag <- as.numeric(levels(score_data$PerformanceTag))[score_data$PerformanceTag]
unique(score_data$PerformanceTag)
str(score_data)

# Calculating log(odds) fro non defaulters
score_data$p_bad <- final_logits
score_data$p_good <- 1 - score_data$p_bad
score_data$odd_goods <- score_data$p_good/score_data$p_bad
score_data$ln_odd_goods <- log(score_data$odd_goods)

str(score_data)


#Points to double odds i.e.20
PDO<-20

#Base Score=400 & odds = 10
BaseScore<-400
Odds<-10

#Calaculating Factor & Offset
Factor=PDO/log(2)
Offset=BaseScore-(Factor*log(Odds))
Offset
score_data$score <- Offset+(Factor*score_data$ln_odd_goods)

# Structure of Dataset
str(score_data)
summary(score_data)

# Mean and Standard Deviation of scores
sd(score_data$score)
mean(score_data$score)

# Min and Max Scores
min(score_data$score)
max(score_data$score)

# Creating bins at regular intervals of 20
score_data$scorebins <- cut(score_data$score, breaks = seq(350, 470, 20)) 
View(score_data)
str(score_data)

### Creating a function to generate cutoffs at regular intervals, and calculates the percetage default at each cutoff

## Inputs: Score_Dataset, Cutoff_Start, Cutoff_Stop, double
## Outputs: Generates a "cutoff_dataset" dataframe in global environment with cutoff values and respective percentage defaults

# Before using this function, please remember to change the names of the score dataset in following manner:
# Performance Tag variable to 'PerformanceTag' ; score varaible to 'score' 
# Also, Performance Tag should be numeric variable, where 0 represents non default and 1 represents default.

cutoff_populator <- function(dataset = score_data, cutoff_start, cutoff_stop, doubles = 20)
{
  cutoff_dataset <- data.frame(Cutoffs = as.character(), Percentage_Default = as.numeric(),
                               Credit_risk <- as.numeric(), Potential_Customer_Loss = as.numeric(), stringsAsFactors = FALSE)
  Default_Percentage <- round((sum(dataset$PerformanceTag)/length(dataset$PerformanceTag)) * 100,2)
  default_subset <- subset(dataset, dataset$PerformanceTag == 1)
  Credit_risk <- sum(default_subset$Outstanding.Balance)
  total_non_defaults <- sum(score_data$PerformanceTag == 0)
  non_defaults_ignored <- sum(score_data$PerformanceTag == 0)
  Potential_Customer_Loss <- total_non_defaults - non_defaults_ignored
  cutoff_dataset <- rbind(cutoff_dataset, data.frame('No_Cutoffs', Default_Percentage, Credit_risk, Potential_Customer_Loss, stringsAsFactors = FALSE))
  colnames(cutoff_dataset)[1] <- "Cutoffs"
  
  while(cutoff_start <= cutoff_stop)
  {
    subseted_df <- subset(dataset, dataset$score >= cutoff_start)
    default_subset <- subset(subseted_df, subseted_df$PerformanceTag == 1)
    Default_Percentage <- round((sum(subseted_df$PerformanceTag)/length(subseted_df$PerformanceTag)) * 100,2)
    Credit_risk <- sum(default_subset$Outstanding.Balance)
    non_defaults_ignored <- sum(subseted_df$PerformanceTag == 0)
    Potential_Customer_Loss <- total_non_defaults - non_defaults_ignored
    Cutoffs <- paste(cutoff_start, '+', sep = '')
    cutoff_dataset <- rbind(cutoff_dataset, data.frame(Cutoffs, Default_Percentage, Credit_risk, Potential_Customer_Loss, stringsAsFactors = FALSE))
    cutoff_start <- cutoff_start + doubles
  }
  
  assign("cutoff_dataset", cutoff_dataset, .GlobalEnv)
}

## Generating the cutoff dataset through above function.
## We have scores in the range of 340 - 480. Hence cutoff_start = 340 and cutoff_stop = 480 in this case.
## Also, the organization has demanded the doubling of scores at every 20 points. Assigning the parameters accordingly.


cutoff_populator(dataset = score_data, cutoff_start = 350, cutoff_stop = 470, doubles = 20)
str(cutoff_dataset)

# Application Scorecard
Application_Scorecard <- cutoff_dataset

# Calculating Optimal Cutoff Score Value

bad_opt <- final_optimal_prob
good_opt <- 1 - bad_opt
odds_opt <- good_opt/bad_opt
log_odds_opt <- log(odds_opt)

Offset = BaseScore-(Factor*log(Odds))
Offset

Optimal_Cutoff_Score <- round(Offset + (Factor*log_odds_opt))

# Optimal_Cutoff_Score -- 417
Optimal_Cutoff_Score

# Comparing the Scores of Defaulters with NonDefaulers
subset_defaulters <- subset
mean_score_defaulters <- mean(score_data$score)

score_compare <- data.frame(aggregate(score~as.factor(PerformanceTag), score_data, mean))

mean_score_defaulters <- score_compare$score[2]
mean_score_non_defaulters <- score_compare$score[1]

mean_score_defaulters     # 398.49
mean_score_non_defaulters # 427.37

# As expected, the mean score of non_defaulters is higher than that of defaulters.

###########################################################################################################################

### Analysing the Financial Benefits of using our deployed model -- Weighted Master WOE Model ###

###########################################################################################################################

### Preparing a Scorecard for our applied model
cutoff_populator(cutoff_start = Optimal_Cutoff_Score, cutoff_stop = Optimal_Cutoff_Score)

model_cutoff_card <- cutoff_dataset


### Credit_Amout_Saved
credit_amount_saved <- model_cutoff_card$Credit_risk[1] - model_cutoff_card$Credit_risk[2]
credit_amount_saved  # 964829982

# Thus, using our delpoyed model, we will reduce our credit_amount by 96,48,29,982


### Business Loss in terms of customers
total_num_good_customers <- sum(score_data$PerformanceTag == 0)
num_good_customers_ignored <- model_cutoff_card$Potential_Customer_Loss[2]

customer_percentage_loss <- round(num_good_customers_ignored/total_num_good_customers*100)
customer_percentage_loss

# Though we managed to save a reasonable amount of credit loss, but in return we made a trade off in terms of potential_customers
# We lost around 30% of the potential customers, which surely affects a little bit on over final collected revenue.

### Default_Percentage_reduced
default_percent_reduced <- model_cutoff_card$Default_Percentage[1] - model_cutoff_card$Default_Percentage[2]
default_percent_reduced  # 3.29

# Using our deployed model, we can reduce the default_percentage by 3.29%


### Credit_Risk without model

default_subset_no_model <- subset(score_data, score_data$PerformanceTag == 1)
str(default_subset_no_model)
cred_def_no_model <- sum(default_subset_no_model$Outstanding.Balance)
total_cred_no_model <- sum(as.numeric(score_data$Outstanding.Balance))

cred_def_no_model      # Credit_Defaulted
total_cred_no_model    # Total_Credit

cred_risk_no_model <- cred_def_no_model/total_cred_no_model
cred_risk_no_model_precentage <- round(cred_risk_no_model*100)
cred_risk_no_model_precentage  # 6%

# Hence, without model, there is 6% credit risk

### Credit_Risk with deployed Model

model_subset <- subset(score_data, score_data$score >= Optimal_Cutoff_Score)
default_subset_model <- subset(model_subset, model_subset$PerformanceTag == 1)
str(default_subset_model)
cred_def_model <- sum(default_subset_model$Outstanding.Balance)
total_cred_model  <- sum(as.numeric(model_subset$Outstanding.Balance))

cred_def_model         # Credit_Defaulted
total_cred_model       # Total_Credit

cred_risk_model <- cred_def_model/total_cred_model
Cred_risk_model_percentage <- round(cred_risk_model*100)
Cred_risk_model_percentage   # 3%

# With model, the credit risk is reduced to 3%, i.e. half of that without model

## Credit Loss Saved
credit_loss_saved <- cred_risk_no_model_precentage - Cred_risk_model_percentage
credit_loss_saved # 3%

# Thus, using our deployed model, we brought the credit loss down by 3%

