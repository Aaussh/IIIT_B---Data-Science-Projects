library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)


Loan <- read.csv("loan.csv", stringsAsFactors = FALSE)

options(scipen = 999)### Convert the scientific notation to the regular format


#######################################################################################################################

                                          ### Data Cleaning and Preparation ###

#######################################################################################################################


# Removing all the columns which doesn't have two or more than two distinct values( Na's and blanks are also considered as distinct values)
Loan_w <- Loan[ , FALSE]

for(i in colnames(Loan))
{
  if (n_distinct(Loan[, i]) > 1 ){
    Loan_w[ , i] <- Loan[ , i]
  }
}


# Removing unnecessary columns
Loan_w <- subset(Loan_w , select = -c(2,4,5,10,11,18,19,21,22,25:29,31,35:46,48:51))
Loan_w[Loan_w == "n/a" ] <- NA

# Checking for NA values in loan id's
ifelse(sum(ifelse(is.na(Loan_w$id), 1, 0)) == 0, "No NA Values in Loan ID", "NA values exists in Loan ID")

# Checking for duplicate loan id's
ifelse(length(unique(Loan_w$id)) == nrow(Loan_w), "No duplicate Loan ID's", "Duplicate Loan ID's found")

## convert the interest rate to Numeric . 
Loan_w$int_rate <- as.numeric(str_replace_all(Loan_w$int_rate,"%",""))
Loan_w$revol_util <- as.numeric(str_replace_all(Loan_w$revol_util,"%",""))

## Clean the column emp_length(remove the test 'years' and replace <1 years with 0.5 and >10 years with 11 years for easier calculation . )

#Loan_w$emp_length <- gsub('[years,year]','',Loan_w$emp_length)
Loan_w$emp_length <- str_replace_all(Loan_w$emp_length,'[year,years]','')
Loan_w$emp_length <- str_replace_all(Loan_w$emp_length,'< 1','0.5')
Loan_w$emp_length <- str_replace_all(Loan_w$emp_length,'[10].\\+','11')
Loan_w$emp_length <- as.numeric(Loan_w$emp_length)
str(Loan_w)

## Standardise the date columns
Loan_w$issue_d = as.Date(paste(trimws(Loan_w$issue_d),'-2018',sep=""), format = "%b-%d-%Y")
Loan_w$last_pymnt_d = as.Date(paste(trimws(Loan_w$last_pymnt_d),'-2018',sep=""), format = "%b-%d-%Y")
Loan_w$last_credit_pull_d = as.Date(paste(trimws(Loan_w$last_credit_pull_d),'-2018',sep=""), format = "%b-%d-%Y")
Loan_w$next_pymnt_d = as.Date(paste(trimws(Loan_w$next_pymnt_d),'-2018',sep=""), format = "%b-%d-%Y")

## BucketiNG annual_income
Loan_w$annual_inc_bucket[Loan_w$annual_inc < 25000] <- "< 25000"
Loan_w$annual_inc_bucket[Loan_w$annual_inc >= 25000 & Loan_w$annual_inc <= 75000] <- "25000-75000"
Loan_w$annual_inc_bucket[Loan_w$annual_inc > 75000 & Loan_w$annual_inc <= 150000] <- "75000-150000"
Loan_w$annual_inc_bucket[Loan_w$annual_inc > 150000 & Loan_w$annual_inc <= 300000] <- "150000 - 300000"
Loan_w$annual_inc_bucket[Loan_w$annual_inc > 300000] <- ">300000"

## Bucketing interest_rate
Loan_w$int_rate_bucket[Loan_w$int_rate < 10] <- "< 10%"
Loan_w$int_rate_bucket[Loan_w$int_rate >= 10 & Loan_w$int_rate <= 15] <- "10% - 15%"
Loan_w$int_rate_bucket[Loan_w$int_rate > 15 & Loan_w$int_rate <= 20] <- "15% - 20%"
Loan_w$int_rate_bucket[Loan_w$int_rate > 20 & Loan_w$int_rate <= 25] <- "20% - 25%"
Loan_w$int_rate_bucket[Loan_w$int_rate > 25] <- "> 25%"

## Bucketing Loan_amnt
Loan_w$loan_bucket[Loan_w$loan_amnt < 10000] <- "< 10000"
Loan_w$loan_bucket[Loan_w$loan_amnt >= 10000 & Loan_w$loan_amnt <= 20000] <- "10000 - 20000"
Loan_w$loan_bucket[Loan_w$loan_amnt > 20000 & Loan_w$loan_amnt <= 30000] <- "20000 - 30000"
Loan_w$loan_bucket[Loan_w$loan_amnt > 30000] <- "> 30000"

## Bucketing installment_amnt
Loan_w$installment_bucket[Loan_w$installment < 500] <- "< 500"
Loan_w$installment_bucket[Loan_w$installment >= 500 & Loan_w$installment <= 1000] <- "500 - 1000"
Loan_w$installment_bucket[Loan_w$installment > 1000] <- "> 1000"

## Bucketing dti columns
Loan_w$dti_bucket[Loan_w$dti < 10] <- "< 10"
Loan_w$dti_bucket[Loan_w$dti >= 10 & Loan_w$dti <= 20] <- "10 to 20"
Loan_w$dti_bucket[Loan_w$dti > 20] <- "> 20"

## Bucketing Employment Length
Loan_w$emp_length_bucket[Loan_w$emp_length < 4] <- "< 4"
Loan_w$emp_length_bucket[Loan_w$emp_length >= 4 & Loan_w$emp_length <= 9 ] <- "4 to 9"
Loan_w$emp_length_bucket[Loan_w$emp_length > 9] <- "> 9"

## Bucketing Revolving Balance
Loan_w$revol_bal_bucket[Loan_w$revol_bal < 50000] <- "< 50000"
Loan_w$revol_bal_bucket[Loan_w$revol_bal >= 50000 & Loan_w$revol_bal <= 100000] <- "50000 - 100000"
Loan_w$revol_bal_bucket[Loan_w$revol_bal > 100000] <- "> 100000"

## Bucketing Revolving util
Loan_w$revol_util_bucket[Loan_w$revol_util < 20] <- "< 20"
Loan_w$revol_util_bucket[Loan_w$revol_util >= 20 & Loan_w$revol_util <= 40] <- "20 - 40"
Loan_w$revol_util_bucket[Loan_w$revol_util > 40 & Loan_w$revol_util <= 60] <- "40 - 60"
Loan_w$revol_util_bucket[Loan_w$revol_util > 60 & Loan_w$revol_util <= 80] <- "60 - 80"
Loan_w$revol_util_bucket[Loan_w$revol_util > 80] <- "> 80"

## Bucketing Revolving Balance
Loan_w$revol_bal_bucket[Loan_w$revol_bal < 50000] <- "< 50000"
Loan_w$revol_bal_bucket[Loan_w$revol_bal >= 50000 & Loan_w$revol_bal <= 100000] <- "50000 - 100000"
Loan_w$revol_bal_bucket[Loan_w$revol_bal > 100000] <- "> 100000"

## Bucketing total_acc
Loan_w$total_acc_bucket[Loan_w$total_acc < 25] <- "< 25"
Loan_w$total_acc_bucket[Loan_w$total_acc >= 25 & Loan_w$open_acc <= 50] <- "25 to 50"
Loan_w$total_acc_bucket[Loan_w$total_acc > 50] <- "> 50"

## Bucketing open_acc
Loan_w$open_acc_bucket[Loan_w$open_acc < 12] <- "< 12"
Loan_w$open_acc_bucket[Loan_w$open_acc >= 12 & Loan_w$open_acc <= 24] <- "12 to 24"
Loan_w$open_acc_bucket[Loan_w$open_acc > 24] <- "> 24"


#######################################################################################################################

                                        ### Data Visualization ###

#######################################################################################################################
##number of total request vs verification status
verification_status_plot<-ggplot(Loan_w,aes(x=Loan_w$verification_status))+geom_bar()
#number of total request vs loan status
loan_status_plot<-ggplot(Loan_w,aes(x=Loan_w$verification_status,fill=Loan_w$loan_status))+geom_bar(position = "fill")
loan_status_plot

#verification vs loan status trend
grid.arrange(loan_status_plot,verification_status_plot,ncol=2)

#State having maximum defaulters : NV not NE (because the number of loan request for NE is negligible)
statewise_analysis<-ggplot(Loan_w,aes(x=Loan_w$addr_state,fill=Loan_w$loan_status))+geom_bar(position = "fill")+coord_flip()
statewise_analysis

#employment length having maximum defaulters : "n/a" that is when employment years data is not mentioned
emp_lengthwise_analysis<-ggplot(Loan_w,aes(x=factor(Loan_w$emp_length_bucket),fill=Loan_w$loan_status))+geom_bar(position = "fill")+coord_flip()
emp_lengthwise_analysis

#installment rangewise: the difference is ~3%(low impact)
installment_rangewise<-ggplot(Loan_w,aes(Loan_w$installment_bucket,fill=Loan_w$loan_status))+geom_bar(position = "fill")
installment_rangewise

#interest rate wise we have maximum defaulters with interest rate : 20-25%
int_ratewise<-ggplot(Loan_w,aes(Loan_w$int_rate_bucket,fill=Loan_w$loan_status))+geom_bar(position = "fill")
int_ratewise

#if null and grade= A then pass the request since for lower grades the charged off %age multiplies by 1.5 times
emp.len_vs_grade <-ggplot(Loan_w,aes(factor(Loan_w$grade),fill=Loan_w$loan_status))+geom_bar(position = "fill")+coord_flip()+facet_grid(.~factor(Loan_w$emp_length))
emp.len_vs_grade

#Grades having maximum defaulters : G>F>E
gradewise_analysis<-ggplot(Loan_w,aes(x=Loan_w$grade,fill=Loan_w$loan_status))+geom_bar(position = "fill")+coord_flip()
gradewise_analysis

#maximum defaulters with purpose : small_business
purposewise_analysis<-ggplot(Loan_w,aes(Loan_w$purpose,fill=Loan_w$loan_status))+geom_bar(position = "fill")+coord_flip()
purposewise_analysis

#purpose vs grade
#small business :grade= A then pass the request since for lower grades the charged off %age multiplies by 1.5 times
purpose_category<-filter(Loan_w,Loan_w$purpose=="small_business")
purpose_vs_grade<-ggplot(Loan_w,aes(Loan_w$purpose,fill=Loan_w$loan_status))+geom_bar(position = "fill")+coord_flip()+facet_grid(factor(Loan_w$grade)~.)+theme_bw(base_size = 10)
purpose_vs_grade

#income vs purpose
income_vs_purpose<-ggplot(purpose_category,aes(purpose_category$annual_inc_bucket,fill=purpose_category$loan_status))+geom_bar(position = "fill")+theme_bw()
annual_incomewise_count<-ggplot(Loan_w,aes(Loan_w$annual_inc_bucket,fill=Loan_w$loan_status))+geom_bar()+theme_bw(base_size = 10)
grid.arrange(income_vs_purpose,annual_incomewise_count,ncol=2)

#maximum defaulters with term : 60months
termwise<-ggplot(Loan_w,aes(factor(Loan_w$term),fill=Loan_w$loan_status))+geom_bar(position = "fill")
termwise

#grade vs term: for grades E, F, G we have maximum loan requests with term 60 months as compared to 36 months.
grade_vs_term<-ggplot(Loan_w,aes(Loan_w$grade,fill=Loan_w$loan_status))+geom_bar(position = "fill")+coord_flip()+facet_grid(factor(Loan_w$term)~.)+theme_bw(base_size = 10)
grade_vs_term

#annual income bucket 25k to 75k ,<25k
#installment : 0 to 500, 500 to 1000
#grade : E,F, G
#range of charged off is between 27% to 44%
required_annual_income<-subset(purpose_category,purpose_category$annual_inc_bucket=="25000-75000")
required_grades<-subset(required_annual_income,required_annual_income$grade=="G" |required_annual_income$grade=="F" | required_annual_income$grade=="E")
ggplot(required_grades,aes(required_grades$installment_bucket,fill=required_grades$loan_status))+geom_bar(position = "fill")+facet_grid(required_grades$grade~required_grades$annual_inc_bucket)+coord_flip()

#annual income less than 75k, subsetting as mentioned in below
required_annual_income2<-subset(purpose_category,purpose_category$annual_inc_bucket=="25000-75000"|purpose_category$annual_inc_bucket=="<25000")
#above,interest rate and Loan bucket analysis, higher for 20-25% int rate
ggplot(required_annual_income2,aes(required_annual_income2$int_rate_bucket,fill=required_annual_income2$loan_status))+geom_bar(position = "fill")+facet_grid(.~factor(required_annual_income2$loan_bucket))+theme_bw(base_size = 8)


#revol_util_bucket analysis
#revol_util analysis: charged off in %age
revol_util_analysis<-ggplot(Loan_w,aes(Loan_w$revol_util_bucket,fill=Loan_w$loan_status))+geom_bar(position = "fill")
#count of charged off
revol_util_count<-ggplot(Loan_w,aes(Loan_w$revol_util_bucket,fill=Loan_w$loan_status))+geom_bar()

#Charged off is maximum for revol_util_bucket >80, neglecting NA as the number of loan request is negligible as shown in the below plot
grid.arrange(revol_util_analysis,revol_util_count,ncol=2)

#Trend difference for grade G:
#For revolv_util_bucket, usually the percentage of charged off increases as the bucket size increases, But specifically for Grade G, the percentage of charged off was found to be decreasing w.r.t to increase in bucket size i.e. A reverse trend was observed for Grade G.
revol_util_vs_grade<-ggplot(Loan_w,aes(Loan_w$revol_util,fill=Loan_w$loan_status))+geom_bar(position = "fill")+facet_grid(.~Loan_w$grade)+coord_flip()

