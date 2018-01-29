###############################################################################
# 
#  Deveploped by Alay Shah, Rutuja Mowade, Sabyasachi Tripathy, Rahul Rungta 
# 
##########################################################################################################################
#setwd("C:/Users/sabyasachi.t/Desktop/Learning/Upgrad/Group_Project_1_Investment_Case_Study")
#getwd()

library(tidyr)
library(dplyr)
library(sqldf)
library(gdata)
library(stringr)

options(scipen = 999) ### Convert the sceintific notation to the regular format

### CHECKPOINT 1 : Data Cleaning

# Loading the Companies and rounds data
companies<-read.delim("companies.txt",header = T,sep = "\t")
rounds2<-read.csv("rounds2.csv",stringsAsFactors = FALSE)

companies$permalink<-tolower(companies$permalink)
rounds2$company_permalink<-tolower(rounds2$company_permalink)

#How many unique companies are present in rounds2?
unique_companies_rounds2<-nrow(distinct(rounds2,company_permalink))
cat("No of Unique companies in rounds2 = ",unique_companies_rounds2)

#How many unique companies are present in the companies file?
unique_companies<-nrow(distinct(companies,permalink))
cat("No of Unique companies in companies file = ",unique_companies)

###Are there any companies in the rounds2 file which are not present in companies? 

Comparison <-setdiff(unique_companies_rounds2,unique_companies)
Comparison

cat("There are no companies in the rounds2 file which are not present in companies ")

#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.Name the merged frame master_frame.
#How many observations are present in master_frame ?
#merging the two data frames via common column. Since the column names are different in both df, so we have explicitly mentioned the both column names as below:
master_frame<-merge(companies,rounds2,by.x = "permalink",by.y = "company_permalink")
cat("No of observations in master_frame = ",nrow(master_frame))

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#CHECKPOINT 2:


### filter the desired funding types 
### group them by funding types and compute the avg investment 
### arange them in descending order and fetch the funding type with maximum amount in the provided range 

mf_filtered_cp2 <- filter(master_frame,!is.na(raised_amount_usd),funding_round_type == "venture" | funding_round_type=="seed"|funding_round_type=="angel"|funding_round_type=="private_equity")
mf_grouped <- group_by(mf_filtered_cp2,funding_round_type)
mf_summary<-summarise(mf_grouped,mean(raised_amount_usd))

cat("Average funding amount of venture type = ",as.numeric(mf_summary[which(mf_summary$funding_round_type=="venture"),2]))
cat("Average funding amount of angel type = ",as.numeric(mf_summary[which(mf_summary$funding_round_type=="angel"),2]))
cat("Average funding amount of seed type = ",as.numeric(mf_summary[which(mf_summary$funding_round_type=="seed"),2]))
cat("Average funding amount of private equity type = ",as.numeric(mf_summary[which(mf_summary$funding_round_type=="private_equity"),2]))

mf_summary_criteria <- filter(mf_summary,`mean(raised_amount_usd)`>=5000000 & `mean(raised_amount_usd)`<=15000000)
suitable_funding_type <- as.character( mf_summary_criteria[which.max(mf_summary_criteria$`mean(raised_amount_usd)`),1])

cat("Most suitable investment type for Spark Funds is ",suitable_funding_type )

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#CHECKPOINT 3: COUNTRY ANALYSIS
#English speaking countries :
investment_type_venture<-filter(master_frame,master_frame$funding_round_type==suitable_funding_type) #applying filter for venture investment type

investment_type_venture<-na.omit(investment_type_venture) #removing NAs

total_funding_venture<-aggregate(investment_type_venture$raised_amount_usd,by=list(investment_type_venture$country_code),FUN=sum) #calculate the sum of funds raised per country

top9<-arrange(total_funding_venture,desc(total_funding_venture$x))[1:9,] #top9 countries with the highest funds.

top1_eng_spk_country <- "USA"
top2_eng_spk_country <- "GBR"
top3_eng_spk_country <- "IND"

#------------------------------------------------------------------------------------------------------------------------------------------------------------------                                                              
#CHECKPOINT 4: SECTOR ANALYSIS :


### create a data frame out of the mapping.csv and clean the data.
mapping <- read.csv("mapping.csv",header = T)
#Data cleaning in mapping df
mapping <- filter(mapping,category_list!="")
mapping$category_list <- str_replace_all(tolower(mapping$category_list),"0","na")
mapping$category_list <- str_replace_all(tolower(mapping$category_list),"\\.na","\\.0")

### convert the wide data in mapping.csv to long data 

mapping_intermediate <- gather(mapping,sector,sector_val,Automotive...Sports : Social..Finance..Analytics..Advertising)
mapping_final <- mapping_intermediate[!(mapping_intermediate$sector_val == 0),-3]

####merge mapping_final with the master_frame to extract the sectors 
master_frame_cp4<- master_frame
master_frame_cp4$category_list <- tolower(master_frame_cp4$category_list) 
master_frame_cp4_splitted <- separate(master_frame_cp4,category_list,into=c("category_list"),extra = "warn",sep = '\\|')
master_frame_withsector <- merge(x=master_frame_cp4_splitted,y=mapping_final,by="category_list",all.x =  T)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Checkpoint 5
###Creating the three data frames for the top three countries identified in CP # 3 as per the requirement given:

D1 <- filter(master_frame_withsector,country_code==top1_eng_spk_country,funding_round_type==suitable_funding_type,raised_amount_usd>=5000000 & raised_amount_usd <=15000000)
D2 <- filter(master_frame_withsector,country_code==top2_eng_spk_country,funding_round_type==suitable_funding_type,raised_amount_usd>=5000000 & raised_amount_usd <=15000000)
D3 <- filter(master_frame_withsector,country_code==top3_eng_spk_country,funding_round_type==suitable_funding_type,raised_amount_usd>=5000000 & raised_amount_usd <=15000000)

# 1) Total Number of Investments
# Country 1
# Ans - 12150
cat("Total number of Investments (count) for Country1:",as.character(count(D1)))


# Country 2
# Ans - 628
cat("Total number of Investments (count) for Country2:",as.character(count(D2)))

# Country 3
# Ans - 330
cat("Total number of Investments (count) for Country3:",as.character(count(D3)))

# 2) Total Amount of Investments

# Country 1
# Ans - 108531347515
cat("Total amount of Investments (sum) for Country1:",as.character(sum(D1$raised_amount_usd)))
    
# Country 2
# Ans - 5436843539
cat("Total amount of Investments (sum) for Country2:",as.character(sum(D2$raised_amount_usd)))
        
# Country 3
# Ans - 2976543602
cat("Total amount of Investments (sum) for Country3:",as.character(sum(D3$raised_amount_usd)))
            
####
#calculating Top 3 sectors for country 1,2,3 based on number of investment-wise in each sector
            
D1_count_invest <- aggregate(raised_amount_usd~sector,D1,FUN = length)
D2_count_invest <- aggregate(raised_amount_usd~sector,D2,FUN = length)
D3_count_invest <- aggregate(raised_amount_usd~sector,D3,FUN = length)
            
#Arranging the sectors in descending order for getting top 3 sectors for each country
#Country 1
Top_sector_D1 <- arrange(D1_count_invest,desc(D1_count_invest$raised_amount_usd))[1,1]
Sec_sector_D1 <- arrange(D1_count_invest,desc(D1_count_invest$raised_amount_usd))[2,1]
Third_sector_D1 <- arrange(D1_count_invest,desc(D1_count_invest$raised_amount_usd))[3,1]
            
#Country 2            
Top_sector_D2 <- arrange(D2_count_invest,desc(D2_count_invest$raised_amount_usd))[1,1]
Sec_sector_D2 <- arrange(D2_count_invest,desc(D2_count_invest$raised_amount_usd))[2,1]
Third_sector_D2 <- arrange(D2_count_invest,desc(D2_count_invest$raised_amount_usd))[3,1]
            
#Country 3            
Top_sector_D3 <- arrange(D3_count_invest,desc(D3_count_invest$raised_amount_usd))[1,1]
Sec_sector_D3 <- arrange(D3_count_invest,desc(D3_count_invest$raised_amount_usd))[2,1]
Third_sector_D3 <- arrange(D3_count_invest,desc(D3_count_invest$raised_amount_usd))[3,1]
            
#Displaying top 3 sectors for country 1,2,3
# 3) TOP SECTOR (based on count of investments)
# Country 1
# Ans - Others
cat("Top Sector name (no. of investment-wise) for D1 is :",as.character(Top_sector_D1))
            
# Country 2
# Ans - Others
cat("Top Sector name (no. of investment-wise) for D2 is :",as.character(Top_sector_D2))
            
# Country 3
# Ans - Others
cat("Top Sector name (no. of investment-wise) for D3 is :",as.character(Top_sector_D3))
            
# 4) SECOND BEST SECTOR (based on count of investments)
            
# Country 1
# Ans - Social..Finance..Analytics..Advertising
cat("Second Sector name (no. of investment-wise) for D1 is :",as.character(Sec_sector_D1))
            
# Country 2
# Ans - Social..Finance..Analytics..Advertising
cat("Second Sector name (no. of investment-wise) for D2 is :",as.character(Sec_sector_D2))
            
# Country 3
# Ans - Social..Finance..Analytics..Advertising
cat("Second Sector name (no. of investment-wise) for D2 is  :",as.character(Sec_sector_D3))
            
            
# 5) Third-best sector (based on count of investments)
            
# Country 1
# Ans - Cleantech...Semiconductors
cat("Third Sector name (no. of investment-wise) for D1 is : ",as.character(Third_sector_D1))
            
# Country 2
# Ans - Cleantech...Semiconductors
cat("Third Sector name (no. of investment-wise) for D2 is : ",as.character(Third_sector_D2))
            
# Country 3
# Ans - News..Search.and.Messaging
cat("Third Sector name (no. of investment-wise) for D3 is : ",as.character(Third_sector_D3))
            
# 6) Number of investments in the top sector (refer to point 3)
            
#Arranging number of investments of sectors for all three countries in descending order:
            
D1_arrange_sector_name<-arrange(D1_count_invest,desc(D1_count_invest$raised_amount_usd))
D2_arrange_sector_name<-arrange(D2_count_invest,desc(D2_count_invest$raised_amount_usd))
D3_arrange_sector_name<-arrange(D3_count_invest,desc(D3_count_invest$raised_amount_usd))
            
# Country 1
# Ans - Others - 2950
cat("Number of investments in top sector for Country 1:",as.character(D1_arrange_sector_name[1,2]))
# Country 2
# Ans - Others - 147
cat("Number of investments in top sector for Country 2:",as.character(D2_arrange_sector_name[1,2]))
            
# Country 3
# Ans - Others - 110
cat("Number of investments in top sector for Country 3:",as.character(D3_arrange_sector_name[1,2]))
                
# 7) Number of investments in the second-best sector (refer to point 4)
                
# Country 1
# Ans - Social..Finance..Analytics..Advertising - 2714
cat("Number of investments in second sector for Country 1:",as.character(D1_arrange_sector_name[2,2]))
                
# Country 2
# Ans - Social..Finance..Analytics..Advertising - 133
cat("Number of investments in second sector for Country 2:",as.character(D2_arrange_sector_name[2,2]))
                
# Country 3
# Ans - Social..Finance..Analytics..Advertising - 60
cat("Number of investments in second sector for country 3:",as.character(D3_arrange_sector_name[2,2]))
                
# 8) Number of investments in the third-best sector (refer to point 5)
                
# Country 1
# Ans - Cleantech...Semiconductors - 2350
cat("Number of investments in third sector for country 1:",as.character(D1_arrange_sector_name[3,2]))
                
# Country 2
# Ans - Cleantech...Semiconductors - 130
cat("Number of investments in third sector for country 2:",as.character(D2_arrange_sector_name[3,2]))
                
# Country 3
# Ans - News..Search.and.Messaging - 52
cat("Number of investments in third sector for country 3:",as.character(D3_arrange_sector_name[3,2]))
                
                
# 9) For the top sector count-wise (point 3), which company received the highest investment?
# Country 1
# Ans - Virtustream 
#Creating the subset with required fields(top sector) for each country,
TopCompany_sector_D1 <- subset(D1,D1$sector==Top_sector_D1,select = c(name,permalink,raised_amount_usd))
#calculating total number of funds received by each company as there are multiple rounds of investments.
TopCompany_sector_D1_grpd <- aggregate(TopCompany_sector_D1$raised_amount_usd,by=list(TopCompany_sector_D1$name),FUN = sum)
#displays the top country name with highest amount of funds received.
cat("company with top sector count-wise who received the highest investment for D1 is ", as.character(arrange(TopCompany_sector_D1_grpd,desc (x))[1,1]))
 
#Repeating the same 3 steps mentioned above for df D2 and D3 for getting the top company name.               
# Country 2
# Ans - Electric Cloud 
TopCompany_sector_D2 <- subset(D2,D2$sector==Top_sector_D2,select = c(name,permalink,raised_amount_usd))
TopCompany_sector_D2_grpd <- aggregate(TopCompany_sector_D2$raised_amount_usd,by=list(TopCompany_sector_D2$name),FUN = sum)
cat("company with top sector count-wise who received the highest investment for D2 is",as.character(arrange(TopCompany_sector_D2_grpd,desc (x))[1,1]))

# Country 3
# Ans - FirstCry.com 
TopCompany_sector_D3 <- subset(D3,D3$sector==Top_sector_D3,select = c(name,permalink,raised_amount_usd))
TopCompany_sector_D3_grpd <- aggregate(TopCompany_sector_D3$raised_amount_usd,by=list(TopCompany_sector_D3$name),FUN = sum)
cat("company with top sector count-wise who received the highest investment for D3 is",as.character(arrange(TopCompany_sector_D3_grpd,desc (x))[1,1]))

# 10) For the second-best sector count-wise (point 4), which company received the highest investment?
                
 # Country 1
 # Ans - SST Inc. (Formerly ShotSpotter)
Top2Company_sector_D1 <- subset(D1,D1$sector==Sec_sector_D1,select = c(name,permalink,raised_amount_usd))
Top2Company_sector_D1_grpd <- aggregate(Top2Company_sector_D1$raised_amount_usd,by=list(Top2Company_sector_D1$name),FUN = sum)
cat("company with 2nd best sector count-wise who received the highest investment for D1 is",as.character(arrange(Top2Company_sector_D1_grpd,desc (x))[1,1]))

                
# Country 2
# Ans - Celltick Technologies
Top2Company_sector_D2 <- subset(D2,D2$sector==Sec_sector_D2,select = c(name,permalink,raised_amount_usd))
Top2Company_sector_D2_grpd <- aggregate(Top2Company_sector_D2$raised_amount_usd,by=list(Top2Company_sector_D2$name),FUN = sum)
cat("company with 2nd best sector count-wise who received the highest investment for D2 is",as.character(arrange(Top2Company_sector_D2_grpd,desc (x))[1,1]))

                
# Country 3
# Ans - Manthan Systems
Top2Company_sector_D3 <- subset(D3,D3$sector==Sec_sector_D3,select = c(name,permalink,raised_amount_usd))
Top2Company_sector_D3_grpd <- aggregate(Top2Company_sector_D3$raised_amount_usd,by=list(Top2Company_sector_D3$name),FUN = sum)
cat("company with 2nd best sector count-wise who received the highest investment for D3 is",as.character(arrange(Top2Company_sector_D3_grpd,desc (x))[1,1]))
                