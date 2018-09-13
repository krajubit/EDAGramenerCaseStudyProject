#--------------------------------Gramener Case Study---------------------------------#

#--------------------------------Install  required libraries---------------------------------#
# Install the libraries if not available
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("scales")
# install.packages("gridExtra")
# install.packages("corrplot")
# install.packages("GGally")
# install.packages("moments")
#-------------------------------- Load the required libraries---------------------------------#
# Load the libraries 
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(scales)
library(gridExtra)
library(corrplot)
library(moments)
library(GGally)
#--------------------------------Data Loading, Cleaning, Preperation ---------------------------------#
#Set working directory where data file is available for reading
#setwd("C:/Training/DataScience-Upgrad/Course2-Stastics and Exploratory Data Analytics/GroupStudy-Gramener Case Study/loan")

# Read the data set to be analyzed
loanDf <- read.csv("loan.csv",stringsAsFactors = FALSE,na.strings = "NA")

#########Data Understanding and Praparation#################################
#Verify structure of loan data frame
str(loanDf)

#View data
#View(loanDf)
#--------------------------------Data Quality Check  ---------------------------------#

#since the company has decided to work only on driver variable we will 
#subset the loan file with driver variable
# RK1APR - Added fields inq_last_6mths,delinq_2yrs,addr_state,pub_rec,revol_util,total_pymnt,total_pymnt_inv,total_rec_prncp,total_rec_late_fee,recoveries,collection_recovery_fee

loan_Driver <- subset(loanDf,select = c(id,annual_inc,dti,emp_length,funded_amnt,grade,home_ownership,installment,int_rate,issue_d,last_credit_pull_d,last_pymnt_amnt,last_pymnt_d,loan_amnt,loan_status,member_id,next_pymnt_d,purpose,sub_grade,term,title,addr_state,zip_code,inq_last_6mths,delinq_2yrs,total_pymnt,total_pymnt_inv,total_rec_prncp,total_rec_int))

loan_Driver$id <- trimws(tolower(loan_Driver$id))
loan_Driver$emp_length <- trimws(tolower(loan_Driver$emp_length))

#Analyse Id columns first

#Check for duplicate values for id variable
sum(duplicated(loan_Driver$id))
#Result is 0 which means that id is a unique key and can be traeted as primey key for each loan record

#Check for duplicate values for member_id variable
sum(duplicated(loan_Driver$member_id))
#Result is 0 which means that member_id is a unique key and each loan is associated with a unique member.
#It means that the same member is not availaing the loan more than once

#Check for NA values
sum(is.na(loan_Driver$id)) # sum 0
sum(is.na(loan_Driver$member_id)) # sum 0
sum(is.na(loan_Driver$loan_amnt)) # sum 0
sum(is.na(loan_Driver$annual_inc))# sum 0


#we also notice that there are NA values precent in emp_lenght with a value of n/a which we cannot get by is.na function
#so we will search by n/a in loan df

loan_Driver$emp_length <- trimws(tolower(loan_Driver$emp_length))

x <- which(loan_Driver$emp_length == "n/a")

str(x)

y <- loan_Driver[x,]


#now we will impute the value of mode of emp_lenght in n/a values

table(loan_Driver$emp_length)

#< 1 year    1 year 10+ years   2 years   3 years   4 years   5 years   6 years   7 years   8 years   9 years     n/a 
#   4583      3240      8879      4388      4095      3436      3282      2229      1773      1479      1258      1075 




#so we have 10+ years as the highest number of observation so we will replace n/a with 10+ years

loan_Driver[x,4] <- "10+ years"

# Data alalysis after replacing n/a with high possible value 
table(loan_Driver$emp_length)

#< 1 year    1 year 10+ years   2 years   3 years   4 years   5 years   6 years   7 years   8 years   9 years 
# 4583      3240      9954      4388      4095      3436      3282      2229      1773      1479      1258 
 

#Instead of checking NA for each column one by one, we can use following by checking colSums for NA columns
na_count_per_column_loandf <- as.data.frame(colSums(is.na(loan_Driver)))

na_count_per_column_loandf 

#Summarize loan Data 
#loanDf :	39717 obs. of  22 Driving variables:
#Primary key: id (It can be used to identify each loan record in data set )
#Foreign Key: member_id (It can be used to identify each borrower member associated to loan record in data set )

#na_count
#id                        0
#annual_inc                0
#dti                       0
#emp_length                0
#funded_amnt               0
#grade                     0
#home_ownership            0
#installment               0
#int_rate                  0
#issue_d                   0
#last_credit_pull_d        0
#last_pymnt_amnt           0
#last_pymnt_d              0
#loan_amnt                 0
#loan_status               0
#member_id                 0
#next_pymnt_d              0
#purpose                   0
#sub_grade                 0
#term                      0
#title                     1
#zip_code                  0
#inq_last_6mths            0
#delinq_2yrs               0
#addr_state                0
#pub_rec                   0
#revol_util                0
#total_pymnt               0
#total_pymnt_inv           0
#total_rec_prncp           0
#total_rec_late_fee        0
#recoveries                0
#collection_recovery_fee   0

loan_Driver$title <- trimws(tolower(loan_Driver$title))


##########################Data Cleaning#################


#since the int_rate is a character variable we will first convert it to numberic variable and remove the % sign from the data 

loan_Driver$int_rate <-  as.double(sub("%","",loan_Driver$int_rate))

#Check if their is any numeric column having sum of column values as 0. If yes then we can exclude those from aalysis
loandfNumeric <- loan_Driver[,which(sapply(loan_Driver,is.numeric))]
colSums(loandfNumeric,na.rm = TRUE)
#Here all numeric columns avaible in data frame loan_Driver is atleast having some value other than NA and 0


#Round all numeric columns to 2 digits
loan_Driver[,which(sapply(loan_Driver,is.numeric))] <- round(loandfNumeric,digits = 2)

#Set blank values in title column as character value "BLANK" to do categorical analysis on missing values
loan_Driver$title <- as.character(loan_Driver$title)
loan_Driver[which(loan_Driver$title == ''),"title"] <- "BLANK"

################################        Univariate Analysis        ###############################################
#--------------------------------Final Derived Variables for further Analysis  ---------------------------------#
summary(loan_Driver)

#id              annual_inc     
#Length:39717       Min.   :   4000  
#Class :character   1st Qu.:  40404  
#Mode  :character   Median :  59000  
#Mean   :  68969  
#3rd Qu.:  82300  
#Max.   :6000000  
#dti         emp_length       
#Min.   : 0.00   Length:39717      
#1st Qu.: 8.17   Class :character  
#Median :13.40   Mode  :character  
#Mean   :13.32                     
#3rd Qu.:18.60                     
#Max.   :29.99                     
#funded_amnt       grade          
#Min.   :  500   Length:39717      
#1st Qu.: 5400   Class :character  
#Median : 9600   Mode  :character  
#Mean   :10948                     
#3rd Qu.:15000                     
#Max.   :35000                     
#home_ownership      installment     
#Length:39717       Min.   :  15.69  
#Class :character   1st Qu.: 167.02  
#Mode  :character   Median : 280.22  
#Mean   : 324.56  
#3rd Qu.: 430.78  
#Max.   :1305.19  
#int_rate           issue_d         
#Length:39717       Length:39717      
#Class :character   Class :character  
#Mode  :character   Mode  :character  

#last_credit_pull_d last_pymnt_amnt  
#Length:39717       Min.   :    0.0  
# Class :character   1st Qu.:  218.7  
# Mode  :character   Median :  546.1  
# Mean   : 2678.8  
# 3rd Qu.: 3293.2  
# Max.   :36115.2  
# last_pymnt_d         loan_amnt    
# Length:39717       Min.   :  500  
# Class :character   1st Qu.: 5500  
# Mode  :character   Median :10000  
# Mean   :11219  
# 3rd Qu.:15000  
# Max.   :35000  
# loan_status          member_id      
# Length:39717       Min.   :  70699  
# Class :character   1st Qu.: 666780  
# Mode  :character   Median : 850812  
# Mean   : 850464  
# 3rd Qu.:1047339  
# Max.   :1314167  
# next_pymnt_d         purpose         
# Length:39717       Length:39717      
# Class :character   Class :character  
# Mode  :character   Mode  :character  
# 
# sub_grade             term          
# Length:39717       Length:39717      
# Class :character   Class :character  
# Mode  :character   Mode  :character  
# 
# title             zip_code        
# Length:39717       Length:39717      
# Class :character   Class :character  
# Mode  :character   Mode  :character 

################*********************  Check Loan Status Structure ***************************************#
table(loanDf$loan_status)

#Charged Off     Current  Fully Paid 
#5627        1140       32950 
 

#now we will crate two bin variable for int_rate and emp_lenght
#we will create a new bin int rate grp

str(loan_Driver$int_rate)

#now we will group int_rate variable as below:

loan_Driver$int_rate_grp <- 
  ifelse(
    loan_Driver$int_rate < 10,"Low",
    ifelse(
      loan_Driver$int_rate >= 10 &
        loan_Driver$int_rate <= 18,"Medium",
      ifelse(
        loan_Driver$int_rate > 18,"High",
        "NA"
      )
    )
  )

#grouping of emp_length varaible as below

str(loan_Driver$emp_length)
#chr [1:39717] "10+ years" "< 1 year" "10+ years" ...

loan_Driver$emp_len_grp <-
  ifelse(
    loan_Driver$emp_length %in% c("< 1 year", "1 year", "2 years", "3 years", "4 years"),
    "Junior",
    ifelse(
      loan_Driver$emp_length %in% c("5 years", "6 years", "7 years", "8 years") ,
      "Mid-Level",
      ifelse(
        loan_Driver$emp_length %in% c("9 years", "10+ years"),
        "Senior",
        "NA"
      )
    )
  )


table(loan_Driver$emp_len_grp)
#Junior Mid-Level    Senior 
#19742      8763     11212 



#--------------------------------------Data Check and Cleaning completed --------------------------------#
#--------------------------------------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------------------------------------#
      #------------------------------------------Univariate Analysis------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------#

summary(loan_Driver)

#since we have character varaible we will  convert them to factor variable for better analysis

chr_to_factor <-
  function(x) {
    for (i in 1:ncol(x)) {
      if (typeof(x[, i]) == "character") {
        x[, i] <- as.factor(x[, i])
      }
    } 
    return(x)
  }

#now we will use the function chr_to_factor to convert character variable to factor variable

loan_Driver <- chr_to_factor(loan_Driver)


str(loan_Driver)

summary(loan_Driver)

# id          annual_inc           dti       
# 1000007:    1   Min.   :   4000   Min.   : 0.00  
# 1000030:    1   1st Qu.:  40404   1st Qu.: 8.17  
# 1000033:    1   Median :  59000   Median :13.40  
# 1000045:    1   Mean   :  68969   Mean   :13.32  
# 1000067:    1   3rd Qu.:  82300   3rd Qu.:18.60  
# 1000095:    1   Max.   :6000000   Max.   :29.99 

# Based on the summay data we can see the variable annual_inc have outlies and we need to work on this to get a better analysis 

quantile(loan_Driver$annual_inc)
#0%     25%     50%     75%    100% 
#4000   40404   59000   82300 6000000 
boxplot(loan_Driver$annual_inc, col="red", border="black") #outlier present

quantile(loan_Driver$loan_amnt)
#0%   25%   50%   75%  100% 
#500  5500 10000 15000 35000
boxplot(loan_Driver$loan_amnt, col="red", border="black") #outlier Present

quantile(loan_Driver$funded_amnt)
#0%   25%   50%   75%  100% 
#500  5400  9600 15000 35000 
boxplot(loan_Driver$funded_amnt,col="red", border="black") #outlier Present

quantile(loan_Driver$dti)
#0%   25%   50%   75%  100% 
#0.00  8.17 13.40 18.60 29.99
boxplot(loan_Driver$dti) #No Outlier

#we see that there exist an outlier in annual_inc , loan_amt and funded_amnt

boxplot.stats(loan_Driver$annual_inc)
#use the outlier value of annual_inc to get the index of rows for imputation with median 
outlier_values <- boxplot.stats(loan_Driver$annual_inc)$out
x <- which(loan_Driver$annual_inc %in% outlier_values)

loan_Driver[x,2] <- median(loan_Driver$annual_inc)
#Re box plot the variable to check if the outlier reduced or moved out 
boxplot(loan_Driver$annual_inc,col="green", border="black") # Outlier removed 



#use the outlier value of loan_amt to get the index of rows for imputation with median 
outlier_values1 <- boxplot.stats(loan_Driver$loan_amnt)$out
x1 <- which(loan_Driver$loan_amnt %in% outlier_values1)

loan_Driver[x1,"loan_amnt"] <- median(loan_Driver$loan_amnt)
#Re box plot the variable to check if the outlier reduced or moved out 
boxplot(loan_Driver$loan_amnt,col="green", border="black")


#use the outlier value of loan_amt to get the index of rows for imputation with median 
outlier_value2  <- boxplot.stats(loan_Driver$funded_amnt)$out
x2 <- which(loan_Driver$funded_amnt %in% outlier_value2)

loan_Driver[x2,"funded_amnt"] <- median(loan_Driver$funded_amnt)
#Re box plot the variable to check if the outlier reduced or moved out 
boxplot(loan_Driver$funded_amnt,col="green", border="black")

#we have treated all the outlier values of continuous variable by using median
#check for the distribution of the outlier variable

 

#for Annual_inc

x <- loan_Driver$annual_inc

y <- dnorm(x,mean = mean(x),sd= sd(x))

plot(x,y,main = "Distribution of Annual Income",xlab ="Annual Income",ylab ="Frequency" ) 

#for loan_amnt

x <- loan_Driver$loan_amnt

y <- dnorm(x,mean = mean(x),sd= sd(x))

plot(x,y,main = "Distribution of Loan Amount",xlab ="Loan Amount",ylab ="Frequency" )


#for funded_amnt

x <- loan_Driver$funded_amnt

y <- dnorm(x,mean = mean(x),sd= sd(x))

plot(x,y,main = "Distribution of Funded Amount",xlab ="Funded Amount",ylab ="Frequency" )
#-----------------------------------------------------------------------------------------------#
#           Plot histograms to visually verify the results of distribution                      #
#-----------------------------------------------------------------------------------------------#

#Distribution of Annual Income
Plot1 <- ggplot(loan_Driver,aes(x=loan_Driver$annual_inc)) + geom_histogram(col="red",fill="blue",alpha=0.4,binwidth = 1000)

Plot1 + labs(title = "Distribution of Annual Income", x = "Annual Income", y = "Frequency")

#Distribution of  Loan Amount
Plot2 <- ggplot(loan_Driver,aes(x=loan_Driver$loan_amnt)) + geom_histogram(col="red",fill="blue",alpha=0.4,binwidth = 1000)

Plot2 + labs(title = "Distribution of  Loan Amount", x = "Loan Amount", y = "Frequency")

#Distribution of  Funded Amount
Plot3 <- ggplot(loan_Driver,aes(x=loan_Driver$funded_amnt)) + geom_histogram(col="red",fill="blue",alpha=0.4,binwidth = 1000)

Plot3 + labs(title = "Distribution of  Funded Amount", x = "Funded Amount", y = "Frequency")

#Distribution of  DTI
Plot4 <- ggplot(loan_Driver,aes(x=loan_Driver$dti)) + geom_histogram(col="red",fill="blue",alpha=0.4,binwidth = 1)

Plot4 + labs(title = "Distribution of  DTI", x = "DTI", y = "Frequency")

#Interest Rate distribution
ggplot(loan_Driver, aes(x = loan_Driver$int_rate))+ geom_histogram(stat = "count", fill="tomato2",binwidth = 0.01) +  labs(x = "Interest Rate", y = "Number of Loans") 



#-----------------------------------------------------------------------------------------------#
#--------------           Plot Other Categorical Variable Analysis  ----------------            #
#-----------------------------------------------------------------------------------------------#

#Plot for loan Status
p1 <- ggplot(loan_Driver, aes(x = loan_Driver$loan_status)) + geom_bar(fill="tomato3") + labs(x = "Loan Status", y="No. of Requests")

#Plot Term vs Number of Loan requests  as percentage and count
#We can clearly see the 36 months terms terms is always much higher for paid status but not much difference in numbers for charged of loans
p2 <- ggplot(loan_Driver, aes(x=loan_Driver$term, y = (..count..)/sum(..count..)*100)) + geom_bar(fill="tomato2") + labs(x = "Term", y = "Number of Requests")

#Plot between purpose vs Number of Loans
#We can see that Debt consolidation purpose has highest number of counts
p3 <- ggplot(loan_Driver, aes(x =loan_Driver$purpose)) + geom_bar(fill="tomato2") +  labs(x = "Purpose", y = "Number of Loans")

grid.arrange(p1,p2,p3,nrow=3)

#Plot between Grade vs Number of Loans
#We can see that grade B has highest number of loans then grade A and then grade C and so on
p5 <- ggplot(loan_Driver, aes(x =loan_Driver$grade)) + geom_bar(fill="tomato2") +  labs(x = "Grade", y = "Number of Loans")

#Plot between State vs Number of Loans
#We can see that states CA and then NY has highest number of loans then other states
p6 <- ggplot(loan_Driver, aes(x =loan_Driver$addr_state)) + geom_bar(fill="tomato2") +  labs(x = "Address Sate", y = "Number of Loans")

grid.arrange(p5,p6,nrow=2)





#-------------------------------------------------------------------------------------------------------------------#
#------------------------------------------Univariate Analysis  Completed ------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------#



#-------------------------------------------------------------------------------------------------------------------#
#------------------------------------------Bivariate and Multivariate Analysis------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------#

#find out if there is any corelation exist between any of the continuous variables
#Create a data set with numaric variables which are critical to drive 
con_loan  <- subset(loan_Driver,select = c(annual_inc,loan_amnt,funded_amnt,dti))

ggpairs(con_loan,ggplot2::aes(colour= "green"))

#Another way of plotting the cor between the variables 
loancor <- cor(con_loan)

corrplot(as.matrix(loancor), method = "circle",is.corr = FALSE)

#using Correlation Matrix cor()

cor(con_loan)

#             annual_inc loan_amnt funded_amnt         dti
#annual_inc   1.00000000 0.34316230  0.34309313 -0.07115484
#loan_amnt    0.34316230 1.00000000  0.97004145  0.07374973
#funded_amnt  0.34309313 0.97004145  1.00000000  0.07248517
#dti         -0.07115484 0.07374973  0.07248517  1.00000000

# Based on the above correlation analysis between the key variables we can see below output 
# Funded_Amount and Loan_amount, and the correlation value is 0.97004145 which is equal to 1 positive corelated
# This  means if the loan amount increses funded amount also increases which is what we expect


#Co-relation matrix plot for all numeric values
#Here we can see that loan amount is highly co-related with funded amount and funded amount investment. 
#Loan amount is slightly corelated with outstanding principal, outstanding principal investment , total payment and total payment investment
#Loan amount is inversely corelated with delinq_2yrs,inq_last_6mths
loancor <- cor(loandfNumeric)
#View(loancor)
ggcorr(loancor)

#Above Correlation analysis gives a high level dependencies between variables and this will use for the further analysis 

#we can also see this with the help of a scatter plot

plot5 <- ggplot(loan_Driver,aes(loan_amnt,funded_amnt)) + geom_point(col = "red",alpha = 0.4)

plot5 + labs(title = "Relation between Funded Amount and Loan Amount",x = "Loan Amount",y = "Funded Amount")

#now we will Analysing Categorical variables using summary function
cat_loan <- subset(subset(loan_Driver,select = -c(id,annual_inc,loan_amnt,funded_amnt,dti)))

summary(cat_loan)

# emp_length   grade      home_ownership   installment        
# 10+ years:9954   A:10085   MORTGAGE:17659   Min.   :  15.69  
# < 1 year :4583   B:12020   NONE    :    3   1st Qu.: 167.02  
# 2 years  :4388   C: 8098   OTHER   :   98   Median : 280.22  
# 3 years  :4095   D: 5307   OWN     : 3058   Mean   : 324.56  
# 4 years  :3436   E: 2842   RENT    :18899   3rd Qu.: 430.78  
# 5 years  :3282   F: 1049                    Max.   :1305.19  
# (Other)  :9979   G:  316                                     
# int_rate        issue_d      last_credit_pull_d
# Min.   : 5.42   Dec-11 : 2260   May-16 :10308     
# 1st Qu.: 9.25   Nov-11 : 2223   Apr-16 : 2547     
# Median :11.86   Oct-11 : 2114   Mar-16 : 1123     
# Mean   :12.02   Sep-11 : 2063   Feb-13 :  843     
# 3rd Qu.:14.59   Aug-11 : 1928   Feb-16 :  736     
# Max.   :24.59   Jul-11 : 1870   Jan-16 :  657     
# (Other):27259   (Other):23503     
# last_pymnt_amnt    last_pymnt_d        loan_status   
# Min.   :    0.0   May-16 : 1256   Charged Off: 5627  
# 1st Qu.:  218.7   Mar-13 : 1026   Current    : 1140  
# Median :  546.1   Dec-14 :  945   Fully Paid :32950  
# Mean   : 2678.8   May-13 :  907                      
# 3rd Qu.: 3293.2   Feb-13 :  869                      
# Max.   :36115.2   Apr-13 :  851                      
# (Other):33863                      
# member_id       next_pymnt_d                 purpose     
# Min.   :  70699         :38577   debt_consolidation:18641  
# 1st Qu.: 666780   Jul-16:   15   credit_card       : 5130  
# Median : 850812   Jun-16: 1125   other             : 3993  
# Mean   : 850464                  home_improvement  : 2976  
# 3rd Qu.:1047339                  major_purchase    : 2187  
# Max.   :1314167                  small_business    : 1828  
# (Other)           : 4962  
# sub_grade             term                           title      
# B3     : 2917    36 months:29096   debt consolidation     : 3190  
# A4     : 2886    60 months:10621   debt consolidation loan: 1915  
# A5     : 2742                      personal loan          : 1012  
# B5     : 2704                      consolidation          :  762  
# B4     : 2512                      personal               :  567  
# C1     : 2136                      (Other)                :32270  
# (Other):23820                      NA's                   :    1  
# zip_code     int_rate_grp      emp_len_grp   
# 100xx  :  597   High  : 2512   Junior   :19742  
# 945xx  :  545   Low   :12142   Mid-Level: 8763  
# 112xx  :  516   Medium:25063   Senior   :11212  
# 606xx  :  503                                   
# 070xx  :  473                                   
# 900xx  :  453                                   
# (Other):36630              

#from the summary of categorical variable we see that most of the people were taking loan
#for debt_consolidation,junior employee are the one who are taking more loan,and there are 
#high number of default in loan repayment,where as most of the interest rate are at medium

#-------------------------------------------------------------------------------------------------------------------
#check for the emp_len_grp who tends to default more

loan_Driver1 <- loan_Driver[-c(which(loan_Driver$loan_status == "Fully Paid")),]
emp_plot <- ggplot(loan_Driver1,aes(x=loan_Driver1$emp_len_grp,fill = loan_Driver1$emp_len_grp))+ geom_bar()+facet_wrap(~loan_status)

emp_plot + labs(title="No of loan taken by each employment group and their Status",x = "",y="No of Loan Availed",fill = "Employment Group")

# based on above analysis from the data who are not paied we can see Junior Employment Group 1 to 4 years tend to default more than rest
#-------------------------------------------------------------------------------------------------------------------

#check for purpose which are in high default

purpose_plot <- ggplot(loan_Driver1,aes(x=purpose,fill = purpose))+ geom_bar()+facet_wrap(~loan_status)

purpose_plot + labs(title="Purpose of loan taken and their Status",x = "",y="No of Loan Availed",fill = "Purpose")

# based on above analysis from the data who are not paid we see that high no of default are for debt_consolidation

#-------------------------------------------------------------------------------------------------------------------

#check for home ownership

home_plot <- ggplot(loan_Driver1,aes(x=home_ownership,fill = home_ownership))+ geom_bar()+facet_wrap(~loan_status)

home_plot + labs(title="home_ownership of the applicant and their Status",x = "",y="No of Loan Availed",fill = "home_ownership")

#based on above analysis from the data who are not paied we see that applicant with home_ownership of Mortgage and Rent tends to default more
#-------------------------------------------------------------------------------------------------------------------
#check for int_rate_grp

int_plot <- ggplot(loan_Driver1,aes(x=int_rate_grp,fill = int_rate_grp))+ geom_bar()+facet_wrap(~loan_status)

int_plot + labs(title="Interest Rate of the Loan and their Status",x = "",y="No of Loan Availed",fill = "Interest Rate")

#based on above analysis from the data who are not paied we see that if the interest rate is medium people tends to default more

#-------------------------------------------------------------------------------------------------------------------

#check for group

grp_plot <- ggplot(loan_Driver1,aes(x=grade,fill = grade))+ geom_bar()+facet_wrap(~loan_status)

grp_plot + labs(title="Grade Assigned by LC and their Status",x = "",y="No of Loan Availed",fill = "Grade")

#based on above analysis from the data who are not paied now we can see some grades like B,C,D have trends to default more 

#-------------------------------------------------------------------------------------------------------------------

#now we need to see if annual income differ on loan status

Inc_Stat <- ggplot(loan_Driver1,aes(x = loan_Driver1$loan_status,y = loan_Driver1$annual_inc,fill = loan_status)) + geom_boxplot()

Inc_Stat + labs(title = "Distribution of Annual Income on Loan Status",x="",y = "Annual Income",fill = "Loan Status")

#now we see the distribution of funded amount on loan status

Fun_Stat <- ggplot(loan_Driver1,aes(x=loan_status,y=funded_amnt,fill = loan_status)) + geom_boxplot()

Fun_Stat + labs(title = "Distribution of Funded Amount On Loan Status",x="",y="Funded Amount",fill = "Loan Status")

#now we see the distribution of loan amount on loan status

Lon_stat <- ggplot(loan_Driver1,aes(x=loan_status,y=loan_amnt,fill = loan_status)) + geom_boxplot()
Lon_stat + labs(title = "Distribution of Loan Amount on Status of Loan",x="",y="Loan Amount",fill = "Loan Status")

#now we check for DTI with loan status

dti_stat <- ggplot(loan_Driver1,aes(x=loan_status,y=dti,fill = loan_status)) + geom_boxplot()
dti_stat + labs(title = "Distribution of DTI over Loan Status",x="",y="Debt To Income Ratio",fill = "Loan Status")

#we again check for DTI on Interest Rate

dti_int <- ggplot(loan_Driver1,aes(x= int_rate_grp,y=dti,fill = int_rate_grp)) + geom_boxplot()
dti_int + labs(title = "Distribution of DTI on Interest Rate Group",x="",y="Debt To Income Ratio",fill = "Interest Rate Group")


#Plot between loan status vs he number of inquiries in past 6 months in Percentage.
#Here we can clearly see that number of inquiry per loan is 1.06 times for defaulted loans which is highest among all
loaninq <- summarise(group_by(loan_Driver, loan_status), sum(inq_last_6mths, na.rm = TRUE))
colnames(loaninq) <- c("loan_status", "inquiry_count")
loaninq$inquiry_percent <- round(loaninq$inquiry_count/table(loan_Driver$loan_status),digits = 2)*100
ggplot(loaninq, aes(x=loaninq$loan_status, y=as.numeric(loaninq$inquiry_percent))) + geom_col(fill="tomato2") +  labs(x = "Loan Status", y = "Percent of No. of Inquiries")  

#Plot between loan status vs delinq2yrs Percentage.
#Here we can clearly see that delinq2yrs Percentage for defaulted loans is highest among all
delinq2yrs <- summarise(group_by(loan_Driver, loan_status), sum(delinq_2yrs, na.rm = TRUE))
colnames(delinq2yrs) <- c("loan_status", "delinq2yrs_count")
delinq2yrs$delinq2yrs_percent <- round(delinq2yrs$delinq2yrs_count/table(loan_Driver$loan_status),digits = 2)*100
ggplot(delinq2yrs, aes(x=delinq2yrs$loan_status, y=as.numeric(delinq2yrs$delinq2yrs_percent))) + geom_col(fill="tomato2") +  labs(x = "Loan Status", y = "Percent of no. of 30+ days past-due incidences of delinquency")  

#Box plot for Payments received to date for total amount funded  for different loan status. 
#Here we can see that The  Payments received to date for total amount funded is smallest  for defaulted loans among all status types.
ggplot(loan_Driver, aes(x = loan_status, y=as.numeric(total_pymnt))) + geom_boxplot() + labs(x = "Loan Status", y = "Payments received to date for total amount funded")

#Box plot for Payments received to date for portion of total amount funded by investors for different loan status. 
#Here we can see that The  Payments received to date for portion of total amount funded by investors is smallest  for defaulted loans among all status types.
ggplot(loan_Driver, aes(x = loan_status, y=as.numeric(total_pymnt_inv))) + geom_boxplot() + labs(x = "Loan Status", y = "Payments received to date for portion of total invested amount")

#Bar plot for Mean of Principal received to date for different loan status. 
#Here we can see that Mean of Principal received to date is smallest  for defaulted loans among all status types.
totalrecprncp <- summarise(group_by(loan_Driver, loan_status), mean(total_rec_prncp, na.rm = TRUE))
colnames(totalrecprncp) <- c("loan_status", "totalrecprncp_mean")
ggplot(totalrecprncp, aes(x=totalrecprncp$loan_status, y=as.numeric(totalrecprncp$totalrecprncp_mean))) + geom_col(fill="tomato2") +   geom_label(aes(label=round(totalrecprncp$totalrecprncp_mean,digits = 0)),vjust=-0.2) +  labs(x = "Loan Status", y = "Mean of Interest received to date")


#Bar plot for Mean of Last total payment amount received for different loan status. 
#Here we can see that The  mean of Last total payment amount received is least for defaulted loans.
lastpymntamnt <- summarise(group_by(loan_Driver, loan_status), mean(last_pymnt_amnt, na.rm = TRUE))
colnames(lastpymntamnt) <- c("loan_status", "lastpymntamnt_mean")
ggplot(lastpymntamnt, aes(x=lastpymntamnt$loan_status, y=as.numeric(lastpymntamnt$lastpymntamnt_mean))) + geom_col(fill="tomato2") +   geom_label(aes(label=round(lastpymntamnt$lastpymntamnt_mean,digits = 0)),vjust=-0.2) +  labs(x = "Loan Status", y = "Mean of Last total payment amount received")


#-------------------------------------------------------------------------------------------------------------------#
#------------------------------------------Multivariate Analysis  Completed -----------------------------------------#
#-------------------------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------------------------#
#------------------------------------------Hypothesis Testing and Final conclusios  -----------------------------------------#
#-------------------------------------------------------------------------------------------------------------------#

#Now we to check whether there is significant difference between mean of two categorical variables
#Loan Status and Interest Rate on continuous variables

#First we will create different sets of data for calculating  hypothesis

#Loan Status

table(loan_Driver1$loan_status)

Loan_Hypo2 <- subset(loan_Driver1,select = c(annual_inc,loan_amnt,funded_amnt,dti,loan_status))

Loan_Current <- subset(Loan_Hypo2,Loan_Hypo2$loan_status == "Current")

Loan_Default <- subset(Loan_Hypo2,Loan_Hypo2$loan_status == "Charged Off")


#1 Test , Whether there is a significant difference between Annual_Default and Annual_Current mean

#Null Hypothesis : H0 : A1 = A2

#Alternative Hypothesis : H1 : A1 <> A2

#Level of Significance at 95% Confidence is Alpha = 1 - 0.95 = 0.05

A1 <- Loan_Current$annual_inc

A2 <- Loan_Default$annual_inc

Test1 <- t.test(A1,A2,alternative = "two.sided",conf.level = 0.95)

Test1


#Since the p-value is less than alpha 2.2e-16 or is less than 0.0001 we can reject the null Hypothesis and say that 
#there is significant difference between means of A1 and A2

#2 Test Whether there is a significant difference between Loan_Amount_Default and Loan_Amount_Current mean

#Null Hypothesis - H0 : L1 = L2

#Alternative Hypothesis - H1 : L1 <> L2

#Level of Significance at 95% Confidence is Alpha = 1 - 0.95 = 0.05

L1 <- Loan_Current$loan_amnt

L2 <- Loan_Default$loan_amnt

Test2 <- t.test(L1,L2,alternative = "two.sided",mu = 0,conf.level = 0.95)

Test2

#Since the p-value is less than alpha 2.2e-16 or is less than 0.0001 we can reject the null Hypothesis and say that 
#there is significant difference between means of L1 and L2

#3 Test Whether there is a significant difference between Funded_Amount_Default and Funded_Amount_Current mean

#Null Hypothesis - H0 : F1 = F2

#Alternative Hypothesis - H1 : F1 <> F2

#Level of Significance at 95% Confidence is Alpha = 1 - 0.95 = 0.05

F1 <- Loan_Current$funded_amnt

F2 <- Loan_Default$funded_amnt

Test3 <- t.test(F1,F2,alternative = "two.sided",conf.level = 0.95)

Test3

#Since the p-value is less than alpha 2.2e-16 or is less than 0.0001 we can reject the null Hypothesis and say that 
#there is significant difference between means of F1 and F2

#4 Test Whether there is a significant difference between DTI_Default and DTI_Current mean

#Null Hypothesis - H0 : D1 = D2

#Alternative Hypothesis - H1 : D1 <> D2

#Level of Significance at 95% Confidence is Alpha = 1 - 0.95 = 0.05

D1 <- Loan_Current$dti

D2 <- Loan_Default$dti

Test4 <- t.test(D1,D2,alternative = "two.sided",conf.level = 0.95)

Test4

#Since the p-value is less than alpha 0.025 or P value = 0.0005771 we can reject the null Hypothesis and say that 
#there is significant difference between means of D1 and D2 with 95% Confidence


#Now we will test for Interest rate group, and befire that we will subset the data

Interest_Hypo <- subset(loan_Driver1,select = c(annual_inc,loan_amnt,funded_amnt,dti,int_rate_grp))

Interest_High <- subset(Interest_Hypo,Interest_Hypo$int_rate_grp == "High")

Interest_Low <- subset(Interest_Hypo,Interest_Hypo$int_rate_grp == "Low")

#Interest Rate Group Test

#5 Test Whether there is a significant difference between Annual_Income_High_Interest_Rate and Annual_Income_Low_Interest mean

#Null Hypothesis - H0 : A3 = A4

#Alternative Hypothesis - H1 : A3 <> A4

#Level of Significance at 95% Confidence is Alpha = 1 - 0.95 = 0.05

A3 <- Interest_High$annual_inc

A4 <- Interest_Low$annual_inc

Test5 <- t.test(A3,A4,alternative = "two.sided",conf.level = 0.95)

Test5

#Since the p-value is less than alpha 2.2e-16 we can reject the null Hypothesis and say that 
#there is significant difference between means of A3 and A4 with 95% Confidence

#6 Test Whether there is a significant difference between Loan_Amount_High_Interest_Rate and Loan_Amount_Low_Interest mean

#Null Hypothesis - H0 : L3 = L4

#Alternative Hypothesis - H1 : L3 <> L4

#Level of Significance at 95% Confidence is Alpha = 1 - 0.95 = 0.05

L3 <- Interest_High$loan_amnt

L4 <- Interest_Low$loan_amnt

Test6 <- t.test(L3,L4,alternative = "two.sided",conf.level = 0.95)

Test6

#Since the p-value is less than alpha 2.2e-16 we can reject the null Hypothesis and say that 
#there is significant difference between means of L3 and L4 with 95% Confidence


#7 Test Whether there is a significant difference between Funded_Amount_High_Interest_Rate and Funded_Amount_Low_Interest mean

#Null Hypothesis - H0 : F3 = F4

#Alternative Hypothesis - H1 : F3 <> F4

#Level of Significance at 95% Confidence is Alpha = 1 - 0.95 = 0.05

F3 <- Interest_High$funded_amnt

F4 <- Interest_Low$funded_amnt

Test7 <- t.test(F3,F4,alternative = "two.sided",conf.level = 0.95)

Test7

#Since the p-value is less than alpha 2.2e-16 we can reject the null Hypothesis and say that 
#there is significant difference between means of F3 and F4 with 95% Confidence

#8 Test Whether there is a significant difference between DTI_High_Interest_Rate and DTI_Low_Interest mean

#Null Hypothesis - H0 : D3 = D4

#Alternative Hypothesis - H1 : D3 <> D4

#Level of Significance at 95% Confidence is Alpha = 1 - 0.95 = 0.05

D3 <- Interest_High$dti

D4 <- Interest_Low$dti

Test8 <- t.test(D3,D4,alternative = "two.sided",conf.level = 0.95)

Test8

#Since the p-value 0.001342 is greater than alpha 0.025 we cannot reject the null Hypothesis and say that 
#there is no significant difference between means of D3 and D4




#-------------------------------------------------------------------------------------------------------------------#
#------------------------------------------Hypothesis Testing and Final conclusios Completed  -----------------------------------------#
#-------------------------------------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------------------------------------#
#-----------------extract the final data set in to CSV for tableau Analysis -----------------------------------------#
#-------------------------------------------------------------------------------------------------------------------#


write.csv(loan_Driver1, file="loan_Driver.csv")

