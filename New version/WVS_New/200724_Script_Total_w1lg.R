###################### Chapter 1 Cleaning the obvious################################

#===================Section 1.1- Getting started==================================

#manually create working directory folder "R WVS" in Documents
#set working directory
setwd("C:/Users/Sophia/Documents/R WVS")
getwd
# manually download csv data (no linebreaks, numeric values, viewed but unanswered questions coded with -99)
# and pull into working directory folder ("R WVS")


#Install qualtRics-package Also see http://adrianbruegger.com/import-qualtrics-csv-files/

install.packages("qualtRics")
library("qualtRics")
#import raw data into R, use the code below and simply edit the filename
raw_data <- read_survey("Leuphana GSR Survey_June 30, 2020_08.18.csv")


#==================Section 1.2- Remove test responses=============================
# Somewhere along the data gathering process you might have tested the 
# questionnaire. Now its time to remove these test responses. 
# A few variables might help to identify these test responses:
# StartDate  Q2_10 Q161 Q1 Q144 Q164 Q166     ResponseId

#load the dplyr package
library(dplyr)

#create new dataframe with the variables of interest
raw_data_tests<-select(raw_data,StartDate,Q2_10,Q161,Q1,Q144,Q164,Q166,ResponseId, Q_RelevantIDFraudScore,Q_RelevantIDDuplicate )


#show the whole raw_data_tests dataframe
raw_data_tests %>% print(n = Inf)

raw_data_tests$ResponseId
#Inspect data
#Have an additional look at the dataset viewer (upper right corner of the R interface) to get the full ResponseId

#create new vector with identified Ids
id_to_remove<-c("R_1Ic8ReiRD4g5lP2", "R_21bqEO1uU6QhcZU", "R_plvF5c2AcLwN6rT", "R_cOPppOqup2XL0C5", "R_9v0FhRykqifHg9H",
                "R_30wPpoGVRNAHtfl")
id_to_remove

#Delete rows/cases with these IDs
print(raw_data$ResponseId==id_to_remove)

#Delete the identified IDs
raw_data<-raw_data[raw_data$ResponseId!="R_1Ic8ReiRD4g5lP2",]
raw_data<-raw_data[raw_data$ResponseId!="R_21bqEO1uU6QhcZU",]
raw_data<-raw_data[raw_data$ResponseId!="R_plvF5c2AcLwN6rT",]
raw_data<-raw_data[raw_data$ResponseId!="R_cOPppOqup2XL0C5",]
raw_data<-raw_data[raw_data$ResponseId!="R_9v0FhRykqifHg9H",]
raw_data<-raw_data[raw_data$ResponseId!="R_30wPpoGVRNAHtfl",]



#==================Section 1.3-Remove participants that disagree Q160=============
raw_data<-raw_data[raw_data$Q160!=2,]

#==================Section 1.4- Remove participants that did not finish===========
raw_data<-raw_data[raw_data$Finished==1,]

#==================Section 1.5- Remove rows that are NA===========
install.packages("tidyr")
library(tidyr)
raw_data<-raw_data[!is.na(raw_data$StartDate), ]

#==================Section 1.6- Remove participants that are fraud or answered twice========
sum(raw_data$Q_RelevantIDFraudScore!=0)

install.packages(dplyr)
library(dplyr)
#save fraudulent cases into new variable
raw_data_fraud<-filter(raw_data, Q_RelevantIDFraudScore !=0)


#Have a look at the cases
print(raw_dta_fraud)
# Exclude cases with a higher fraud score than 30
#"A score greater than or equal to 30 means the response is likely fraudulent and a bot." (Source https://www.qualtrics.com/support/survey-platform/survey-module/survey-checker/fraud-detection/)
# However, make this decision case dependent, for example on additional information like the time it took the respondents to finish the survey

#You could use the following code to exclude all respondents with a score >=30
#raw_data<-raw_data[raw_data$Q_RelevantIDFraudScore<=30,]


#In the case of the first wave of the survey for LG the decision was made to leave to respondents<30 in the sample. There seemed to be a mistake with the timing variables of Qualtrics

#Alternatively, you can also exclude the identified cases with the ID
raw_data<-raw_data[raw_data$ResponseId!="R_3KpgwscUshy4WGm",]
raw_data<-raw_data[!is.na(raw_data$StartDate), ]


#look at distribution of participants between groups by time
table(raw_data$`Duration (in seconds)`, raw_data$group)


#==================Section 1.7- Remove participants that don't live in LG ========

raw_data<-raw_data[raw_data$Q7!="Region #18",]
raw_data<-raw_data[!is.na(raw_data$StartDate), ]


#look at distribution of participants between groups

table(raw_data$group) 
install.packages("gmodels")
library(gmodels)

CrossTable(raw_data$Q7,raw_data$group)


#set all variables to lowercase letters
names(raw_data) <- tolower(names(raw_data))

#Set all values that are -99 to NA
raw_data[raw_data == -99] <- NA


###################### Chapter 2 Exploring raw data ################################

#This chapter is mainly about looking at the data. So note down everything you find odd

#===================Section 2.1- Class and dimensions==================================

# understanding the class of the dataset (result should be data.frame)
class(raw_data)

# view the data dimensions (shows us how many rows and columns there are)
dim(raw_data)


#===================Section 2.2- Overview==================================

# look at column names
colnames(raw_data)

# because we will use the survey with the variable names again for other parts of our study, for now leave the labels the way they are.

# look at the structure of the data
str(raw_data)

# better overview than with str. a preview of  your data, which may reveal issues with 
# the way columns are labeled, how variables are encoded, etc.
library(dplyr)
glimpse(raw_data)

# summary of data - get a better feel for how your data are distributed, which may reveal unusual or extreme values,  
# unexpected missing data, etc. For numeric variables, this means looking at means, quartiles (including the median),
# and extreme values. For character or factor  variables, you may be curious about the number  of times each value appears in the data+
# (i.e. counts), which summary() also reveals.
summary(raw_data)

# show the first 10 rows of the data frame
head(raw_data,n=10)

#show the last 10 rows of the data frame
tail(raw_data,n=10)

# visualize data to identify mistakes, please note down mistakes you found
hist(raw_data$group)

install.packages("Hmisc")
library(Hmisc)

#Please note: the following operation takes some CPU and needs some time, so if you are on a slow computer, please just skip it.

#raw_data_hist<-select(raw_data,q1,q3, q2_10,q4, q179, q180, q8, q6, q181,q182, q183, q7,
#q11,q13,q14,q15,q16,q18,q19,q20,q21,q22,q23,q24,q25,q26,q171,q172,
#q27_1,q27_2,q27_3,q27_4,q27_5,q27_6,q27_7,q27_8,q27_9,q173,q164,q165,q168_1,
#q168_2,q168_3,q168_4,q168_5,q167,q170,q162,q172_1,q172_2,q172_3,q172_4,q172_6,
#q172_7,q172_8,q172_11,q172_14,q172_16,q172_20,q169_1,q169_2,q169_3,q169_4,q169_5,
#q169_6,q170_1,q170_2,q170_3,
#q170_4,	q170_5,	q170_6,	q170_7,	q171_1,	q173_1,	q164_1,	q165_1,	q166,	q184_1,	q184_2,	
#q184_3,	q184_4,	q184_5,	q38	,q40,	q44_1,	q44_2,	q44_3,	q44_4,	q45_1,	q45_2,
#q45_3,	q45_4,	q45_5,	q45_6,	q46_1,	q47_1,	q50_1,	q50_2,	q50_3,	q50_4,	q55_1,	
#q55_2,	q55_3,	q55_4,	q53_1,	q53_2,	q53_3,	q53_4,	q181_1,	q56_1,	q56_2,	q56_3,
#q57_1,	q57_2,	q57_3,	q57_4,	q57_5,	q57_6,	q57_7,	q57_8,	q57_9,	q57_10,	q57_11,	
#q59,	q60_1,	q60_2,	q62_1,	q62_2,	q62_3,	q62_4,	q62_5,	q65_1,	q70_1,	q70_2,	
#q73_1,	q73_2,	q73_3,	q73_4,	q73_5,	q73_6,	q73_7,	q79_1,	q78,	q81,	q83,
#q89_1,	q97_1,	q97_2,	q97_3,	q97_4,	q97_5,	q97_6,	q97_7,	q97_8,	q97_9,	
#q97_10,	q101,	q112_1,	q108_1,	q116_1,	q116_2,	q116_3,	q116_4,	q116_5,	q138_1,	
#q138_2,	q138_3,	q138_4,	q138_5,	q138_6,	q138_7,	q138_8,	random,	group)

#hist.data.frame(raw_data_hist,2,na.big = T)


#write.csv(raw_data,"raw_data.csv",row.names = T)

###################### Chapter 3 Tidying data ################################


#===================Section 3.1 use tidyr==================================

install(tidyr)
library(tidyr)

# Observation should be rows
# Variables should be columns
# One type of observational unit per table

#are variables variables and values values?
print(raw_data)



###################### Chapter 4 Preparing data for analysis ################################


#===================Section 4.1 do type conversions==================================


#Have a look at the variables to find out which need conversion


str(raw_data)


#coerce variables into correct format start with q160

#as.factor(variable)

raw_data$q160<-as.factor(raw_data$q160)

raw_data$q160<-as.factor(raw_data$q160)
raw_data$q1<-as.factor(raw_data$q1)
raw_data$q3<-as.factor(raw_data$q3)
raw_data$q4<-as.factor(raw_data$q4)
raw_data$q179<-as.factor(raw_data$q179)
raw_data$q8<-as.factor(raw_data$q8)
raw_data$q6<-as.factor(raw_data$q6)
raw_data$q182<-as.factor(raw_data$q182)
raw_data$q183<-as.factor(raw_data$q183)
raw_data$q7<-as.factor(raw_data$q7)
raw_data$q171<-as.factor(raw_data$q171)
raw_data$q172<-as.factor(raw_data$q172)
raw_data$q173<-as.factor(raw_data$q173)
raw_data$q165<-as.factor(raw_data$q165)
raw_data$q167<-as.factor(raw_data$q167)
raw_data$q170<-as.factor(raw_data$q170)
raw_data$q162<-as.factor(raw_data$q162)
raw_data$q163<-as.factor(raw_data$q163)
raw_data$q169_1<-as.factor(raw_data$q169_1)
raw_data$q169_2<-as.factor(raw_data$q169_2)
raw_data$q169_3<-as.factor(raw_data$q169_3)
raw_data$q169_4<-as.factor(raw_data$q169_4)
raw_data$q169_5<-as.factor(raw_data$q169_5)
raw_data$q169_6<-as.factor(raw_data$q169_6)
raw_data$q170_1<-as.factor(raw_data$q170_1)
raw_data$q170_2<-as.factor(raw_data$q170_2)
raw_data$q170_3<-as.factor(raw_data$q170_3)
raw_data$q170_4<-as.factor(raw_data$q170_4)
raw_data$q170_5<-as.factor(raw_data$q170_5)
raw_data$q170_6<-as.factor(raw_data$q170_6)
raw_data$q170_7<-as.factor(raw_data$q170_7)
raw_data$q171_1<-as.factor(raw_data$q171_1)
raw_data$q173_1<-as.factor(raw_data$q173_1)
raw_data$q164_1<-as.factor(raw_data$q164_1)
raw_data$q165_1<-as.factor(raw_data$q165_1)
raw_data$q38<-as.factor(raw_data$q38)
raw_data$q40<-as.factor(raw_data$q40)
raw_data$q44_1<-as.factor(raw_data$q44_1)
raw_data$q44_2<-as.factor(raw_data$q44_2)
raw_data$q44_3<-as.factor(raw_data$q44_3)
raw_data$q44_4<-as.factor(raw_data$q44_4)
raw_data$q45_1<-as.factor(raw_data$q45_1)
raw_data$q45_2<-as.factor(raw_data$q45_2)
raw_data$q45_3<-as.factor(raw_data$q45_3)
raw_data$q45_4<-as.factor(raw_data$q45_4)
raw_data$q45_5<-as.factor(raw_data$q45_5)
raw_data$q45_6<-as.factor(raw_data$q45_6)
raw_data$q50_1<-as.factor(raw_data$q50_1)
raw_data$q50_2<-as.factor(raw_data$q50_2)
raw_data$q50_3<-as.factor(raw_data$q50_3)
raw_data$q50_4<-as.factor(raw_data$q50_4)
raw_data$q55_1<-as.factor(raw_data$q55_1)
raw_data$q55_2<-as.factor(raw_data$q55_2)
raw_data$q55_3<-as.factor(raw_data$q55_3)
raw_data$q55_4<-as.factor(raw_data$q55_4)
raw_data$q53_1<-as.factor(raw_data$q53_1)
raw_data$q53_2<-as.factor(raw_data$q53_2)
raw_data$q53_3<-as.factor(raw_data$q53_3)
raw_data$q53_4<-as.factor(raw_data$q53_4)
raw_data$q56_1<-as.factor(raw_data$q56_1)
raw_data$q56_2<-as.factor(raw_data$q56_2)
raw_data$q56_3<-as.factor(raw_data$q56_3)
raw_data$q57_1<-as.factor(raw_data$q57_1)
raw_data$q57_2<-as.factor(raw_data$q57_2)
raw_data$q57_3<-as.factor(raw_data$q57_3)
raw_data$q57_4<-as.factor(raw_data$q57_4)
raw_data$q57_5<-as.factor(raw_data$q57_5)
raw_data$q57_6<-as.factor(raw_data$q57_6)
raw_data$q57_7<-as.factor(raw_data$q57_7)
raw_data$q57_8<-as.factor(raw_data$q57_8)
raw_data$q57_9<-as.factor(raw_data$q57_9)
raw_data$q57_10<-as.factor(raw_data$q57_10)
raw_data$q57_11<-as.factor(raw_data$q57_11)
raw_data$q59<-as.factor(raw_data$q59)
raw_data$q60_1<-as.factor(raw_data$q60_1)
raw_data$q60_2<-as.factor(raw_data$q60_2)
raw_data$q62_1<-as.factor(raw_data$q62_1)
raw_data$q62_2<-as.factor(raw_data$q62_2)
raw_data$q62_3<-as.factor(raw_data$q62_3)
raw_data$q62_4<-as.factor(raw_data$q62_4)
raw_data$q62_5<-as.factor(raw_data$q62_5)
raw_data$q70_1<-as.factor(raw_data$q70_1)
raw_data$q70_2<-as.factor(raw_data$q70_2)
raw_data$q73_1<-as.factor(raw_data$q73_1)
raw_data$q73_2<-as.factor(raw_data$q73_2)
raw_data$q73_3<-as.factor(raw_data$q73_3)
raw_data$q73_4<-as.factor(raw_data$q73_4)
raw_data$q73_5<-as.factor(raw_data$q73_5)
raw_data$q73_6<-as.factor(raw_data$q73_6)
raw_data$q73_7<-as.factor(raw_data$q73_7)
raw_data$q78<-as.factor(raw_data$q78)
raw_data$q81<-as.factor(raw_data$q81)
raw_data$q83<-as.factor(raw_data$q83)
raw_data$q97_1<-as.factor(raw_data$q97_1)
raw_data$q97_2<-as.factor(raw_data$q97_2)
raw_data$q97_3<-as.factor(raw_data$q97_3)
raw_data$q97_4<-as.factor(raw_data$q97_4)
raw_data$q97_5<-as.factor(raw_data$q97_5)
raw_data$q97_6<-as.factor(raw_data$q97_6)
raw_data$q97_7<-as.factor(raw_data$q97_7)
raw_data$q97_8<-as.factor(raw_data$q97_8)
raw_data$q97_9<-as.factor(raw_data$q97_9)
raw_data$q97_10<-as.factor(raw_data$q97_10)
raw_data$q101<-as.factor(raw_data$q101)
raw_data$q116_1<-as.factor(raw_data$q116_1)
raw_data$q116_2<-as.factor(raw_data$q116_2)
raw_data$q116_3<-as.factor(raw_data$q116_3)
raw_data$q116_4<-as.factor(raw_data$q116_4)
raw_data$q116_5<-as.factor(raw_data$q116_5)
raw_data$q138_1<-as.factor(raw_data$q138_1)
raw_data$q138_2<-as.factor(raw_data$q138_2)
raw_data$q138_3<-as.factor(raw_data$q138_3)
raw_data$q138_4<-as.factor(raw_data$q138_4)
raw_data$q138_5<-as.factor(raw_data$q138_5)
raw_data$q138_6<-as.factor(raw_data$q138_6)
raw_data$q138_7<-as.factor(raw_data$q138_7)
raw_data$q138_8<-as.factor(raw_data$q138_8)
raw_data$random<-as.factor(raw_data$random)
raw_data$group<-as.factor(raw_data$group)

#if you want to look at these variables now, please use the barchart function


#as.numeric(variable)
raw_data$q2_10<-as.numeric(raw_data$q2_10)
raw_data$q180<-as.numeric(raw_data$q180)
raw_data$q181<-as.numeric(raw_data$q181)
raw_data$q10<-as.numeric(raw_data$q10)
raw_data$q11<-as.numeric(raw_data$q11)
raw_data$q13<-as.numeric(raw_data$q13)
raw_data$q14<-as.numeric(raw_data$q14)
raw_data$q15<-as.numeric(raw_data$q15)
raw_data$q16<-as.numeric(raw_data$q16)
raw_data$q18<-as.numeric(raw_data$q18)
raw_data$q19<-as.numeric(raw_data$q19)
raw_data$q20<-as.numeric(raw_data$q20)
raw_data$q21<-as.numeric(raw_data$q21)
raw_data$q22<-as.numeric(raw_data$q22)
raw_data$q23<-as.numeric(raw_data$q23)
raw_data$q24<-as.numeric(raw_data$q24)
raw_data$q25<-as.numeric(raw_data$q25)
raw_data$q26<-as.numeric(raw_data$q26)
raw_data$q27_1<-as.numeric(raw_data$q27_1)
raw_data$q27_2<-as.numeric(raw_data$q27_2)
raw_data$q27_3<-as.numeric(raw_data$q27_3)
raw_data$q27_4<-as.numeric(raw_data$q27_4)
raw_data$q27_5<-as.numeric(raw_data$q27_5)
raw_data$q27_6<-as.numeric(raw_data$q27_6)
raw_data$q27_7<-as.numeric(raw_data$q27_7)
raw_data$q27_8<-as.numeric(raw_data$q27_8)
raw_data$q27_9<-as.numeric(raw_data$q27_9)
raw_data$q168_1<-as.numeric(raw_data$q168_1)
raw_data$q168_2<-as.numeric(raw_data$q168_2)
raw_data$q168_3<-as.numeric(raw_data$q168_3)
raw_data$q168_4<-as.numeric(raw_data$q168_4)
raw_data$q168_5<-as.numeric(raw_data$q168_5)
raw_data$q184_1<-as.numeric(raw_data$q184_1)
raw_data$q184_2<-as.numeric(raw_data$q184_2)
raw_data$q184_3<-as.numeric(raw_data$q184_3)
raw_data$q184_4<-as.numeric(raw_data$q184_4)
raw_data$q184_5<-as.numeric(raw_data$q184_5)
raw_data$q46_1<-as.numeric(raw_data$q46_1)
raw_data$q47_1<-as.numeric(raw_data$q47_1)
raw_data$q181_1<-as.numeric(raw_data$q181_1)
raw_data$q65_1<-as.numeric(raw_data$q65_1)
raw_data$q79_1<-as.numeric(raw_data$q79_1)
raw_data$q89_1<-as.numeric(raw_data$q89_1)
raw_data$q112_1<-as.numeric(raw_data$q112_1)
raw_data$q108_1<-as.numeric(raw_data$q108_1)


#as.character (variable)
raw_data$q161<-as.character(raw_data$q161)
raw_data$q144<-as.character(raw_data$q144)
raw_data$q173_4_text<-as.character(raw_data$q173_4_text)
raw_data$q164<-as.character(raw_data$q164)
raw_data$q162_7_text<-as.character(raw_data$q162_7_text)
raw_data$q163_7_text<-as.character(raw_data$q163_7_text)
raw_data$q166<-as.character(raw_data$q166)


str(raw_data)

#Don't run,because this will turn every variable to numeric:
#raw_data <- mutate_at(raw_data, vars(q160:group), funs(as.numeric))



#===================Section 4.2 further recoding and tuning==================================

#transform personalized codeword to all lower case to make later matching easier. 
raw_data$q161<-tolower(raw_data$q161)



# Recode seconds variable into new variable surveytime (in minutes)

raw_data$surveytime<-((raw_data$`duration (in seconds)`)/60)
raw_data$surveytime


# Dealing with dates

#the lubridate package helps with dates. Coerce strings to dates
#install.packages("lubridate")
#library(lubridate) 


install.packages("lubridate")
library(lubridate)
raw_data$startdate2<-ymd_hms(raw_data$startdate)
raw_data$enddate2<-ymd_hms(raw_data$enddate)


#Missing and special values

#Are ther any missing values in the data?
any(is.na(raw_data))

#count number of NAs
sum(is.na(raw_data))

#use summary to find missing values
summary(raw_data)
barchart(raw_data$q7)


#Outliers and obvious errors
# Identify values so extreme they cannot be plausible
# Identify values that don't make sense



#How to change values: 
# Change 1000 to 100
#weather6$Max.Humidity[ind] <- 100


#===================Section 4.3 label data==================================

#I think the expss package makes sense here
summary(raw_data$q8)

install.packages("expss")
library(expss)
barchart(raw_data$q15)


with(raw_data, table(q8, q15))

#var labeling data var_lab(raw_data$q1)	 =	"	What gender are you?	"

var_lab(raw_data$startdate	)	 =	"Start Date"
var_lab(raw_data$enddate	)	 =	"End Date"
var_lab(raw_data$status	)	 =	"Response Type"
var_lab(raw_data$progress	)	 =	"Progress"
var_lab(raw_data$finished	)	 =	"Finished"
var_lab(raw_data$recordeddate	)	 =	"Recorded Date"
var_lab(raw_data$responseid	)	 =	"Response ID"
var_lab(raw_data$distributionchannel	)	 =	"Distribution Channel"
var_lab(raw_data$userlanguage	)	 =	"User Language"
var_lab(raw_data$q_relevantidduplicate	)	 =	"	Q_RelevantIDDuplicate"
var_lab(raw_data$q_relevantidduplicatescore	)	 =	"Q_RelevantIDDuplicateScore"
var_lab(raw_data$q_relevantidfraudscore	)	 =	"Q_RelevantIDFraudScore"
var_lab(raw_data$q_relevantidlaststartdate	)	 =	"Q_RelevantIDLastStartDate"
var_lab(raw_data$q162_first click	)	 =	"Timing - First Click"
var_lab(raw_data$q162_last click	)	 =	"Timing - Last Click"
var_lab(raw_data$q162_page submit	)	 =	"Timing - Page Submit"
var_lab(raw_data$q162_click count	)	 =	"Timing - Click Count"
var_lab(raw_data$q160	)	 =	"Welcome Text + Consent"
var_lab(raw_data$q161	)	 =	"Individual code word"
var_lab(raw_data$q163_first click	)	 =	"Timing - First Click"
var_lab(raw_data$q163_last click	)	 =	"Timing - Last Click"
var_lab(raw_data$q163_page submit	)	 =	"Timing - Page Submit"
var_lab(raw_data$q163_click count	)	 =	"Timing - Click Count"
var_lab(raw_data$q1	)	 =	"What gender are you?"
var_lab(raw_data$q2_10	)	 =	"How old are you? - Age in Years"
var_lab(raw_data$q3	)	 =	"Were you born in this country, or are you an immigrant?"
var_lab(raw_data$q4	)	 =	"What is the highest educational level that you have attained?"
var_lab(raw_data$q179	)	 =	"Are you currently"
var_lab(raw_data$q180	)	 =	"Have you had any children?"
var_lab(raw_data$q8	)	 =	"Are you working for the government or public institution, for private business or industry, or for a private non-profit organization? If you do not work currently, characterize your major work in the past! Do you or did you work for"
var_lab(raw_data$q144	)	 =	"Please specify in which field you work"
var_lab(raw_data$q6	)	 =	"What is your regular monthly household income after tax? This means the sum of any wage, salary, income from self-employment or pension after subtracting taxes. Additionally, please include any form of public aid, income from rentals or lease, housing money, child allowance, or other earnings. If you do not share your income with your household (e.g. live in a shared flat) please state your individual income."
var_lab(raw_data$q181	)	 =	"Including yourself, how many people live permanently in your household? Please also consider all children that live in your household. If you indicated your individual income in the previous question (e.g. because you live in a shared flat), please state 1."
var_lab(raw_data$q182	)	 =	"Are you the chief wage earner in your household?"
var_lab(raw_data$q183	)	 =	"Is the chief wage earner of your household employed now or not?"
var_lab(raw_data$q7_1_x	)	 =	"Where in Lüneburg do you live? If you do not live in Lüneburg, please click on the shield."
var_lab(raw_data$q7_1_y	)	 =	"Where in Lüneburg do you live? If you do not live in Lüneburg, please click on the shield."
var_lab(raw_data$q7	)	 =	"Where in Lüneburg do you live? If you do not live in Lüneburg, please click on the shield. - Regions"
var_lab(raw_data$q10	)	 =	"We are approaching the limit of the number of people the earth can support."
var_lab(raw_data$q11	)	 =	"Humans have the right to modify the natural environment to suit their needs."
var_lab(raw_data$q13	)	 =	"When humans interfere with nature it often produces disastrous consequences."
var_lab(raw_data$q14	)	 =	"Human ingenuity will insure that we do NOT make the earth unlivable."
var_lab(raw_data$q15	)	 =	"Humans are severely abusing the environment."
var_lab(raw_data$q16	)	 =	"The earth has plenty of natural resources if we just learn how to develop them."
var_lab(raw_data$q18	)	 =	"Plants and animals have as much right as humans to exist."
var_lab(raw_data$q19	)	 =	"The balance of nature is strong enough to cope with impacts of modern industrial nations."
var_lab(raw_data$q20	)	 =	"Despite our special abilities humans are still subject to the laws of nature."
var_lab(raw_data$q21	)	 =	"The so-called 'ecological crisis' facing humankind has been greatly exaggerated."
var_lab(raw_data$q22	)	 =	"The earth is like a spaceship with very limited room and resources."
var_lab(raw_data$q23	)	 =	"Humans were meant to rule over the rest of nature."
var_lab(raw_data$q24	)	 =	"The balance of nature is very delicate and easily upset."
var_lab(raw_data$q25	)	 =	"Humans will eventually learn enough about how nature works to be able to control it."
var_lab(raw_data$q26	)	 =	"If things continue on their present course, we will soon experience a major ecological catastrophe."
var_lab(raw_data$q171	)	 =	"You may have heard that the climate on the earth is changing because temperatures rose over the past 100 years. Do you think the world's climate is changing?"
var_lab(raw_data$q172	)	 =	"Do you think that climate change is caused by natural processes, human activity, or both?"
var_lab(raw_data$q27_1	)	 =	"People around the world are generally concerned about environmental problems because of the consequences that result from harming nature. However, people differ in the consequences that concern them the most. Please rate the following items from 1 (not important) to 5 (extreme importance) in response to the question: I am concerned about environmental problems because of the consequences for ____. - my future"
var_lab(raw_data$q27_2	)	 =	"People around the world are generally concerned about environmental problems because of the consequences that result from harming nature. However, people differ in the consequences that concern them the most. Please rate the following items from 1 (not important) to 5 (extreme importance) in response to the question: I am concerned about environmental problems because of the consequences for ____. - my lifestyle"
var_lab(raw_data$q27_3	)	 =	"People around the world are generally concerned about environmental problems because of the consequences that result from harming nature. However, people differ in the consequences that concern them the most. Please rate the following items from 1 (not important) to 5 (extreme importance) in response to the question: I am concerned about environmental problems because of the consequences for ____. - my health"
var_lab(raw_data$q27_4	)	 =	"People around the world are generally concerned about environmental problems because of the consequences that result from harming nature. However, people differ in the consequences that concern them the most. Please rate the following items from 1 (not important) to 5 (extreme importance) in response to the question: I am concerned about environmental problems because of the consequences for ____. - humanity"
var_lab(raw_data$q27_5	)	 =	"People around the world are generally concerned about environmental problems because of the consequences that result from harming nature. However, people differ in the consequences that concern them the most. Please rate the following items from 1 (not important) to 5 (extreme importance) in response to the question: I am concerned about environmental problems because of the consequences for ____. - future generations"
var_lab(raw_data$q27_6	)	 =	"People around the world are generally concerned about environmental problems because of the consequences that result from harming nature. However, people differ in the consequences that concern them the most. Please rate the following items from 1 (not important) to 5 (extreme importance) in response to the question: I am concerned about environmental problems because of the consequences for ____. - people in the community"
var_lab(raw_data$q27_7	)	 =	"People around the world are generally concerned about environmental problems because of the consequences that result from harming nature. However, people differ in the consequences that concern them the most. Please rate the following items from 1 (not important) to 5 (extreme importance) in response to the question: I am concerned about environmental problems because of the consequences for ____. - plants and trees"
var_lab(raw_data$q27_8	)	 =	"People around the world are generally concerned about environmental problems because of the consequences that result from harming nature. However, people differ in the consequences that concern them the most. Please rate the following items from 1 (not important) to 5 (extreme importance) in response to the question: I am concerned about environmental problems because of the consequences for ____. - marine life and whales"
var_lab(raw_data$q27_9	)	 =	"People around the world are generally concerned about environmental problems because of the consequences that result from harming nature. However, people differ in the consequences that concern them the most. Please rate the following items from 1 (not important) to 5 (extreme importance) in response to the question: I am concerned about environmental problems because of the consequences for ____. - land animals and birds"
var_lab(raw_data$q173	)	 =	"What type of diet do you follow? - Selected Choice"
var_lab(raw_data$q173_4_text	)	 =	"What type of diet do you follow? - Other - Text"
var_lab(raw_data$q164	)	 =	"What is the first image or thought that comes to your mind when thinking about crisis?"
var_lab(raw_data$q165	)	 =	"How strong is your emotional reaction to this image/thought?"
var_lab(raw_data$q168_1	)	 =	"Over the last four weeks, how often have you been bothered by the following problems? - Feeling nervous, anxious, or on edge."
var_lab(raw_data$q168_2	)	 =	"Over the last four weeks, how often have you been bothered by the following problems? - Not being able to stop or control worrying."
var_lab(raw_data$q168_3	)	 =	"Over the last four weeks, how often have you been bothered by the following problems? - Feeling down, depressed or hopeless."
var_lab(raw_data$q168_4	)	 =	"Over the last four weeks, how often have you been bothered by the following problems? - Little interest or pleasure in doing things."
var_lab(raw_data$q168_5	)	 =	"Over the last four weeks, how often have you been bothered by the following problems? - I have felt lonely."
var_lab(raw_data$q167	)	 =	"Who are you worried about the most?	"
var_lab(raw_data$q170	)	 =	"To what extent do you currently feel at risk?"
var_lab(raw_data$q162	)	 =	"Which of the following areas in your life are currently at risk? - Selected Choice"
var_lab(raw_data$q162_7_text	)	 =	"Which of the following areas in your life are currently at risk? - Other - Text"
var_lab(raw_data$q163	)	 =	"Of the areas you selected, which one are you most worried about? - Selected Choice"
var_lab(raw_data$q163_7_text	) =	"Of the areas you selected, which one are you most worried about? - Other - Text"
var_lab(raw_data$q172_1	)	 =	"Please drag and drop the following items to rank them by what you consider to be the biggest potential global risk to the lowest global risk. Items contain possible examples of specific risks. When ranking the items please don't think about yourself as an individual but rather about the world as a whole. - Loss of privacy (to companies or to governments)"
var_lab(raw_data$q172_2	)	 =	"Please drag and drop the following items to rank them by what you consider to be the biggest potential global risk to the lowest global risk. Items contain possible examples of specific risks. When ranking the items please don't think about yourself as an individual but rather about the world as a whole. - Cyberattacks & cybercrime (disruption of operations and infrastructure, theft of data or money, criminal use of cryptocurrencies, identity theft)"
var_lab(raw_data$q172_3	)	 =	"Please drag and drop the following items to rank them by what you consider to be the biggest potential global risk to the lowest global risk. Items contain possible examples of specific risks. When ranking the items please don't think about yourself as an individual but rather about the world as a whole. - International military confrontations (State-on-state military conflict, military actions short of war)"
var_lab(raw_data$q172_4	)	 =	"Please drag and drop the following items to rank them by what you consider to be the biggest potential global risk to the lowest global risk. Items contain possible examples of specific risks. When ranking the items please don't think about yourself as an individual but rather about the world as a whole. - Economic instability (Market collapse of stock or other assets, currency crisis, debt defaults (public or private), major recession in an economy, economic confrontation between two major powers, erosion of global supply chains, crisis-driven or economic migration, ineffective monetary stimuli)"
var_lab(raw_data$q172_6	)	 =	"Please drag and drop the following items to rank them by what you consider to be the biggest potential global risk to the lowest global risk. Items contain possible examples of specific risks. When ranking the items please don't think about yourself as an individual but rather about the world as a whole. - Uncertainty regarding jobs/Changing job market (High levels of youth unemployment, job losses due to technology)"
var_lab(raw_data$q172_7	)	 =	"Please drag and drop the following items to rank them by what you consider to be the biggest potential global risk to the lowest global risk. Items contain possible examples of specific risks. When ranking the items please don't think about yourself as an individual but rather about the world as a whole. - Extreme environmental conditions/Impacts of environmental degradation (Uncontrolled fires, water crisis, extreme heat waves, involuntary climate-related migration, erosion of global policy coordination on climate change, destruction of natural ecosystems, human health impacted by air, water pollution and plastic)"
var_lab(raw_data$q172_8	)	 =	"Please drag and drop the following items to rank them by what you consider to be the biggest potential global risk to the lowest global risk. Items contain possible examples of specific risks. When ranking the items please don't think about yourself as an individual but rather about the world as a whole. - Terrorist attacks"
var_lab(raw_data$q172_11	)	 =	"Please drag and drop the following items to rank them by what you consider to be the biggest potential global risk to the lowest global risk. Items contain possible examples of specific risks. When ranking the items please don't think about yourself as an individual but rather about the world as a whole. - Domestic political disruptions (civil unrest (including strikes and riots), foreign interference in domestic politics, public anger against elites)"
var_lab(raw_data$q172_14	)	 =	"Please drag and drop the following items to rank them by what you consider to be the biggest potential global risk to the lowest global risk. Items contain possible examples of specific risks. When ranking the items please don't think about yourself as an individual but rather about the world as a whole. - Deep or widespread poverty or inequality within countries"
var_lab(raw_data$q172_16	)	 =	"Please drag and drop the following items to rank them by what you consider to be the biggest potential global risk to the lowest global risk. Items contain possible examples of specific risks. When ranking the items please don't think about yourself as an individual but rather about the world as a whole. - Weakened institutions (Loss of trust in media and information sources, Loss of confidence in collective security alliances, corrupt leadership, Domestic political polarization)"
var_lab(raw_data$q172_20	)	 =	"Please drag and drop the following items to rank them by what you consider to be the biggest potential global risk to the lowest global risk. Items contain possible examples of specific risks. When ranking the items please don't think about yourself as an individual but rather about the world as a whole. - Nationalism (Populist & nativist agendas, hostility against minorities, authoritarian leadership, protectionism against foreign workers, protectionism regarding trade and investment)"
var_lab(raw_data$q169_1	)	 =	"Considering your health situation during the Corona crisis, please state for each of the following experiences whether or not it happened to you (voluntary information). - I have been tested positively for COVID-19."
var_lab(raw_data$q169_2	)	 =	"Considering your health situation during the Corona crisis, please state for each of the following experiences whether or not it happened to you (voluntary information). - I have been tested negatively for COVID-19."
var_lab(raw_data$q169_3	)	 =	"Considering your health situation during the Corona crisis, please state for each of the following experiences whether or not it happened to you (voluntary information). - I have or had mild symptoms of COVID-19."
var_lab(raw_data$q169_4	)	 =	"Considering your health situation during the Corona crisis, please state for each of the following experiences whether or not it happened to you (voluntary information). - I have or had severe symptoms of COVID-19."
var_lab(raw_data$q169_5	)	 =	"Considering your health situation during the Corona crisis, please state for each of the following experiences whether or not it happened to you (voluntary information). - People close to me have or had mild symptoms of COVID-19."
var_lab(raw_data$q169_6	)	 =	"Considering your health situation during the Corona crisis, please state for each of the following experiences whether or not it happened to you (voluntary information). - People close to me have or had severe symptoms of COVID-19."
var_lab(raw_data$q170_1	)	 =	"Please, state for each of the following economic experiences whether or not it happened to you during the Corona crisis (voluntary information). - I lost my job."
var_lab(raw_data$q170_2	)	 =	"Please, state for each of the following economic experiences whether or not it happened to you during the Corona crisis (voluntary information). - I had to close my business."
var_lab(raw_data$q170_3	)	 =	"Please, state for each of the following economic experiences whether or not it happened to you during the Corona crisis (voluntary information). - I am reduced to part time work."
var_lab(raw_data$q170_4	)	 =	"Please, state for each of the following economic experiences whether or not it happened to you during the Corona crisis (voluntary information). - I am doing home office."
var_lab(raw_data$q170_5	)	 =	"Please, state for each of the following economic experiences whether or not it happened to you during the Corona crisis (voluntary information). - I receive money from an aid package."
var_lab(raw_data$q170_6	)	 =	"Please, state for each of the following economic experiences whether or not it happened to you during the Corona crisis (voluntary information). - I go to work as before."
var_lab(raw_data$q170_7	)	 = "Please, state for each of the following economic experiences whether or not it happened to you during the Corona crisis (voluntary information). - I daycare my children."
var_lab(raw_data$q171	)	 =	"The social media are full of stories telling that the Corona pandemic is a hoax and that all the lockdown measures are a hysterical overreaction. Do you believe in these stories?"
var_lab(raw_data$q173	)	 =	"How credible do you think are the social media, like Twitter and Facebook, compared to the traditional media, like TV and newspapers?"
var_lab(raw_data$q164	)	 =	"How do you feel about the length of this survey?"
var_lab(raw_data$q165	)	 =	"How do you rate the overall design of this survey?"
var_lab(raw_data$q166	)	 =	"Do you have any comments on this survey or would you like to say something further on the topic?"
var_lab(raw_data$q184_1	)	 =	"	All things considered, how satisfied are you these days with your: - health condition"
var_lab(raw_data$q184_2	)	 =	"	All things considered, how satisfied are you these days with your: - financial situation"
var_lab(raw_data$q184_3	)	 =	"	All things considered, how satisfied are you these days with your: - social relations"
var_lab(raw_data$q184_4	)	 =	"All things considered, how satisfied are you these days with your: - work-life balance"
var_lab(raw_data$q184_5	)	 = "All things considered, how satisfied are you these days with your: - life as a whole"
var_lab(raw_data$q38	)	 =	"Here is a list of qualities that children can be encouraged to learn at home. Which, if any, do you consider to be especially important? Please choose up to five!"
var_lab(raw_data$q40	)	 =	"Generally speaking, would you say that most people can be trusted or that you need to be very careful in dealing with people"
var_lab(raw_data$q44_1	)	 =	"Do you agree, disagree or neither agree nor disagree with the following statements? - When jobs are scarce, men should have more right to a job than women."
var_lab(raw_data$q44_2	)	 =	"Do you agree, disagree or neither agree nor disagree with the following statements? - When jobs are scarce, employers should give priority to people of this country over immigrants."
var_lab(raw_data$q44_3	)	 =	"Do you agree, disagree or neither agree nor disagree with the following statements? - If a woman earns more money than her husband, it's almost certain to cause problems."
var_lab(raw_data$q44_4	)	 =	"Do you agree, disagree or neither agree nor disagree with the following statements? - Having a job is the best way for a woman to be an independent person."
var_lab(raw_data$q45_1	)	 =	"For each of the following statements, please mark how strongly you agree or disagree with each. Do you strongly agree, agree, disagree, or strongly disagree? - One of my main goals in life has been to make my parents proud"
var_lab(raw_data$q45_2	)	 =	"For each of the following statements, please mark how strongly you agree or disagree with each. Do you strongly agree, agree, disagree, or strongly disagree? - When a mother works for pay, the children suffer."
var_lab(raw_data$q45_3	)	 =	"For each of the following statements, please mark how strongly you agree or disagree with each. Do you strongly agree, agree, disagree, or strongly disagree? - On the whole, men make better political leaders than women do."
var_lab(raw_data$q45_4	)	 =	"For each of the following statements, please mark how strongly you agree or disagree with each. Do you strongly agree, agree, disagree, or strongly disagree? - A university education is more important for a boy than for a girl."
var_lab(raw_data$q45_5	)	 =	"For each of the following statements, please mark how strongly you agree or disagree with each. Do you strongly agree, agree, disagree, or strongly disagree? - On the whole, men make better  business executives than women do."
var_lab(raw_data$q45_6	)	 =	"For each of the following statements, please mark how strongly you agree or disagree with each. Do you strongly agree, agree, disagree, or strongly disagree? - Being a housewife is just as fulfilling as working for pay."
var_lab(raw_data$q46_1	)	 =	"Some people feel they have completely free choice and control over their lives, while other people feel that what they do has no real effect on what happens to them. Please use this scale where 1 means 'no choice at all' and 10 means 'a great deal of choice' to indicate how much freedom of choice and control you feel you have over the way your life turns out - No choice at all:A great deal of choice"
var_lab(raw_data$q47_1	)	 =	"Do you think most people would try to take advantage of you if they got a chance, or would they try to be fair? Please show your response on this scale, where 1 means that 'people would try to take advantage of you,' and 10 means that 'people would try to be fair' - People would try to take advantage of you:People would try to be fair"
var_lab(raw_data$q50_1	)	 =	"People sometimes talk about what the aims of this country should be for the next ten years. Here you can see some of the goals which different people would give top priority. Please indicate which goal you would consider most important and which would be the next most important. - A high level of economic growth"
var_lab(raw_data$q50_2	)	 =	"People sometimes talk about what the aims of this country should be for the next ten years. Here you can see some of the goals which different people would give top priority. Please indicate which goal you would consider most important and which would be the next most important. - Making sure this country has strong defense forces"
var_lab(raw_data$q50_3	)	 =	"People sometimes talk about what the aims of this country should be for the next ten years. Here you can see some of the goals which different people would give top priority. Please indicate which goal you would consider most important and which would be the next most important. - Seeing that people have more say about how things are done at their jobs and in their communities"
var_lab(raw_data$q50_4	)	 =	"People sometimes talk about what the aims of this country should be for the next ten years. Here you can see some of the goals which different people would give top priority. Please indicate which goal you would consider most important and which would be the next most important. - Trying to make our cities and countryside more beautiful"
var_lab(raw_data$q55_1	)	 =	"Here is another list. In your opinion, which one of these is most important? And what would be the next most important? - A stable economy"
var_lab(raw_data$q55_2	)	 =	"Here is another list. In your opinion, which one of these is most important? And what would be the next most important? - Progress toward a less impersonal and more humane society"
var_lab(raw_data$q55_3	)	 =	"Here is another list. In your opinion, which one of these is most important? And what would be the next most important? - Progress toward a society in which ideas count more than money"
var_lab(raw_data$q55_4	)	 =	"Here is another list. In your opinion, which one of these is most important? And what would be the next most important? - The fight against crime"
var_lab(raw_data$q53_1	)	 =	"If you had to choose, which one of the following things would you say is most important?  And which would be the next most important? - Maintaining order in the nation"
var_lab(raw_data$q53_2	)	 =	"If you had to choose, which one of the following things would you say is most important?  And which would be the next most important? - Giving people more say in important government decisions"
var_lab(raw_data$q53_3	)	 =	"If you had to choose, which one of the following things would you say is most important?  And which would be the next most important? - Fighting rising prices"
var_lab(raw_data$q53_4	)	 =	"If you had to choose, which one of the following things would you say is most important?  And which would be the next most important? - Protecting freedom of speech"
var_lab(raw_data$q181_1	)	 =	"Talking about political priorities, do you think that our government should give top priority to solve our own country's problems by ourselves or should it give top priority to solving global problems in cooperation with other countries: - Solving our own problems by ourselves:Solving global problems in cooperation"
var_lab(raw_data$q56_1	)	 =	"The following is a list of various changes in our way of life that might take place in the near future. Please say for each one, if it were to happen, whether you think it would be a good thing, a bad thing, or don't you mind? - Less importance placed on work in our lives"
var_lab(raw_data$q56_2	)	 =	"The following is a list of various changes in our way of life that might take place in the near future. Please say for each one, if it were to happen, whether you think it would be a good thing, a bad thing, or don't you mind? - More emphasis on the development of technology"
var_lab(raw_data$q56_3	)	 =	"The following is a list of various changes in our way of life that might take place in the near future. Please say for each one, if it were to happen, whether you think it would be a good thing, a bad thing, or don't you mind? - Greater respect for authority"
var_lab(raw_data$q57_1	)	 =	"Some people will briefly be described. Please indicate for each description whether that person is very much like you, like you, somewhat like you, not like you, or not at all like you. - It is important to this person to think up new ideas and be creative; to do things one's own way."
var_lab(raw_data$q57_2	)	 =	"Some people will briefly be described. Please indicate for each description whether that person is very much like you, like you, somewhat like you, not like you, or not at all like you. - It is important to this person to be rich; to have a lot of money and expensive things"
var_lab(raw_data$q57_3	)	 =	"Some people will briefly be described. Please indicate for each description whether that person is very much like you, like you, somewhat like you, not like you, or not at all like you. - Living in secure surroundings is important to this person; to avoid anything that might be dangerous"
var_lab(raw_data$q57_4	)	 =	"Some people will briefly be described. Please indicate for each description whether that person is very much like you, like you, somewhat like you, not like you, or not at all like you. - It is important to this person to have a good time; to 'spoil' oneself."
var_lab(raw_data$q57_5	)	 =	"Some people will briefly be described. Please indicate for each description whether that person is very much like you, like you, somewhat like you, not like you, or not at all like you. - It is important to this person to do something for the good of society."
var_lab(raw_data$q57_6	)	 =	"Some people will briefly be described. Please indicate for each description whether that person is very much like you, like you, somewhat like you, not like you, or not at all like you. - It is important for this people to help the people nearby; to care for their well-being."
var_lab(raw_data$q57_7	)	 =	"Some people will briefly be described. Please indicate for each description whether that person is very much like you, like you, somewhat like you, not like you, or not at all like you. - Being very successful is important to this person; to have people recognize one's achievements."
var_lab(raw_data$q57_8	)	 =	"Some people will briefly be described. Please indicate for each description whether that person is very much like you, like you, somewhat like you, not like you, or not at all like you. - Adventure and taking risks are important to this person; to have an exciting life."
var_lab(raw_data$q57_9	)	 =	"Some people will briefly be described. Please indicate for each description whether that person is very much like you, like you, somewhat like you, not like you, or not at all like you. - It is important to this person to always behave properly; to avoid doing anything people would say is wrong."
var_lab(raw_data$q57_10	)	 =	"Some people will briefly be described. Please indicate for each description whether that person is very much like you, like you, somewhat like you, not like you, or not at all like you. - Looking after the environment is important to this person; to care for nature and save life resources."
var_lab(raw_data$q57_11	)	 =	"Some people will briefly be described. Please indicate for each description whether that person is very much like you, like you, somewhat like you, not like you, or not at all like you. - Tradition is important to this person; to follow the customs handed down by one's religion or family."
var_lab(raw_data$q59	)	 =	"Here are two statements people sometimes make when discussing the environment and economic growth. Which of them comes closer to your own point of view?"
var_lab(raw_data$q60_1	)	 =	"During the past two years have you. - Given money to an ecological organization?"
var_lab(raw_data$q60_2	)	 =	"During the past two years have you. - Participated in a demonstration for some environmental cause?"
var_lab(raw_data$q62_1	)	 =	"Please look at the following items and select whether you have done any of these things, whether you might do it or would never under any circumstances do it. - Signing a petition"
var_lab(raw_data$q62_2	)	 =	"Please look at the following items and select whether you have done any of these things, whether you might do it or would never under any circumstances do it. - Joining in boycotts"
var_lab(raw_data$q62_3	)	 =	"Please look at the following items and select whether you have done any of these things, whether you might do it or would never under any circumstances do it. - Attending peaceful demonstrations"
var_lab(raw_data$q62_4	)	 =	"Please look at the following items and select whether you have done any of these things, whether you might do it or would never under any circumstances do it. - Joining strikes"
var_lab(raw_data$q62_5	)	 =	"Please look at the following items and select whether you have done any of these things, whether you might do it or would never under any circumstances do it. - Any other act of protest?"
var_lab(raw_data$q65_1	)	 =	"In political matters, people talk of 'the left' and 'the right.' How would you place your views on this scale, generally speaking? - Left:Right"
var_lab(raw_data$q70_1	)	 =	"Please indicate how much you trust people from various groups. Could you tell for each whether you trust people from this group completely, somewhat, not very much or not at all? - People you know personally"
var_lab(raw_data$q70_2	)	 =	"Please indicate how much you trust people from various groups. Could you tell for each whether you trust people from this group completely, somewhat, not very much or not at all? - People you meet for the first time	"
var_lab(raw_data$q73_1	)	 =	"Below is a list of different organizations. For each one, please indicate how much confidence you have in them: is it a great deal of confidence, quite a lot of confidence, not very much confidence or none at all? - Health Sector"
var_lab(raw_data$q73_2	)	 =	"Below is a list of different organizations. For each one, please indicate how much confidence you have in them: is it a great deal of confidence, quite a lot of confidence, not very much confidence or none at all? - Government"
var_lab(raw_data$q73_3	)	 =	"Below is a list of different organizations. For each one, please indicate how much confidence you have in them: is it a great deal of confidence, quite a lot of confidence, not very much confidence or none at all? - Major Companies"
var_lab(raw_data$q73_4	)	 =	"Below is a list of different organizations. For each one, please indicate how much confidence you have in them: is it a great deal of confidence, quite a lot of confidence, not very much confidence or none at all? - Environmental organizations"
var_lab(raw_data$q73_5	)	 =	"Below is a list of different organizations. For each one, please indicate how much confidence you have in them: is it a great deal of confidence, quite a lot of confidence, not very much confidence or none at all? - Our country's institutions as a whole"
var_lab(raw_data$q73_6	)	 =	"Below is a list of different organizations. For each one, please indicate how much confidence you have in them: is it a great deal of confidence, quite a lot of confidence, not very much confidence or none at all? - The European Union"
var_lab(raw_data$q73_7	)	 =	"Below is a list of different organizations. For each one, please indicate how much confidence you have in them: is it a great deal of confidence, quite a lot of confidence, not very much confidence or none at all? - The United Nations"
var_lab(raw_data$q79_1	)	 =	"And how democratically is this country being governed today? Again using a scale from 1 to 10, where 1 means that it is 'not at all democratic' and 10 means that it is 'completely democratic', what position would you choose? - not at all democratic:completely democratic"
var_lab(raw_data$q78	)	 =	"How much respect is there for individual human rights nowadays in this country? Do you feel there is"
var_lab(raw_data$q81	)	 =	"Apart from weddings and funerals, about how often do you attend religious services usually?"
var_lab(raw_data$q83	)	 =	"Independently of whether you attend religious services or not, would you say you are"
var_lab(raw_data$q89_1	)	 =	"How important is God in your life? Please use this scale to indicate. 10 means 'very important' and 1 means 'not at all important.' - not at all important:very important"
var_lab(raw_data$q97_1	)	 =	"I see myself as someone who - is reserved"
var_lab(raw_data$q97_2	)	 =	"I see myself as someone who - is generally trusting"
var_lab(raw_data$q97_3	)	 =	"I see myself as someone who - tends to be lazy"
var_lab(raw_data$q97_4	)	 =	"I see myself as someone who - is relaxed, handles stress well"
var_lab(raw_data$q97_5	)	 =	"I see myself as someone who - has few artistic interests"
var_lab(raw_data$q97_6	)	 =	"I see myself as someone who - is outgoing, sociable"
var_lab(raw_data$q97_7	)	 =	"I see myself as someone who - tends to find fault with others"
var_lab(raw_data$q97_8	)	 =	"I see myself as someone who - does a thorough job"
var_lab(raw_data$q97_9	)	 =	"I see myself as someone who - gets nervous easily"
var_lab(raw_data$q97_10	)	 =	"I see myself as someone who - has an active imagination"
var_lab(raw_data$q101	)	 =	"Do you agree or disagree with the following statement: 'Under some conditions, war is necessary to obtain justice.'"
var_lab(raw_data$q112_1	)	 =	"It is not important for me to know about science in my daily life. - Completely disagree:Completely agree"
var_lab(raw_data$q108_1	)	 =	"Science and technology are making our lives healthier, easier, and more comfortable. - Completely disagree:Completely agree"
var_lab(raw_data$q116_1	)	 =	"People have different views about themselves and how they relate to the world. Would you indicate how strongly you agree or disagree with each of the following statements about how you see yourself? - I see myself as a world citizen"
var_lab(raw_data$q116_2	)	 =	"People have different views about themselves and how they relate to the world. Would you indicate how strongly you agree or disagree with each of the following statements about how you see yourself? - I see myself as part of my local community"
var_lab(raw_data$q116_3	)	 =	"People have different views about themselves and how they relate to the world. Would you indicate how strongly you agree or disagree with each of the following statements about how you see yourself? - I see myself as a part of the German nation"
var_lab(raw_data$q116_4	)	 =	"People have different views about themselves and how they relate to the world. Would you indicate how strongly you agree or disagree with each of the following statements about how you see yourself? - I see myself as part of the European Union"
var_lab(raw_data$q116_5	)	 =	"People have different views about themselves and how they relate to the world. Would you indicate how strongly you agree or disagree with each of the following statements about how you see yourself? - I see myself as an autonomous individual"
var_lab(raw_data$q138_1	)	 =	"In the city or area where you live, are you satisfied or dissatisfied with the quality of the following? - The public transportation systems"
var_lab(raw_data$q138_2	)	 =	"In the city or area where you live, are you satisfied or dissatisfied with the quality of the following? - The roads and highways"
var_lab(raw_data$q138_3	)	 =	"In the city or area where you live, are you satisfied or dissatisfied with the quality of the following? - The schools"
var_lab(raw_data$q138_4	)	 =	"In the city or area where you live, are you satisfied or dissatisfied with the quality of the following? - The quality of air"
var_lab(raw_data$q138_5	)	 =	"In the city or area where you live, are you satisfied or dissatisfied with the quality of the following? - The quality of water"
var_lab(raw_data$q138_6	)	 =	"In the city or area where you live, are you satisfied or dissatisfied with the quality of the following? - The quality of health care"
var_lab(raw_data$q138_7	)	 =	"In the city or area where you live, are you satisfied or dissatisfied with the quality of the following? - The quality of housing"
var_lab(raw_data$q138_8	)	 =	"In the city or area where you live, are you satisfied or dissatisfied with the quality of the following? - The beauty or physical setting"
var_lab(raw_data$random	)	 =	"random"
var_lab(raw_data$group	)	 =	"group"
var_lab(raw_data$q161 - parenttopics	)	 =	"Q161 - Parent Topics"
var_lab(raw_data$q161 - topics	)	 = "Q161 - Topics"

#with var_lab you can now read variable labels and find out, how they are named

var_lab(raw_data$q8)

use_labels(raw_data, barchart(q8))

#var_lab(raw_data) # get variable label

#barchart(raw_data$q164)

#barchart(raw_data$q173_1)

#barchart (raw_data2$q1)
# barplot(dat$Freq, names.arg = dat$pl)

raw_data_nolabels<-raw_data
#Assign value labels to variables

raw_data$q1 <- ordered (raw_data$q1,  
                        levels=c( 1,2,3 ),  
                        labels=c( "male","female","other" ))
raw_data$q3 <- ordered (raw_data$ q3 ,  
                        levels=c( 1,2 ),  
                        labels=c( "I am born in this country", "I am an immigrant to this country" ))
raw_data$q4 <- ordered (raw_data$ q4 ,  
                        levels=c( 1,2,3,4,5,6,7,8 ),  
                        labels=c( "Didn't go to school/didn't graduate from school", "Volks/Hauptschule complete", "Mittlere Reife/Realschule complete", "Abitur/Fachabitur complete", "Technical school", "Bachelor's", "Master's", "Doctorate or higher " ))
raw_data$ q179 <- ordered (raw_data$ q179 ,  
                           levels=c( 1,2,6,7,8,9 ),  
                           labels=c( "Married", "Living together as married", "Divorced", "Seperated", "Widowed", "Single" ))
raw_data$ q180 <- ordered (raw_data$ q180 ,  
                           levels=c( 1,2,3,4,5 ),  
                           labels=c( "No child", "One child", "Two children", "Three children", "Four or more children" ))
raw_data$ q8 <- ordered (raw_data$ q8 ,  
                         levels=c( 8,9,10,11 ),  
                         labels=c( "Government or public institution", "Private business or industry", "Private non-profit organization", "I've never worked" ))
raw_data$ q6 <- ordered (raw_data$ q6 ,  
                         levels=c( 1,2,3,4,5,6,13,14,15,16,17 ),  
                         labels=c( "Less than ???500", "???500- ???1,000", "???1,001 - ???1,500", "???1,501 - ???2,000", "???4,501 - ???5,000", "More than ???5,000", "???2,001 - ???2,500", "???2,501 - ???3,000", "???3,001 - ???3,500", "???3,501 - ???4,000", "???4,001 - ???4,500" ))
raw_data$ q182 <- ordered (raw_data$ q182 ,  
                           levels=c( 1,2 ),  
                           labels=c( "yes","no" ))
raw_data$ q183 <- ordered (raw_data$ q183 ,  
                           levels=c( 1,2 ),  
                           labels=c( "yes","no" ))
raw_data$ q10 <- ordered (raw_data$ q10 ,  
                          levels=c( 1,2,3,4,5 ),  
                          labels=c( "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree" ))
raw_data$ q11 <- ordered (raw_data$ q11 ,  
                          levels=c( 1,2,3,4,5 ),  
                          labels=c( "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree" ))
raw_data$ q13 <- ordered (raw_data$ q13 ,  
                          levels=c( 1,2,3,4,5 ),  
                          labels=c( "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree" ))
raw_data$ q14 <- ordered (raw_data$ q14 ,  
                          levels=c( 1,2,3,4,5 ),  
                          labels=c( "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree" ))
raw_data$ q15 <- ordered (raw_data$ q15 ,  
                          levels=c( 1,2,3,4,5 ),  
                          labels=c( "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree" ))
raw_data$ q16 <- ordered (raw_data$ q16 ,  
                          levels=c( 1,2,3,4,5 ),  
                          labels=c( "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree" ))
raw_data$ q18 <- ordered (raw_data$ q18 ,  
                          levels=c( 1,2,3,4,5 ),  
                          labels=c( "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree" ))
raw_data$ q19 <- ordered (raw_data$ q19 ,  
                          levels=c( 1,2,3,4,5 ),  
                          labels=c( "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree" ))
raw_data$ q20 <- ordered (raw_data$ q20 ,  
                          levels=c( 1,2,3,4,5 ),  
                          labels=c( "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree" ))
raw_data$ q21 <- ordered (raw_data$ q21 ,  
                          levels=c( 1,2,3,4,5 ),  
                          labels=c( "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree" ))
raw_data$ q22 <- ordered (raw_data$ q22 ,  
                          levels=c( 1,2,3,4,5 ),  
                          labels=c( "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree" ))
raw_data$ q23 <- ordered (raw_data$ q23 ,  
                          levels=c( 1,2,3,4,5 ),  
                          labels=c( "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree" ))
raw_data$ q24 <- ordered (raw_data$ q24 ,  
                          levels=c( 1,2,3,4,5 ),  
                          labels=c( "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree" ))
raw_data$ q25 <- ordered (raw_data$ q25 ,  
                          levels=c( 1,2,3,4,5 ),  
                          labels=c( "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree" ))
raw_data$ q26 <- ordered (raw_data$ q26 ,  
                          levels=c( 1,2,3,4,5 ),  
                          labels=c( "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree" ))
raw_data$ q171 <- ordered (raw_data$ q171 ,  
                           levels=c( 1,2,3,4 ),  
                           labels=c( "Definitely changing", "Probably changing", "Probably not changing", "Definitely not changing" ))
raw_data$ q172 <- ordered (raw_data$ q172 ,  
                           levels=c( 1,2,3,4,5,6 ),  
                           labels=c( "Entirely by natural processes", "Mainly by natural processes", "About equally by natural processes and human activity", "Mainly by human activity", "Entirely by human activity", "I don't think climate change is happening" ))
raw_data$ q173 <- ordered (raw_data$ q173 ,  
                           levels=c( 1,2,3,4 ),  
                           labels=c( "Omnivorous", "Vegetarian", "Vegan", "Other" ))
raw_data$ q165 <- ordered (raw_data$ q165 ,  
                           levels=c( 13,14,16,18,19 ),  
                           labels=c( "Very strong", "Strong", "Moderate", "Weak", "Very weak/no reaction" ))
raw_data$ q168_1 <- ordered (raw_data$ q168_1 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Not at all", "Several days", "More than half of the days", "Nearly every day" ))
raw_data$ q168_2 <- ordered (raw_data$ q168_2 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Not at all", "Several days", "More than half of the days", "Nearly every day" ))
raw_data$ q168_3 <- ordered (raw_data$ q168_3 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Not at all", "Several days", "More than half of the days", "Nearly every day" ))
raw_data$ q168_4 <- ordered (raw_data$ q168_4 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Not at all", "Several days", "More than half of the days", "Nearly every day" ))
raw_data$ q168_5 <- ordered (raw_data$ q168_5 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Not at all", "Several days", "More than half of the days", "Nearly every day" ))
raw_data$ q167 <- ordered (raw_data$ q167 ,  
                           levels=c( 1,2,3,4,5,6,7 ),  
                           labels=c( "myself and my family", "my local community", "my state", "my country as a whole", "people all over the world", "non-human nature", "not concerned at all" ))
raw_data$ q170 <- ordered (raw_data$ q170 ,  
                           levels=c( 1,2,3,4,5 ),  
                           labels=c( "Extremely at risk", "Very at risk", "Moderately at risk", "Slightly at risk", "Not at risk at all" ))
raw_data$ q162_1 <- ordered (raw_data$ q162_1 ,  
                             levels=c( 1 ),  
                             labels=c( "Health (physical and/or mental)" ))
raw_data$ q162_2 <- ordered (raw_data$ q162_2 ,  
                             levels=c( 1 ),  
                             labels=c( "Lifestyle" ))
raw_data$ q162_5 <- ordered (raw_data$ q162_5 ,  
                             levels=c( 1 ),  
                             labels=c( "Prosperity" ))
raw_data$ q162_6 <- ordered (raw_data$ q162_6 ,  
                             levels=c( 1 ),  
                             labels=c( "Job/Professional life" ))
raw_data$ q162_3 <- ordered (raw_data$ q162_3 ,  
                             levels=c( 1 ),  
                             labels=c( "Family or other personal relationships" ))
raw_data$ q162_7 <- ordered (raw_data$ q162_7 ,  
                             levels=c( 1 ),  
                             labels=c( "Other" ))
raw_data$ q162_9 <- ordered (raw_data$ q162_9 ,  
                             levels=c( 1 ),  
                             labels=c( "None" ))
raw_data$ q163 <- ordered (raw_data$ q163 ,  
                           levels=c( 1,2,3,5,6,7,9 ),  
                           labels=c( "Health (physical and/or mental)", "Lifestyle", "Family or other personal relationships", "Prosperity", "Job/Professional life", "Other", "None" ))
raw_data$ q169_1 <- ordered (raw_data$ q169_1 ,  
                             levels=c( 1,2 ),  
                             labels=c( "YES, happened to me", "NO, did not happen to me" ))
raw_data$ q169_2 <- ordered (raw_data$ q169_2 ,  
                             levels=c( 1,2 ),  
                             labels=c( "YES, happened to me", "NO, did not happen to me" ))
raw_data$ q169_3 <- ordered (raw_data$ q169_3 ,  
                             levels=c( 1,2 ),  
                             labels=c( "YES, happened to me", "NO, did not happen to me" ))
raw_data$ q169_4 <- ordered (raw_data$ q169_4 ,  
                             levels=c( 1,2 ),  
                             labels=c( "YES, happened to me", "NO, did not happen to me" ))
raw_data$ q169_5 <- ordered (raw_data$ q169_5 ,  
                             levels=c( 1,2 ),  
                             labels=c( "YES, happened to me", "NO, did not happen to me" ))
raw_data$ q169_6 <- ordered (raw_data$ q169_6 ,  
                             levels=c( 1,2 ),  
                             labels=c( "YES, happened to me", "NO, did not happen to me" ))
raw_data$ q170_1 <- ordered (raw_data$ q170_1 ,  
                             levels=c( 1,2 ),  
                             labels=c( "YES, happened to me", "NO, did not happen to me" ))
raw_data$ q170_2 <- ordered (raw_data$ q170_2 ,  
                             levels=c( 1,2 ),  
                             labels=c( "YES, happened to me", "NO, did not happen to me" ))
raw_data$ q170_3 <- ordered (raw_data$ q170_3 ,  
                             levels=c( 1,2 ),  
                             labels=c( "YES, happened to me", "NO, did not happen to me" ))
raw_data$ q170_4 <- ordered (raw_data$ q170_4 ,  
                             levels=c( 1,2 ),  
                             labels=c( "YES, happened to me", "NO, did not happen to me" ))
raw_data$ q170_5 <- ordered (raw_data$ q170_5 ,  
                             levels=c( 1,2 ),  
                             labels=c( "YES, happened to me", "NO, did not happen to me" ))
raw_data$ q170_6 <- ordered (raw_data$ q170_6 ,  
                             levels=c( 1,2 ),  
                             labels=c( "YES, happened to me", "NO, did not happen to me" ))
raw_data$ q170_7 <- ordered (raw_data$ q170_7 ,  
                             levels=c( 1,2 ),  
                             labels=c( "YES, happened to me", "NO, did not happen to me" ))
raw_data$ q171.0 <- ordered (raw_data$ q171_1 ,  
                             levels=c( 1,2 ),  
                             labels=c( "Yes, I do believe in these stories.", "No, I don't believe in these stories." ))
raw_data$ q173.0 <- ordered (raw_data$ q173_1 ,  
                             levels=c( 1,2,3,4,5 ),  
                             labels=c( "Social media are most credible", "Social media are slightly more credible than traditional media", "Both the same", "Traditional media are slightly more credible than social media", "Traditional media are most credible" ))
raw_data$ q164.0 <- ordered (raw_data$ q164_1 ,  
                             levels=c( 1,2,3,5,6 ),  
                             labels=c( much too short", "slightly too short", "adequate", "slightly too long", "much too long", " ))
raw_data$ q165.0 <- ordered (raw_data$ q165_1 ,  
                             levels=c( 1,2,3,4,5 ),  
                             labels=c( "very good", "rather good", "neutral", "rather bad", "very bad" ))
raw_data$ q38_1 <- ordered (raw_data$ q38_1 ,  
                            levels=c( 1 ),  
                            labels=c( "Independence" ))
raw_data$ q38_2 <- ordered (raw_data$ q38_2 ,  
                            levels=c( 1 ),  
                            labels=c( "Hard work" ))
raw_data$ q38_3 <- ordered (raw_data$ q38_3 ,  
                            levels=c( 1 ),  
                            labels=c( "Feeling of responsibility" ))
raw_data$ q38_4 <- ordered (raw_data$ q38_4 ,  
                            levels=c( 1 ),  
                            labels=c( "Imagination" ))
raw_data$ q38_5 <- ordered (raw_data$ q38_5 ,  
                            levels=c( 1 ),  
                            labels=c( "Tolerance and respect for other people" ))
raw_data$ q38_6 <- ordered (raw_data$ q38_6 ,  
                            levels=c( 1 ),  
                            labels=c( "Thrift, saving money and things" ))
raw_data$ q38_7 <- ordered (raw_data$ q38_7 ,  
                            levels=c( 1 ),  
                            labels=c( "Determination, perseverance" ))
raw_data$ q38_8 <- ordered (raw_data$ q38_8 ,  
                            levels=c( 1 ),  
                            labels=c( "Religious faith" ))
raw_data$ q38_9 <- ordered (raw_data$ q38_9 ,  
                            levels=c( 1 ),  
                            labels=c( "Unselfishness" ))
raw_data$ q38_10 <- ordered (raw_data$ q38_10 ,  
                             levels=c( 1 ),  
                             labels=c( "Obedience" ))
raw_data$ q38_11 <- ordered (raw_data$ q38_11 ,  
                             levels=c( 1 ),  
                             labels=c( "Self-expression" ))
raw_data$ q40 <- ordered (raw_data$ q40 ,  
                          levels=c( 1,2 ),  
                          labels=c( "Most people can be trusted.", "Need to be very careful." ))
raw_data$ q44_1 <- ordered (raw_data$ q44_1 ,  
                            levels=c( 1,2,3 ),  
                            labels=c( "Agree", "Neither", "Disagree" ))
raw_data$ q44_2 <- ordered (raw_data$ q44_2 ,  
                            levels=c( 1,2,3 ),  
                            labels=c( "Agree", "Neither", "Disagree" ))
raw_data$ q44_3 <- ordered (raw_data$ q44_3 ,  
                            levels=c( 1,2,3 ),  
                            labels=c( "Agree", "Neither", "Disagree" ))
raw_data$ q44_4 <- ordered (raw_data$ q44_4 ,  
                            levels=c( 1,2,3 ),  
                            labels=c( "Agree", "Neither", "Disagree" ))
raw_data$ q45_1 <- ordered (raw_data$ q45_1 ,  
                            levels=c( 1,2,3,4 ),  
                            labels=c( "Strongly agree", "Agree", "Disagree", "Strongly disagree" ))
raw_data$ q45_2 <- ordered (raw_data$ q45_2 ,  
                            levels=c( 1,2,3,4 ),  
                            labels=c( "Strongly agree", "Agree", "Disagree", "Strongly disagree" ))
raw_data$ q45_3 <- ordered (raw_data$ q45_3 ,  
                            levels=c( 1,2,3,4 ),  
                            labels=c( "Strongly agree", "Agree", "Disagree", "Strongly disagree" ))
raw_data$ q45_4 <- ordered (raw_data$ q45_4 ,  
                            levels=c( 1,2,3,4 ),  
                            labels=c( "Strongly agree", "Agree", "Disagree", "Strongly disagree" ))
raw_data$ q45_5 <- ordered (raw_data$ q45_5 ,  
                            levels=c( 1,2,3,4 ),  
                            labels=c( "Strongly agree", "Agree", "Disagree", "Strongly disagree" ))
raw_data$ q45_6 <- ordered (raw_data$ q45_6 ,  
                            levels=c( 1,2,3,4 ),  
                            labels=c( "Strongly agree", "Agree", "Disagree", "Strongly disagree" ))
raw_data$ q50_1 <- ordered (raw_data$ q50_1 ,  
                            levels=c( 1,2 ),  
                            labels=c( "Most Important", "Second Most important" ))
raw_data$ q50_2 <- ordered (raw_data$ q50_2 ,  
                            levels=c( 1,2 ),  
                            labels=c( "Most Important", "Second Most important" ))
raw_data$ q50_3 <- ordered (raw_data$ q50_3 ,  
                            levels=c( 1,2 ),  
                            labels=c( "Most Important", "Second Most important" ))
raw_data$ q50_4 <- ordered (raw_data$ q50_4 ,  
                            levels=c( 1,2 ),  
                            labels=c( "Most Important", "Second Most important" ))
raw_data$ q55_1 <- ordered (raw_data$ q55_1 ,  
                            levels=c( 1,2 ),  
                            labels=c( "Most Important", "Second Most important" ))
raw_data$ q55_2 <- ordered (raw_data$ q55_2 ,  
                            levels=c( 1,2 ),  
                            labels=c( "Most Important", "Second Most important" ))
raw_data$ q55_3 <- ordered (raw_data$ q55_3 ,  
                            levels=c( 1,2 ),  
                            labels=c( "Most Important", "Second Most important" ))
raw_data$ q55_4 <- ordered (raw_data$ q55_4 ,  
                            levels=c( 1,2 ),  
                            labels=c( "Most Important", "Second Most important" ))
raw_data$ q53_1 <- ordered (raw_data$ q53_1 ,  
                            levels=c( 1,2 ),  
                            labels=c( "Most Important", "Second Most important" ))
raw_data$ q53_2 <- ordered (raw_data$ q53_2 ,  
                            levels=c( 1,2 ),  
                            labels=c( "Most Important", "Second Most important" ))
raw_data$ q53_3 <- ordered (raw_data$ q53_3 ,  
                            levels=c( 1,2 ),  
                            labels=c( "Most Important", "Second Most important" ))
raw_data$ q53_4 <- ordered (raw_data$ q53_4 ,  
                            levels=c( 1,2 ),  
                            labels=c( "Most Important", "Second Most important" ))
raw_data$ q56_1 <- ordered (raw_data$ q56_1 ,  
                            levels=c( 1,2,3 ),  
                            labels=c( "Good", "Don't mind", "Bad" ))
raw_data$ q56_2 <- ordered (raw_data$ q56_2 ,  
                            levels=c( 1,2,3 ),  
                            labels=c( "Good", "Don't mind", "Bad" ))
raw_data$ q56_3 <- ordered (raw_data$ q56_3 ,  
                            levels=c( 1,2,3 ),  
                            labels=c( "Good", "Don't mind", "Bad" ))
raw_data$ q57_1 <- ordered (raw_data$ q57_1 ,  
                            levels=c( 1,2,3,4,5,6 ),  
                            labels=c( "Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not at all like me" ))
raw_data$ q57_2 <- ordered (raw_data$ q57_2 ,  
                            levels=c( 1,2,3,4,5,6 ),  
                            labels=c( "Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not at all like me" ))
raw_data$ q57_3 <- ordered (raw_data$ q57_3 ,  
                            levels=c( 1,2,3,4,5,6 ),  
                            labels=c( "Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not at all like me" ))
raw_data$ q57_4 <- ordered (raw_data$ q57_4 ,  
                            levels=c( 1,2,3,4,5,6 ),  
                            labels=c( "Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not at all like me" ))
raw_data$ q57_5 <- ordered (raw_data$ q57_5 ,  
                            levels=c( 1,2,3,4,5,6 ),  
                            labels=c( "Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not at all like me" ))
raw_data$ q57_6 <- ordered (raw_data$ q57_6 ,  
                            levels=c( 1,2,3,4,5,6 ),  
                            labels=c( "Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not at all like me" ))
raw_data$ q57_7 <- ordered (raw_data$ q57_7 ,  
                            levels=c( 1,2,3,4,5,6 ),  
                            labels=c( "Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not at all like me" ))
raw_data$ q57_8 <- ordered (raw_data$ q57_8 ,  
                            levels=c( 1,2,3,4,5,6 ),  
                            labels=c( "Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not at all like me" ))
raw_data$ q57_9 <- ordered (raw_data$ q57_9 ,  
                            levels=c( 1,2,3,4,5,6 ),  
                            labels=c( "Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not at all like me" ))
raw_data$ q57_10 <- ordered (raw_data$ q57_10 ,  
                             levels=c( 1,2,3,4,5,6 ),  
                             labels=c( "Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not at all like me" ))
raw_data$ q57_11 <- ordered (raw_data$ q57_11 ,  
                             levels=c( 1,2,3,4,5,6 ),  
                             labels=c( "Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not at all like me" ))
raw_data$ q59 <- ordered (raw_data$ q59 ,  
                          levels=c( 1,2 ),  
                          labels=c( "Protecting the environment should be given priority, even if it causes slower economic growth and some loss of jobs.",  "Economic growth and creating jobs should be the top priority, even if the environment suffers to some extent." ))
raw_data$ q60_1 <- ordered (raw_data$ q60_1 ,  
                            levels=c( 1,2 ),  
                            labels=c( "Yes","No" ))
raw_data$ q60_2 <- ordered (raw_data$ q60_2 ,  
                            levels=c( 1,2 ),  
                            labels=c( "Yes","No" ))
raw_data$ q62_1 <- ordered (raw_data$ q62_1 ,  
                            levels=c( 1,2,3 ),  
                            labels=c( "Have done", "Might do", "Would never do" ))
raw_data$ q62_2 <- ordered (raw_data$ q62_2 ,  
                            levels=c( 1,2,3 ),  
                            labels=c( "Have done", "Might do", "Would never do" ))
raw_data$ q62_3 <- ordered (raw_data$ q62_3 ,  
                            levels=c( 1,2,3 ),  
                            labels=c( "Have done", "Might do", "Would never do" ))
raw_data$ q62_4 <- ordered (raw_data$ q62_4 ,  
                            levels=c( 1,2,3 ),  
                            labels=c( "Have done", "Might do", "Would never do" ))
raw_data$ q62_5 <- ordered (raw_data$ q62_5 ,  
                            levels=c( 1,2,3 ),  
                            labels=c( "Have done", "Might do", "Would never do" ))
raw_data$ q70_1 <- ordered (raw_data$ q70_1 ,  
                            levels=c( 1,2,3,4 ),  
                            labels=c( "Trust completely", "Trust somewhat", "Do not trust very much", "Do not trust at all" ))
raw_data$ q70_2 <- ordered (raw_data$ q70_2 ,  
                            levels=c( 1,2,3,4 ),  
                            labels=c( "Trust completely", "Trust somewhat", "Do not trust very much", "Do not trust at all" ))
raw_data$ q73_1 <- ordered (raw_data$ q73_1 ,  
                            levels=c( 1,2,3,4 ),  
                            labels=c( "Trust completely", "Trust somewhat", "Do not trust very much", "Do not trust at all" ))
raw_data$ q73_2 <- ordered (raw_data$ q73_2 ,  
                            levels=c( 1,2,3,4 ),  
                            labels=c( "Trust completely", "Trust somewhat", "Do not trust very much", "Do not trust at all" ))
raw_data$ q73_3 <- ordered (raw_data$ q73_3 ,  
                            levels=c( 1,2,3,4 ),  
                            labels=c( "Trust completely", "Trust somewhat", "Do not trust very much", "Do not trust at all" ))
raw_data$ q73_4 <- ordered (raw_data$ q73_4 ,  
                            levels=c( 1,2,3,4 ),  
                            labels=c( "Trust completely", "Trust somewhat", "Do not trust very much", "Do not trust at all" ))
raw_data$ q73_5 <- ordered (raw_data$ q73_5 ,  
                            levels=c( 1,2,3,4 ),  
                            labels=c( "Trust completely", "Trust somewhat", "Do not trust very much", "Do not trust at all" ))
raw_data$ q73_6 <- ordered (raw_data$ q73_6 ,  
                            levels=c( 1,2,3,4 ),  
                            labels=c( "Trust completely", "Trust somewhat", "Do not trust very much", "Do not trust at all" ))
raw_data$ q73_7 <- ordered (raw_data$ q73_7 ,  
                            levels=c( 1,2,3,4 ),  
                            labels=c( "Trust completely", "Trust somewhat", "Do not trust very much", "Do not trust at all" ))
raw_data$ q78 <- ordered (raw_data$ q78 ,  
                          levels=c( 1,2,3,4 ),  
                          labels=c( "A lot", "Some", "Little", "None at all" ))
raw_data$ q81 <- ordered (raw_data$ q81 ,  
                          levels=c( 1,2,3,4,5,6,7 ),  
                          labels=c( "More than once a week", "Once a week", "Once a month", "Only on special holy days", "Once a year", "Less often" ))
raw_data$ q83 <- ordered (raw_data$ q83 ,  
                          levels=c( 1,2,3 ),  
                          labels=c( "A religious person", "Not a religious person", "An atheist" ))
raw_data$ q97_1 <- ordered (raw_data$ q97_1 ,  
                            levels=c( 1,2,3,4,5 ),  
                            labels=c( "Disagree strongly", "Disagree a little", "Neither agree nor disagree", "Agree a little", "Agree strongly" ))
raw_data$ q97_2 <- ordered (raw_data$ q97_2 ,  
                            levels=c( 1,2,3,4,5 ),  
                            labels=c( "Disagree strongly", "Disagree a little", "Neither agree nor disagree", "Agree a little", "Agree strongly" ))
raw_data$ q97_3 <- ordered (raw_data$ q97_3 ,  
                            levels=c( 1,2,3,4,5 ),  
                            labels=c( "Disagree strongly", "Disagree a little", "Neither agree nor disagree", "Agree a little", "Agree strongly" ))
raw_data$ q97_4 <- ordered (raw_data$ q97_4 ,  
                            levels=c( 1,2,3,4,5 ),  
                            labels=c( "Disagree strongly", "Disagree a little", "Neither agree nor disagree", "Agree a little", "Agree strongly" ))
raw_data$ q97_5 <- ordered (raw_data$ q97_5 ,  
                            levels=c( 1,2,3,4,5 ),  
                            labels=c( "Disagree strongly", "Disagree a little", "Neither agree nor disagree", "Agree a little", "Agree strongly" ))
raw_data$ q97_6 <- ordered (raw_data$ q97_6 ,  
                            levels=c( 1,2,3,4,5 ),  
                            labels=c( "Disagree strongly", "Disagree a little", "Neither agree nor disagree", "Agree a little", "Agree strongly" ))
raw_data$ q97_7 <- ordered (raw_data$ q97_7 ,  
                            levels=c( 1,2,3,4,5 ),  
                            labels=c( "Disagree strongly", "Disagree a little", "Neither agree nor disagree", "Agree a little", "Agree strongly" ))
raw_data$ q97_8 <- ordered (raw_data$ q97_8 ,  
                            levels=c( 1,2,3,4,5 ),  
                            labels=c( "Disagree strongly", "Disagree a little", "Neither agree nor disagree", "Agree a little", "Agree strongly" ))
raw_data$ q97_9 <- ordered (raw_data$ q97_9 ,  
                            levels=c( 1,2,3,4,5 ),  
                            labels=c( "Disagree strongly", "Disagree a little", "Neither agree nor disagree", "Agree a little", "Agree strongly" ))
raw_data$ q97_10 <- ordered (raw_data$ q97_10 ,  
                             levels=c( 1,2,3,4,5 ),  
                             labels=c( "Disagree strongly", "Disagree a little", "Neither agree nor disagree", "Agree a little", "Agree strongly" ))
raw_data$ q101 <- ordered (raw_data$ q101 ,  
                           levels=c( 1,2 ),  
                           labels=c( "Agree","Disagree" ))
raw_data$ q116_1 <- ordered (raw_data$ q116_1 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Strongly agree", "Agree", "Disagree", "Strongly disagree" ))
raw_data$ q116_2 <- ordered (raw_data$ q116_2 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Strongly agree", "Agree", "Disagree", "Strongly disagree" ))
raw_data$ q116_3 <- ordered (raw_data$ q116_3 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Strongly agree", "Agree", "Disagree", "Strongly disagree" ))
raw_data$ q116_4 <- ordered (raw_data$ q116_4 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Strongly agree", "Agree", "Disagree", "Strongly disagree" ))
raw_data$ q116_5 <- ordered (raw_data$ q116_5 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Strongly agree", "Agree", "Disagree", "Strongly disagree" ))
raw_data$ q138_1 <- ordered (raw_data$ q138_1 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Very satisfied", "Fairly satisfied", "Fairly dissatisfied", "Very dissatisfied" ))
raw_data$ q138_2 <- ordered (raw_data$ q138_2 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Very satisfied", "Fairly satisfied", "Fairly dissatisfied", "Very dissatisfied" ))
raw_data$ q138_3 <- ordered (raw_data$ q138_3 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Very satisfied", "Fairly satisfied", "Fairly dissatisfied", "Very dissatisfied" ))
raw_data$ q138_4 <- ordered (raw_data$ q138_4 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Very satisfied", "Fairly satisfied", "Fairly dissatisfied", "Very dissatisfied" ))
raw_data$ q138_5 <- ordered (raw_data$ q138_5 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Very satisfied", "Fairly satisfied", "Fairly dissatisfied", "Very dissatisfied" ))
raw_data$ q138_6 <- ordered (raw_data$ q138_6 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Very satisfied", "Fairly satisfied", "Fairly dissatisfied", "Very dissatisfied" ))
raw_data$ q138_7 <- ordered (raw_data$ q138_7 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Very satisfied", "Fairly satisfied", "Fairly dissatisfied", "Very dissatisfied" ))
raw_data$ q138_8 <- ordered (raw_data$ q138_8 ,  
                             levels=c( 1,2,3,4 ),  
                             labels=c( "Very satisfied", "Fairly satisfied", "Fairly dissatisfied", "Very dissatisfied" ))



#Assign new names to variables because the old ones are not really intuitive

raw_data_nonames<-raw_data

colnames(raw_data)[which(names(raw_data)=="q160")]<-"dsgvo_w1lg"
colnames(raw_data)[which(names(raw_data)=="q161")]<-"codeword_w1lg"
colnames(raw_data)[which(names(raw_data)=="q1")]<-"gender_w1lg"
colnames(raw_data)[which(names(raw_data)=="q2_10")]<-"age_w1lg"
colnames(raw_data)[which(names(raw_data)=="q3")]<-"born_w1lg"
colnames(raw_data)[which(names(raw_data)=="q4")]<-"education_w1lg"
colnames(raw_data)[which(names(raw_data)=="q179")]<-"marstatus_w1lg"
colnames(raw_data)[which(names(raw_data)=="q180")]<-"children_w1lg"
colnames(raw_data)[which(names(raw_data)=="q8")]<-"employer_w1lg"
colnames(raw_data)[which(names(raw_data)=="q144")]<-"fieldofwork_w1lg"
colnames(raw_data)[which(names(raw_data)=="q6")]<-"income_w1lg"
colnames(raw_data)[which(names(raw_data)=="q181")]<-"householdsize_w1lg"
colnames(raw_data)[which(names(raw_data)=="q182")]<-"chiefwage_w1lg"
colnames(raw_data)[which(names(raw_data)=="q183")]<-"chiefwage_emp_w1lg"
colnames(raw_data)[which(names(raw_data)=="q7")]<-"district_w1lg"
colnames(raw_data)[which(names(raw_data)=="q10")]<-"nep1_w1lg"
colnames(raw_data)[which(names(raw_data)=="q11")]<-"nep2_w1lg"
colnames(raw_data)[which(names(raw_data)=="q13")]<-"nep3_w1lg"
colnames(raw_data)[which(names(raw_data)=="q14")]<-"nep4_w1lg"
colnames(raw_data)[which(names(raw_data)=="q15")]<-"nep5_w1lg"
colnames(raw_data)[which(names(raw_data)=="q16")]<-"nep6_w1lg"
colnames(raw_data)[which(names(raw_data)=="q18")]<-"nep7_w1lg"
colnames(raw_data)[which(names(raw_data)=="q19")]<-"nep8_w1lg"
colnames(raw_data)[which(names(raw_data)=="q20")]<-"nep9_w1lg"
colnames(raw_data)[which(names(raw_data)=="q21")]<-"nep10_w1lg"
colnames(raw_data)[which(names(raw_data)=="q22")]<-"nep11_w1lg"
colnames(raw_data)[which(names(raw_data)=="q23")]<-"nep12_w1lg"
colnames(raw_data)[which(names(raw_data)=="q24")]<-"nep14_w1lg"
colnames(raw_data)[which(names(raw_data)=="q25")]<-"nep15_w1lg"
colnames(raw_data)[which(names(raw_data)=="q26")]<-"nep16_w1lg"
colnames(raw_data)[which(names(raw_data)=="q171")]<-"cli_w1lg"
colnames(raw_data)[which(names(raw_data)=="q172")]<-"cliant_w1lg"
colnames(raw_data)[which(names(raw_data)=="q27_1")]<-"freechoice_w1lg"
colnames(raw_data)[which(names(raw_data)=="q27_2")]<-"snelgar1_w1lg"
colnames(raw_data)[which(names(raw_data)=="q27_3")]<-"snelgar2_w1lg"
colnames(raw_data)[which(names(raw_data)=="q27_4")]<-"snelgar3_w1lg"
colnames(raw_data)[which(names(raw_data)=="q27_5")]<-"snelgar4_w1lg"
colnames(raw_data)[which(names(raw_data)=="q27_6")]<-"snelgar5_w1lg"
colnames(raw_data)[which(names(raw_data)=="q27_7")]<-"snelgar6_w1lg"
colnames(raw_data)[which(names(raw_data)=="q27_8")]<-"snelgar7_w1lg"
colnames(raw_data)[which(names(raw_data)=="q27_9")]<-"snelgar8_w1lg"
colnames(raw_data)[which(names(raw_data)=="q173")]<-"diet_w1lg"
colnames(raw_data)[which(names(raw_data)=="q173_4_text")]<-"diet_other_w1lg"
colnames(raw_data)[which(names(raw_data)=="q164")]<-"crisismage_w1lg"
colnames(raw_data)[which(names(raw_data)=="q165")]<-"crisismageemo_w1lg"
colnames(raw_data)[which(names(raw_data)=="q168_1")]<-"crisisperception1_w1lg"
colnames(raw_data)[which(names(raw_data)=="q168_2")]<-"crisisperception2_w1lg"
colnames(raw_data)[which(names(raw_data)=="q168_3")]<-"crisisperception3_w1lg"
colnames(raw_data)[which(names(raw_data)=="q168_4")]<-"crisisperception4_w1lg"
colnames(raw_data)[which(names(raw_data)=="q168_5")]<-"crisisperception5_w1lg"
colnames(raw_data)[which(names(raw_data)=="q167")]<-"whoworried_w1lg"
colnames(raw_data)[which(names(raw_data)=="q170")]<-"currentrisk_w1lg"
colnames(raw_data)[which(names(raw_data)=="q162")]<-"areasrisk_w1lg"
colnames(raw_data)[which(names(raw_data)=="q162_7_text")]<-"areasrisk_other_w1lg"
colnames(raw_data)[which(names(raw_data)=="q163")]<-"areasriskmost_w1lg"
colnames(raw_data)[which(names(raw_data)=="q163_7_text")]<-"areasriskmost_other_w1lg"
colnames(raw_data)[which(names(raw_data)=="q169_1")]<-"cov19pos_w1lg"
colnames(raw_data)[which(names(raw_data)=="q169_2")]<-"cov19neg_w1lg"
colnames(raw_data)[which(names(raw_data)=="q169_3")]<-"cov19mildsymp_w1lg"
colnames(raw_data)[which(names(raw_data)=="q169_4")]<-"cov19sevsymp_w1lg"
colnames(raw_data)[which(names(raw_data)=="q169_5")]<-"cov19peoplemildsymp_w1lg"
colnames(raw_data)[which(names(raw_data)=="q169_6")]<-"cov19peoplesevsymp_w1lg"
colnames(raw_data)[which(names(raw_data)=="q170_1")]<-"cov19lostjob_w1lg"
colnames(raw_data)[which(names(raw_data)=="q170_2")]<-"cov19closebu_w1lg"
colnames(raw_data)[which(names(raw_data)=="q170_3")]<-"cov19parttime_w1lg"
colnames(raw_data)[which(names(raw_data)=="q170_4")]<-"cov19homeoffice_w1lg"
colnames(raw_data)[which(names(raw_data)=="q170_5")]<-"cov19aidpackage_w1lg"
colnames(raw_data)[which(names(raw_data)=="q170_6")]<-"cov19normaljob_w1lg"
colnames(raw_data)[which(names(raw_data)=="q170_7")]<-"cov19daycare_w1lg"
colnames(raw_data)[which(names(raw_data)=="q171")]<-"hoaxstories_w1lg"
colnames(raw_data)[which(names(raw_data)=="q173")]<-"smvsmedia_w1lg"
colnames(raw_data)[which(names(raw_data)=="q164")]<-"lengthofsurvey_w1lg"
colnames(raw_data)[which(names(raw_data)=="q165")]<-"designofsurvey_w1lg"
colnames(raw_data)[which(names(raw_data)=="q166")]<-"commentssurvey_w1lg"
colnames(raw_data)[which(names(raw_data)=="q184_1")]<-"sathealth_w1lg"
colnames(raw_data)[which(names(raw_data)=="q184_2")]<-"satfinance_w1lg"
colnames(raw_data)[which(names(raw_data)=="q184_3")]<-"satsocial_w1lg"
colnames(raw_data)[which(names(raw_data)=="q184_4")]<-"satbalance_w1lg"
colnames(raw_data)[which(names(raw_data)=="q184_5")]<-"satlife_w1lg"
colnames(raw_data)[which(names(raw_data)=="q38")]<-"qualitychild_w1lg"
colnames(raw_data)[which(names(raw_data)=="q40")]<-"trustpeople_w1lg"
colnames(raw_data)[which(names(raw_data)=="q44_1")]<-"jobsformen_w1lg"
colnames(raw_data)[which(names(raw_data)=="q44_2")]<-"jobsfornationals_w1lg"
colnames(raw_data)[which(names(raw_data)=="q44_3")]<-"richwomanproblem_w1lg"
colnames(raw_data)[which(names(raw_data)=="q44_4")]<-"womanjobindependent_w1lg"
colnames(raw_data)[which(names(raw_data)=="q45_1")]<-"lifegoalparents_w1lg"
colnames(raw_data)[which(names(raw_data)=="q45_2")]<-"momworkingbad_w1lg"
colnames(raw_data)[which(names(raw_data)=="q45_3")]<-"menbetterpoliticians_w1lg"
colnames(raw_data)[which(names(raw_data)=="q45_4")]<-"uniformen_w1lg"
colnames(raw_data)[which(names(raw_data)=="q45_5")]<-"menbetterbusiness_w1lg"
colnames(raw_data)[which(names(raw_data)=="q45_6")]<-"happyhousewife_w1lg"
colnames(raw_data)[which(names(raw_data)=="q46_1")]<-"nochoicevschoice_w1lg"
colnames(raw_data)[which(names(raw_data)=="q47_1")]<-"unfairvsfair_w1lg"
colnames(raw_data)[which(names(raw_data)=="q50_1")]<-"countryaimgrowth_w1lg"
colnames(raw_data)[which(names(raw_data)=="q50_2")]<-"countryaimmilitary_w1lg"
colnames(raw_data)[which(names(raw_data)=="q50_3")]<-"countryaimparticipation_w1lg"
colnames(raw_data)[which(names(raw_data)=="q50_4")]<-"countryaimbeauty_w1lg"
colnames(raw_data)[which(names(raw_data)=="q55_1")]<-"impoeconomy_w1lg"
colnames(raw_data)[which(names(raw_data)=="q55_2")]<-"impojustsociety_w1lg"
colnames(raw_data)[which(names(raw_data)=="q55_3")]<-"impoideasociety_w1lg"
colnames(raw_data)[which(names(raw_data)=="q55_4")]<-"imponocrime_w1lg"
colnames(raw_data)[which(names(raw_data)=="q53_1")]<-"impolaworder_w1lg"
colnames(raw_data)[which(names(raw_data)=="q53_2")]<-"impomoresay_w1lg"
colnames(raw_data)[which(names(raw_data)=="q53_3")]<-"impolowprices_w1lg"
colnames(raw_data)[which(names(raw_data)=="q53_4")]<-"impofreespeech_w1lg"
colnames(raw_data)[which(names(raw_data)=="q181_1")]<-"countryegovsaltru_w1lg"
colnames(raw_data)[which(names(raw_data)=="q56_1")]<-"lessworkgoodbad_w1lg"
colnames(raw_data)[which(names(raw_data)=="q56_2")]<-"moretechgoodbad_w1lg"
colnames(raw_data)[which(names(raw_data)=="q56_3")]<-"moreauthoritygoodbad_w1lg"
colnames(raw_data)[which(names(raw_data)=="q57_1")]<-"pvqselfdirection_w1lg"
colnames(raw_data)[which(names(raw_data)=="q57_2")]<-"pvqpower_w1lg"
colnames(raw_data)[which(names(raw_data)=="q57_3")]<-"pvqsecurity_w1lg"
colnames(raw_data)[which(names(raw_data)=="q57_4")]<-"pvqhedonism_w1lg"
colnames(raw_data)[which(names(raw_data)=="q57_5")]<-"pvqbenevolence_w1lg"
colnames(raw_data)[which(names(raw_data)=="q57_6")]<-"pvqbenevolence_w1lg"
colnames(raw_data)[which(names(raw_data)=="q57_7")]<-"pvqachievement_w1lg"
colnames(raw_data)[which(names(raw_data)=="q57_8")]<-"pvqstimulation_w1lg"
colnames(raw_data)[which(names(raw_data)=="q57_9")]<-"pvqconformity_w1lg"
colnames(raw_data)[which(names(raw_data)=="q57_10")]<-"pvquniversalism_w1lg"
colnames(raw_data)[which(names(raw_data)=="q57_11")]<-"pvqtradition_w1lg"
colnames(raw_data)[which(names(raw_data)=="q59")]<-"ecovsgrowth_w1lg"
colnames(raw_data)[which(names(raw_data)=="q60_1")]<-"moneytoeco_w1lg"
colnames(raw_data)[which(names(raw_data)=="q60_2")]<-"demoforeco_w1lg"
colnames(raw_data)[which(names(raw_data)=="q62_1")]<-"activismpetition_w1lg"
colnames(raw_data)[which(names(raw_data)=="q62_2")]<-"activismboycott_w1lg"
colnames(raw_data)[which(names(raw_data)=="q62_3")]<-"activismdemo_w1lg"
colnames(raw_data)[which(names(raw_data)=="q62_4")]<-"activismstrike_w1lg"
colnames(raw_data)[which(names(raw_data)=="q62_5")]<-"activismotherprotest_w1lg"
colnames(raw_data)[which(names(raw_data)=="q65_1")]<-"leftright_w1lg"
colnames(raw_data)[which(names(raw_data)=="q70_1")]<-"trustknownpeople_w1lg"
colnames(raw_data)[which(names(raw_data)=="q70_2")]<-"trustmetpeople_w1lg"
colnames(raw_data)[which(names(raw_data)=="q73_1")]<-"confhealthsector_w1lg"
colnames(raw_data)[which(names(raw_data)=="q73_2")]<-"confgovernment_w1lg"
colnames(raw_data)[which(names(raw_data)=="q73_3")]<-"confcompanies_w1lg"
colnames(raw_data)[which(names(raw_data)=="q73_4")]<-"confecoorga_w1lg"
colnames(raw_data)[which(names(raw_data)=="q73_5")]<-"confcountry_w1lg"
colnames(raw_data)[which(names(raw_data)=="q73_6")]<-"confeu_w1lg"
colnames(raw_data)[which(names(raw_data)=="q73_7")]<-"confun_w1lg"
colnames(raw_data)[which(names(raw_data)=="q79_1")]<-"undemocraticvsdemocratic_w1lg"
colnames(raw_data)[which(names(raw_data)=="q78")]<-"respecthumanrights_w1lg"
colnames(raw_data)[which(names(raw_data)=="q81")]<-"religiousevents_w1lg"
colnames(raw_data)[which(names(raw_data)=="q83")]<-"religious_w1lg"
colnames(raw_data)[which(names(raw_data)=="q89_1")]<-"importancegod_w1lg"
colnames(raw_data)[which(names(raw_data)=="q97_1")]<-"imreserved_w1lg"
colnames(raw_data)[which(names(raw_data)=="q97_2")]<-"imtrusting_w1lg"
colnames(raw_data)[which(names(raw_data)=="q97_3")]<-"imlazy_w1lg"
colnames(raw_data)[which(names(raw_data)=="q97_4")]<-"imrelaxed_w1lg"
colnames(raw_data)[which(names(raw_data)=="q97_5")]<-"imnoartist_w1lg"
colnames(raw_data)[which(names(raw_data)=="q97_6")]<-"imsociable_w1lg"
colnames(raw_data)[which(names(raw_data)=="q97_7")]<-"impicky_w1lg"
colnames(raw_data)[which(names(raw_data)=="q97_8")]<-"imthourough_w1lg"
colnames(raw_data)[which(names(raw_data)=="q97_9")]<-"imnervous_w1lg"
colnames(raw_data)[which(names(raw_data)=="q97_10")]<-"imimaginative_w1lg"
colnames(raw_data)[which(names(raw_data)=="q101")]<-"warjustified_w1lg"
colnames(raw_data)[which(names(raw_data)=="q112_1")]<-"sciencenotrelevant_w1lg"
colnames(raw_data)[which(names(raw_data)=="q108_1")]<-"sciencemakeseasy_w1lg"
colnames(raw_data)[which(names(raw_data)=="q116_1")]<-"imworldcitizen_w1lg"
colnames(raw_data)[which(names(raw_data)=="q116_2")]<-"imlocal_w1lg"
colnames(raw_data)[which(names(raw_data)=="q116_3")]<-"imgerman_w1lg"
colnames(raw_data)[which(names(raw_data)=="q116_4")]<-"imeuropean_w1lg"
colnames(raw_data)[which(names(raw_data)=="q116_5")]<-"imautonomous_w1lg"
colnames(raw_data)[which(names(raw_data)=="q138_1")]<-"satpublictransport_w1lg"
colnames(raw_data)[which(names(raw_data)=="q138_2")]<-"satroads_w1lg"
colnames(raw_data)[which(names(raw_data)=="q138_3")]<-"satschools_w1lg"
colnames(raw_data)[which(names(raw_data)=="q138_4")]<-"satair_w1lg"
colnames(raw_data)[which(names(raw_data)=="q138_5")]<-"satwater_w1lg"
colnames(raw_data)[which(names(raw_data)=="q138_6")]<-"sathealtcare_w1lg"
colnames(raw_data)[which(names(raw_data)=="q138_7")]<-"sathousing_w1lg"
colnames(raw_data)[which(names(raw_data)=="q138_8")]<-"satsetting_w1lg"



#==========================Data analysis======================================

#-----------Step 1, gender
summary(raw_data$gender_w1lg) 
prop.table(table(raw_data$gender_w1lg))

#Label groups. 1=NEP 2=NEPWVS
raw_data$group <- ordered (raw_data$group ,  
                           levels=c( 1,2 ),  
                           labels=c( "NEP","NEPandWVS" ))

#gender and groups
CrossTable(raw_data$gender_w1lg,raw_data$group)
CrossTable(raw_data$group, raw_data$gender_w1lg)

#Gender distribution is not horrible, but for the NEP+WVS group only 40 percent women is not perfect

#-----------------step 2, age
summary(raw_data$age_w1lg) 
mean(raw_data$group, raw_data$age_w1lg)

#mean age by group
aggregate(x = raw_data$age_w1lg,                # Specify data column
          by = list(raw_data$group),              # Specify group indicator
          FUN = mean) 
#The NEPandWVS group is at average 3 years younger 
#t-test reveals that there is no significant difference in age
t.test(raw_data$age_w1lg~raw_data$group)



#-----------------step 3, born in Germany?
summary(raw_data$born_w1lg)
#6 people of the sample were not born in Germany
CrossTable(raw_data$born_w1lg,raw_data$group)
#4 in group NEP, 2 in group NEPandWVS

#-----------------step 4, education

summary(raw_data$education_w1lg)
#Generally the education distribution is good, but there are to many PhDs in our sample
CrossTable(raw_data$education_w1lg,raw_data$group)
#distribution between groups is good

#-----------------step 5, marstatus

summary(raw_data$marstatus_w1lg)
CrossTable(raw_data$marstatus_w1lg,raw_data$group)
#distribution between groups is good

#-----------------step 6, children

summary(raw_data$children_w1lg)
CrossTable(raw_data$children_w1lg,raw_data$group)
#distribution between groups is good

#-----------------step 7, employer

summary(raw_data$employer_w1lg)
CrossTable(raw_data$employer_w1lg,raw_data$group)
#distribution between groups is good


#-----------------step 8, field of work


table(raw_data$fieldofwork_w1lg)
#variable worked well. Diverse sample

#-----------------step 9, income

#Fix the income variable

summary(raw_data$income_w1lg)


#-----------------step 10, chiefwage
summary(raw_data$chiefwage_w1lg)
CrossTable(raw_data$chiefwage_w1lg,raw_data$group)
# Why do we have so many chiefwageearners in our sample?

#-----------------step 10, householdsize
barchart(raw_data$householdsize_w1lg)
summary(raw_data$householdsize_w1lg)
CrossTable(raw_data$householdsize_w1lg,raw_data$group)

#-----------------step 11, district
barchart(raw_data$district_w1lg)
summary(raw_data$district_w1lg)
CrossTable(raw_data$district_w1lg,raw_data$group)


#-----------------step xx, climate
barchart(raw_data$cli_w1lg)
summary(raw_data$cli_w1lg)
CrossTable(raw_data$cli_w1lg,raw_data$group)

barchart(raw_data$cliant_w1lg)
summary(raw_data$cliant_w1lg)
CrossTable(raw_data$cliant_w1lg,raw_data$group)

#--------Diet
barchart(raw_data$diet_w1lg)
summary(raw_data$diet_w1lg)
CrossTable(raw_data$diet_w1lg,raw_data$group)

#----------NEP

#NEP overview (somehow in the coding I missed number 13 in renaming the NEP items)
#renaming nep15 and nep 16
colnames(raw_data)[which(names(raw_data)=="nep14_w1lg")]<-"nep13_w1lg"
colnames(raw_data)[which(names(raw_data)=="nep15_w1lg")]<-"nep14_w1lg"
colnames(raw_data)[which(names(raw_data)=="nep16_w1lg")]<-"nep15_w1lg"


#Looking at the variables
summary(raw_data$nep1_w1lg) 
summary(raw_data$nep2_w1lg) 
summary(raw_data$nep3_w1lg) 
summary(raw_data$nep4_w1lg) 
summary(raw_data$nep5_w1lg) 
summary(raw_data$nep6_w1lg) 
summary(raw_data$nep7_w1lg) 
summary(raw_data$nep8_w1lg) 
summary(raw_data$nep9_w1lg) 
summary(raw_data$nep10_w1lg) 
summary(raw_data$nep11_w1lg) 
summary(raw_data$nep12_w1lg) 
summary(raw_data$nep13_w1lg) 
summary(raw_data$nep14_w1lg) 
summary(raw_data$nep15_w1lg) 


#Create NEP index
# Items need to be recoded (create new vars for that purpose)
#The seven even numbered items, if agreed to by a respondent, are meant to represent statements endorsed by the dominant social paradigm
# (DSP). The eight odd items, if agreed to by a respondent, are meant to refl ect endorsement of the new environmental paradigm (NEP).




#NEP2 5=1, NEP4 5=1, NEP6 5=1, NEP8 5=1, NEP10 5=1, NEP12 5=1, NEP14 5=1, NEP 5=1

#assign to new, numeric variable
raw_data$nep2score_w1lg <- raw_data$nep2_w1lg  
raw_data$nep4score_w1lg <- raw_data$nep4_w1lg 
raw_data$nep6score_w1lg <- raw_data$nep6_w1lg 
raw_data$nep8score_w1lg <- raw_data$nep8_w1lg 
raw_data$nep10score_w1lg <- raw_data$nep10_w1lg 
raw_data$nep12score_w1lg <- raw_data$nep12_w1lg 
raw_data$nep14score_w1lg <- raw_data$nep14_w1lg 

#as.numeric 
raw_data$nep2score_w1lg<- as.numeric(raw_data$nep2_w1lg)
raw_data$nep4score_w1lg<- as.numeric(raw_data$nep4_w1lg)
raw_data$nep6score_w1lg<- as.numeric(raw_data$nep6_w1lg)
raw_data$nep8score_w1lg<- as.numeric(raw_data$nep8_w1lg)
raw_data$nep10score_w1lg<- as.numeric(raw_data$nep10_w1lg)
raw_data$nep12score_w1lg<- as.numeric(raw_data$nep12_w1lg)
raw_data$nep14score_w1lg<- as.numeric(raw_data$nep14_w1lg)

#reverse score
raw_data$nep2score_w1lg <- (6-raw_data$nep2score_w1lg)
raw_data$nep4score_w1lg <- (6-raw_data$nep4score_w1lg)
raw_data$nep6score_w1lg <- (6-raw_data$nep6score_w1lg)
raw_data$nep8score_w1lg <- (6-raw_data$nep8score_w1lg)
raw_data$nep10score_w1lg <- (6-raw_data$nep10score_w1lg)
raw_data$nep12score_w1lg <- (6-raw_data$nep12score_w1lg)
raw_data$nep14score_w1lg <- (6-raw_data$nep14score_w1lg)

#check results
table(raw_data$nep2score_w1lg)
table(raw_data$nep2_w1lg)

table(raw_data$nep4score_w1lg)
table(raw_data$nep4_w1lg)

table(raw_data$nep6score_w1lg)
table(raw_data$nep6_w1lg)

table(raw_data$nep8score_w1lg)
table(raw_data$nep8_w1lg)

table(raw_data$nep10score_w1lg)
table(raw_data$nep10_w1lg)

table(raw_data$nep12score_w1lg)
table(raw_data$nep12_w1lg)

table(raw_data$nep14score_w1lg)
table(raw_data$nep14_w1lg)



#transform not reverse scored variables to numeric
raw_data$nep1score_w1lg<- as.numeric(raw_data$nep1_w1lg)
raw_data$nep3score_w1lg<- as.numeric(raw_data$nep3_w1lg)
raw_data$nep5score_w1lg<- as.numeric(raw_data$nep5_w1lg)
raw_data$nep7score_w1lg<- as.numeric(raw_data$nep7_w1lg)
raw_data$nep9score_w1lg<- as.numeric(raw_data$nep9_w1lg)
raw_data$nep11score_w1lg<- as.numeric(raw_data$nep11_w1lg)
raw_data$nep13score_w1lg<- as.numeric(raw_data$nep13_w1lg)
raw_data$nep15score_w1lg<- as.numeric(raw_data$nep15_w1lg)



#Creating the index= Sum of NEP/15

raw_data$nepscore_w1lg<- (raw_data$nep1score_w1lg +raw_data$nep2score_w1lg+raw_data$nep3score_w1lg+raw_data$nep4score_w1lg+raw_data$nep5score_w1lg+raw_data$nep6score_w1lg+raw_data$nep7score_w1lg+raw_data$nep8score_w1lg+raw_data$nep9score_w1lg+raw_data$nep10score_w1lg+raw_data$nep11score_w1lg+raw_data$nep12score_w1lg+raw_data$nep13score_w1lg+raw_data$nep14score_w1lg+raw_data$nep15score_w1lg)/15

#the lower the NEP, the higher the environmental attitudes/awarenes / the stronger the new ecological paradigm
boxplot(raw_data$nepscore_w1lg)

#compare NEP by group
t.test(raw_data$nepscore_w1lg~raw_data$group)

#NEP-Scores of the two groups are not different.

#Nep score and age
plot(raw_data$nepscore_w1lg~raw_data$age_w1lg )
abline(lm(raw_data$nepscore_w1lg ~ raw_data$age_w1lg))
summary(lm(raw_data$nepscore_w1lg ~ raw_data$age_w1lg),na.rm=T)

#Nep score and gender
#gender should be an unordered factor, but is not so change that
raw_data$gender_w1lg <- factor(raw_data$gender_w1lg, ordered=FALSE)  # an unordered factor


#Regression for Wave1 in LG shows, that women have significant higher environmental awareness than men. 
summary(lm(formula = raw_data$nepscore_w1lg ~  raw_data$gender_w1lg))

#Nep score and age and gender
summary(lm(formula = raw_data$nepscore_w1lg ~ raw_data$age_w1lg + raw_data$gender_w1lg))

#NEP score and climate
summary(lm(formula = raw_data$nepscore_w1lg ~  raw_data$cli_w1lg))


#--------------Snelgar 
var_lab(raw_data$snelgar1_w1lg)
summary(raw_data$snelgar1_w1lg)

raw_data$snelgar2_w1lg
summary(raw_data$snelgar2_w1lg)

raw_data$snelgar3_w1lg
summary(raw_data$snelgar3_w1lg)

raw_data$snelgar4_w1lg
summary(raw_data$snelgar4_w1lg)

raw_data$snelgar5_w1lg
summary(raw_data$snelgar5_w1lg)

raw_data$snelgar6_w1lg
summary(raw_data$snelgar6_w1lg)

raw_data$snelgar7_w1lg
summary(raw_data$snelgar7_w1lg)

raw_data$snelgar8_w1lg
summary(raw_data$snelgar8_w1lg)

############################ In depth data cleaning and analysis for the WVS #############################

#install packages
library(ggplot2)
library(stringr)
library(gtable)
library(expss)
library(MASS)

#separate wvs data in a separate dataframe
rawdata_WVS <- raw_data[which(raw_data$random=="2"),]

#create backup data frame in case you mess something up
rawdata_WVS_bu <- raw_data[which(rawdata$random=="2"),]

# ------ Q184 Satisfaction Questions - 
#create a dataframe for the satisfaction variables
data_sat <- data.frame(rawdata_WVS$sathealth_w1lg,rawdata_WVS$satfinance_w1lg,
                       rawdata_WVS$satsocial_w1lg,rawdata_WVS$satbalance_w1lg,
                       rawdata_WVS$satlife_w1lg)

#explore those variables in more detail
summary(data_sat)
colnames(data_sat)
dim(data_sat)
str(data_sat)
glimpse(data_sat)

#(double)check for missing values
any(is.na(data_sat))

#check the label of the variables
var_lab(data_sat$rawdata_WVS.sathealth_w1lg)="All things considered, how satisfied are you these days with your health condition"
var_lab(data_sat$rawdata_WVS.satfinance_w1lg)="All things considered, how satisfied are you these days with your financial situation"
var_lab(data_sat$rawdata_WVS.satsocial_w1lg)="All things considered, how satisfied are you these days with your social relations"
var_lab(data_sat$rawdata_WVS.satbalance_w1lg)="All things considered, how satisfied are you these days with your work-life balance"
var_lab(data_sat$rawdata_WVS.satlife_w1lg)="All things considered, how satisfied are you these days with your life as a whole"

lapply(data_sat, table)

#boxplot for all satisfaction variables separately
boxplot(data_sat$rawdata_WVS.sathealth_w1lg,
        data_sat$rawdata_WVS.satfinance_w1lg, 
        data_sat$rawdata_WVS.satsocial_w1lg, 
        data_sat$rawdata_WVS.satbalance_w1lg, 
        data_sat$rawdata_WVS.satlife_w1lg, 
        main="Satisfaction Variables", xlab="Variables", ylab="Satisfaction",
        names=c("Health", "Finance", "Social", "WL-Balance", "Life"))

# create a satisfaction index
sat_index <- c((data_sat$rawdata_WVS.sathealth_w1lg+data_sat$rawdata_WVS.satfinance_w1lg
                + data_sat$rawdata_WVS.satsocial_w1lg+data_sat$rawdata_WVS.satbalance_w1lg
                + data_sat$rawdata_WVS.satlife_w1lg)/5)

#assign the satisfaction index to our rawdata_WVS
rawdata_WVS$satindex_w1lg <- sat_index

#label the new variable
var_lab(rawdata_WVS$satindex_w1lg)="Satisfaction Index (mean of all satisfaction values: health, finance, social, balance, life)"


#have a look at the newly created satisfaciton index
sat_index
summary(sat_index)
hist(sat_index)

table(data_sat$rawdata_WVS.sathealth_w1lg, data_sat$rawdata_WVS.satfinance_w1lg)

# boxplot of the satisfaction index
# boxplot(variable_name, main="Überschrift", xlab="X-Achse", ylab="y-Achse", cex.axis=0.7, cex.lab=0.7, col="color/color number")
boxplot(sat_index, main="Satisfaction Index")

# ------ Q38 Valued Qualitites in Children
summary(rawdata_WVS$qualitychild_w1lg)
class(rawdata_WVS$qualitychild_w1lg)
levels(rawdata_WVS$qualitychild_w1lg)
nlevels(rawdata_WVS$qualitychild_w1lg)

#let's have a look whether a certain combination of qualities was especially common
rawdata_WVS$qualitychildch_w1lg <- as.character(rawdata_WVS$qualitychild_w1lg)
table(rawdata_WVS$qualitychild_w1lg)

#currently the variable is stored as a factor, I could not find out how to do the rest to factor variables, so I changed it into a character variable for now
rawdata_WVS$qualitychild_w1lg <- as.character(rawdata_WVS$qualitychild_w1lg)
class(rawdata_WVS$qualitychild_w1lg)

summary(rawdata_WVS$qualitychild_w1lg)
qualitychild.freq = table(rawdata_WVS$qualitychild_w1lg)

#check missing values
any(is.na(rawdata_WVS$qualitychild_w1lg))

#currently the qualities are shown as numbers (1=independence, 2=hard work,...) in one column
#what we need to do is assign each quality a column and indicate whether it was 
#mentioned as an important quality or not

#Let's start with the quality independence which is indicated by the number 1
#step 1: We create a new variable qualitychildindependence_w1lg and 
#identify all observations which contain the number 1. 
#(note that this is a little more complicated than for the other qualities as R cannot 
#distinguish between 11 and 1. Thus, we search for all observations which either contain
#12, 13, or 15, I checked that this includes all observations containing 1)
#the following code then assigns the identified value to this observation (e.g. 12)

qualitychildindependence_w1lg <- stringr::str_extract(rawdata_WVS$qualitychild_w1lg, '12|13|15')
qualitychildindependence_w1lg

#step2: If you have a look at the values of our newly created variable you see that the value is either
# 12, 13, 15, or NA. Thus, we now replace 12, 13, 15 with "mentioned" and NA with "not mentioned"
qualitychildindependence_w1lg <- str_replace(qualitychildindependence_w1lg, "13", "mentioned")
qualitychildindependence_w1lg <- str_replace(qualitychildindependence_w1lg, "12", "mentioned")
qualitychildindependence_w1lg <- str_replace(qualitychildindependence_w1lg, "15", "mentioned")
qualitychildindependence_w1lg <- str_replace(qualitychildindependence_w1lg, "NA", "not mentioned")
qualitychildindependence_w1lg[is.na(qualitychildindependence_w1lg)] <- "not mentioned"

#step 3: In the last step we add this newly created variable to our rawdata_WVS data frame
rawdata_WVS$qualitychildindependence_w1lg <- qualitychildindependence_w1lg

#Now we just have to do the same thing with all the other qualities 
#I am sure there probably is a more elegant way to do this, but it works, so let's get going
#quality: hard work, indicated by the number 2
qualitychildhardwork_w1lg <- stringr::str_extract(rawdata_WVS$qualitychild_w1lg, '2')
qualitychildhardwork_w1lg
qualitychildhardwork_w1lg <- str_replace(qualitychildhardwork_w1lg, "2", "mentioned")
qualitychildhardwork_w1lg[is.na(qualitychildhardwork_w1lg)] <- "not mentioned"
rawdata_WVS$qualitychildhardwork_w1lg <- qualitychildhardwork_w1lg

#quality: feeling of responsibility, indicated by the number 3
qualitychildresponsibility_w1lg <- stringr::str_extract(rawdata_WVS$qualitychild_w1lg, '3')
qualitychildresponsibility_w1lg
qualitychildresponsibility_w1lg <- str_replace(qualitychildresponsibility_w1lg, "3", "mentioned")
qualitychildresponsibility_w1lg[is.na(qualitychildresponsibility_w1lg)] <- "not mentioned"
rawdata_WVS$qualitychildresponsibility_w1lg <- qualitychildresponsibility_w1lg

#quality: imagination, indicated by the number 4
qualitychildimagination_w1lg <- stringr::str_extract(rawdata_WVS$qualitychild_w1lg, '4')
qualitychildimagination_w1lg
qualitychildimagination_w1lg <- str_replace(qualitychildimagination_w1lg, "4", "mentioned")
qualitychildimagination_w1lg[is.na(qualitychildimagination_w1lg)] <- "not mentioned"
rawdata_WVS$qualitychildimagination_w1lg <- qualitychildimagination_w1lg

#quality: tolerance and respect, indicated by the number 5
qualitychildtolerance_w1lg <- stringr::str_extract(rawdata_WVS$qualitychild_w1lg, '5')
qualitychildtolerance_w1lg
qualitychildtolerance_w1lg <- str_replace(qualitychildtolerance_w1lg, "5", "mentioned")
qualitychildtolerance_w1lg[is.na(qualitychildtolerance_w1lg)] <- "not mentioned"
rawdata_WVS$qualitychildtolerance_w1lg <- qualitychildtolerance_w1lg

#quality: thrift and saving money, indicated by the number 6
qualitychildthrift_w1lg <- stringr::str_extract(rawdata_WVS$qualitychild_w1lg, '6')
qualitychildthrift_w1lg
qualitychildthrift_w1lg <- str_replace(qualitychildthrift_w1lg, "6", "mentioned")
qualitychildthrift_w1lg[is.na(qualitychildthrift_w1lg)] <- "not mentioned"
rawdata_WVS$qualitychildthrift_w1lg <- qualitychildthrift_w1lg

#quality: determination and perseverance, indicated by the number 7
qualitychilddetermination_w1lg <- stringr::str_extract(rawdata_WVS$qualitychild_w1lg, '7')
qualitychilddetermination_w1lg
qualitychilddetermination_w1lg <- str_replace(qualitychilddetermination_w1lg, "7", "mentioned")
qualitychilddetermination_w1lg[is.na(qualitychilddetermination_w1lg)] <- "not mentioned"
rawdata_WVS$qualitychilddetermination_w1lg <- qualitychilddetermination_w1lg

#quality: religious faith, indicated by the number 8
qualitychildreligious_w1lg <- stringr::str_extract(rawdata_WVS$qualitychild_w1lg, '8')
qualitychildreligious_w1lg
qualitychildreligious_w1lg <- str_replace(qualitychildreligious_w1lg, "8", "mentioned")
qualitychildreligious_w1lg[is.na(qualitychildreligious_w1lg)] <- "not mentioned"
rawdata_WVS$qualitychildreligious_w1lg <- qualitychildreligious_w1lg

#quality: unselfishness, indicated by the number 9
qualitychildunselfishness_w1lg <- stringr::str_extract(rawdata_WVS$qualitychild_w1lg, '9')
qualitychildunselfishness_w1lg
qualitychildunselfishness_w1lg <- str_replace(qualitychildunselfishness_w1lg, "9", "mentioned")
qualitychildunselfishness_w1lg[is.na(qualitychildunselfishness_w1lg)] <- "not mentioned"
rawdata_WVS$qualitychildunselfishness_w1lg <- qualitychildunselfishness_w1lg

#quality: obedience, indicated by the number 10
qualitychildobedience_w1lg <- stringr::str_extract(rawdata_WVS$qualitychild_w1lg, '10')
qualitychildobedience_w1lg
qualitychildobedience_w1lg <- str_replace(qualitychildobedience_w1lg, "10", "mentioned")
qualitychildobedience_w1lg[is.na(qualitychildobedience_w1lg)] <- "not mentioned"
rawdata_WVS$qualitychildobedience_w1lg <- qualitychildobedience_w1lg

#quality: self-expression, indicated by the number 11
qualitychildselfexpression_w1lg <- stringr::str_extract(rawdata_WVS$qualitychild_w1lg, '11')
qualitychildselfexpression_w1lg
qualitychildselfexpression_w1lg <- str_replace(qualitychildselfexpression_w1lg, "11", "mentioned")
qualitychildselfexpression_w1lg[is.na(qualitychildselfexpression_w1lg)] <- "not mentioned"
rawdata_WVS$qualitychildselfexpression_w1lg <- qualitychildselfexpression_w1lg

#label the new variables
var_lab(rawdata_WVS$qualitychildindependence_w1lg)="Important child qualities: Independence"
var_lab(rawdata_WVS$qualitychildhardwork_w1lg)="Important child qualities: Hard work"
var_lab(rawdata_WVS$qualitychildresponsibility_w1lg)="Important child qualities: Feeling of responsibility"
var_lab(rawdata_WVS$qualitychildtolerance_w1lg)="Important child qualities: Tolerance and respect for other people"
var_lab(rawdata_WVS$qualitychildthrift_w1lg)="Important child qualities: Thrift, saving money and things"
var_lab(rawdata_WVS$qualitychilddetermination_w1lg)="Important child qualities: Determination, perseverance"
var_lab(rawdata_WVS$qualitychildimagination_w1lg)="Important child qualities: Imagination"
var_lab(rawdata_WVS$qualitychildreligious_w1lg)="Important child qualities: Religious faith"
var_lab(rawdata_WVS$qualitychildunselfishness_w1lg)="Important child qualities: Unselfishness"
var_lab(rawdata_WVS$qualitychildobedience_w1lg)="Important child qualities: Obedience"
var_lab(rawdata_WVS$qualitychildselfexpression_w1lg)="Important child qualities: Self-expression"

#currently the class of all our qualitychild variables is character. We now change it back into factors
rawdata_WVS$qualitychildindependence_w1lg <- as.factor(rawdata_WVS$qualitychildindependence_w1lg)
rawdata_WVS$qualitychildhardwork_w1lg <- as.factor(rawdata_WVS$qualitychildhardwork_w1lg)
rawdata_WVS$qualitychidresponsibility_w1lg <- as.factor(rawdata_WVS$qualitychildresponsibility_w1lg)
rawdata_WVS$qualitychildimagination_w1lg <- as.factor(rawdata_WVS$qualitychildimagination_w1lg)
rawdata_WVS$qualitychildtolerance_w1lg <- as.factor(rawdata_WVS$qualitychildtolerance_w1lg)
rawdata_WVS$qualitychildthrift_w1lg <- as.factor(rawdata_WVS$qualitychildthrift_w1lg)
rawdata_WVS$qualitychilddetermination_w1lg <- as.factor(rawdata_WVS$qualitychilddetermination_w1lg)
rawdata_WVS$qualitychildreligious_w1lg <- as.factor(rawdata_WVS$qualitychildreligious_w1lg)
rawdata_WVS$qualitychildunselfishness_w1lg <- as.factor(rawdata_WVS$qualitychildunselfishness_w1lg)
rawdata_WVS$qualitychildobedience_w1lg <- as.factor(rawdata_WVS$qualitychildobedience_w1lg)
rawdata_WVS$qualitychildselfexpression_w1lg <- as.factor(rawdata_WVS$qualitychildselfexpression_w1lg)

#building the autonomy index(AI) (=a computed variable based on the Children qualities battery)
#the idea is to add the positive answers of religious faith and obedience and substract the positive 
#answers of independence and determination. Since mentioned=1 and not mentioned=2, the index will be -2 
#if both Religious faith and Obedience are mentioned but NOT Independence and Determination-Perseverance (+1+1-2-2=-2) 
#and, on the other extreme +2 if both Independence #and Determination-Perseverance are mentioned but NOT 
#Religious Faith and Obedience (-1-1+2+2=+2).
#(http://www.worldvaluessurvey.org/WVSContents.jsp)

#assign numeric value 1 to "mentioned" and 2 to "not mentioned for the four variables used in the AI
qualitychildreligiousai_w1lg <- as.numeric(factor(qualitychildreligious_w1lg))
qualitychildindependenceai_w1lg <- as.numeric(factor(qualitychildindependence_w1lg))
qualitychilddeterminationai_w1lg <- as.numeric(factor(qualitychilddetermination_w1lg))

#This does not work for obedience since nobody actually mentioned it. Thus, the only level 
#this factor currently has is not mentioned = 1
#I'm sure there is a better way to do this but I just manually set all the values of "not mentioned" to 2
qualitychildobedienceai_w1lg <- ifelse(qualitychildobedience_w1lg=="not mentioned", 2, 1)
qualitychildobedienceai_w1lg

#In the next step we create the autonomy index according to the explanation given by the WVS
autonomyindex_w1lg <- (qualitychildreligiousai_w1lg+qualitychildobedienceai_w1lg)-
  (qualitychildindependenceai_w1lg+qualitychilddeterminationai_w1lg)

#let's have a first look at the index
#maybe check the fit?
autonomyindex_w1lg
table(autonomyindex_w1lg)

#label the index
var_lab(autonomyindex_w1lg)="The autonomy index is created from the valued qualities
in children variables: positive answers to faith and obedience are added, positive 
answers to independence and determination are subtracted"

#convert it into a factor variable
autonomyindex_w1lg <- ordered (autonomyindex_w1lg ,  
                           levels=c(-2,-1,0,1,2),  
                           labels=c( "strong authority/conformity orientation",
                           "weak authority/conformity orientation", 
                           "neither authority/conformity nor autonomy oriented",
                           "weak autonomy orientation",
                           "strong autonomy orientation"))

#visualize the index
barchart(autonomyindex_w1lg)

#assign the index to the rawdata_WVS set
rawdata_WVS$autonomyindex_w1lg <- autonomyindex_w1lg

# ------ Q40 Trusting
#explore variable in more detail
summary(rawdata_WVS$trustpeople_w1lg)
class(rawdata_WVS$trustpeople_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$trustpeople_w1lg))

# ------ Q44 Gender Statements I
#This question includes the four variables jobsformen_w1lg, richwomanproblem_w1lg, womanjobindependent_w1lg
#get an overview:
summary(rawdata_WVS$jobsformen_w1lg)
summary(rawdata_WVS$jobsfornationals_w1lg)
summary(rawdata_WVS$richwomanproblem_w1lg)
summary(rawdata_WVS$womanjobindependent_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$jobsformen_w1lg))
any(is.na(rawdata_WVS$jobsfornationals_w1lg))
any(is.na(rawdata_WVS$richwomanproblem_w1lg))
any(is.na(rawdata_WVS$womanjobindependent_w1lg))
#no missing values

# ------ Q45 Gender Statements II
#This question includes the six variables ifegoalparents_w1lg, momworkingbad_w1lg, menbetterpoliticians_w1lg, uniformen_w1lg, menbetterbusiness_w1lg, happyhousewife_w1lg
summary(rawdata_WVS$lifegoalparents_w1lg)
summary(rawdata_WVS$momworkingbad_w1lg)
summary(rawdata_WVS$menbetterpoliticians_w1lg)
summary(rawdata_WVS$uniformen_w1lg)
summary(rawdata_WVS$menbetterbusiness_w1lg)
summary(rawdata_WVS$happyhousewife_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$lifegoalparents_w1lg))
any(is.na(rawdata_WVS$momworkingbad_w1lg))
any(is.na(rawdata_WVS$menbetterpoliticians_w1lg))
any(is.na(rawdata_WVS$uniformen_w1lg))
any(is.na(rawdata_WVS$menbetterbusiness_w1lg))
any(is.na(rawdata_WVS$happyhousewife_w1lg))
#no missing values

# ------ Q46 no choice vs. choice
#explore variable in more detail
summary(rawdata_WVS$nochoicevschoice_w1lg)
class(rawdata_WVS$nochoicevschoice_w1lg)
#variable should contain values between 1 and 10 but contains values from 1 until 18.
#qualtrics assigned different values to the actual ticked numbers so that:
# 11 = 3, 12 = 4, 13 = 5, 14 = 6, 15 = 7, 16 = 8, 17 = 9, 18 = 10
#now we reassign the correct values
nochoicevschoice_w1lg <- as.character(rawdata_WVS$nochoicevschoice_w1lg)
nochoicevschoice_w1lg[nochoicevschoice_w1lg == "11"] <- "3"
nochoicevschoice_w1lg[nochoicevschoice_w1lg == "12"] <- "4"
nochoicevschoice_w1lg[nochoicevschoice_w1lg == "13"] <- "5"
nochoicevschoice_w1lg[nochoicevschoice_w1lg == "14"] <- "6"
nochoicevschoice_w1lg[nochoicevschoice_w1lg == "15"] <- "7"
nochoicevschoice_w1lg[nochoicevschoice_w1lg == "16"] <- "8"
nochoicevschoice_w1lg[nochoicevschoice_w1lg == "17"] <- "9"
nochoicevschoice_w1lg[nochoicevschoice_w1lg == "18"] <- "10"
nochoicevschoice_w1lg <- as.numeric(nochoicevschoice_w1lg)

rawdata_WVS$nochoicevschoice_w1lg <- nochoicevschoice_w1lg
summary(rawdata_WVS$nochoicevschoice_w1lg)

#check label
var_lab(rawdata_WVS$nochoicevschoice_w1lg) = "Some people feel they have completely free choice and control over their lives, while other people feel that what they do has no real effect on what happens to them. Please use this scale where 1 means 'no choice at all' and 10 means 'a great deal of choice' to indicate how much freedom of choice and control you feel you have over the way your life turns out."

#(double)check for missing values
any(is.na(rawdata_WVS$nochoicevschoice_w1lg))
#there are 3 missing values according to summary
which(is.na(rawdata_WVS$nochoicevschoice_w1lg))
#observations 11, 12, 13

# ------ Q47 unfair vs. fair
#explore variable in more detail
summary(rawdata_WVS$unfairvsfair_w1lg)
class(rawdata_WVS$unfairvsfair_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$unfairvsfair_w1lg))
#no missing values

# ------ Q50 aims for the country
#explore variables in more detail
summary(rawdata_WVS$countryaimgrowth_w1lg)
summary(rawdata_WVS$countryaimmilitary_w1lg)
summary(rawdata_WVS$countryaimparticipation_w1lg)
summary(rawdata_WVS$countryaimbeauty_w1lg)
class(rawdata_WVS$countryaimgrowth_w1lg)
levels(rawdata_WVS$countryaimgrowth_w1lg)

#in order to unify names of the labels we want to change "Most Important" to "Most important"
#and "Second Most important" to "Second most important"
rawdata_WVS$countryaimgrowth_w1lg <- ifelse(rawdata_WVS$countryaimgrowth_w1lg=="Most Important", "Most important",
                                            ifelse(rawdata_WVS$countryaimgrowth_w1lg=="Second Most important", "Second most important", "Not mentioned"))
rawdata_WVS$countryaimmilitary_w1lg <- ifelse(rawdata_WVS$countryaimmilitary_w1lg=="Most Important", "Most important",
                                              ifelse(rawdata_WVS$countryaimmilitary_w1lg=="Second Most important", "Second most important", "Not mentioned"))
rawdata_WVS$countryaimparticipation_w1lg <- ifelse(rawdata_WVS$countryaimparticipation_w1lg=="Most Important", "Most important",
                                                   ifelse(rawdata_WVS$countryaimparticipation_w1lg=="Second Most important", "Second most important", "Not mentioned"))
rawdata_WVS$countryaimbeauty_w1lg <- ifelse(rawdata_WVS$countryaimbeauty_w1lg=="Most Important", "Most important",
                                            ifelse(rawdata_WVS$countryaimbeauty_w1lg=="Second Most important", "Second most important", "Not mentioned"))

#for each case where the respective aim was not selected as most important or second most important
#a missing value is recorded
#we want to assign the missing values to the level "not mentioned" 
rawdata_WVS$countryaimgrowth_w1lg <- ordered(rawdata_WVS$countryaimgrowth_w1lg, levels = c("Most important", "Second most important", "Not mentioned"))
rawdata_WVS$countryaimmilitary_w1lg <- ordered(rawdata_WVS$countryaimmilitary_w1lg, levels = c("Most important", "Second most important", "Not mentioned"))
rawdata_WVS$countryaimparticipation_w1lg <- ordered(rawdata_WVS$countryaimparticipation_w1lg, levels = c("Most important", "Second most important", "Not mentioned"))
rawdata_WVS$countryaimbeauty_w1lg <- ordered(rawdata_WVS$countryaimbeauty_w1lg, levels = c("Most important", "Second most important", "Not mentioned"))

levels(rawdata_WVS$countryaimgrowth_w1lg) <- c(levels(rawdata_WVS$countryaimgrowth_w1lg), "Not mentioned")
rawdata_WVS$countryaimgrowth_w1lg[is.na(rawdata_WVS$countryaimgrowth_w1lg)] <- "Not mentioned"
levels(rawdata_WVS$countryaimmilitary_w1lg) <- c(levels(rawdata_WVS$countryaimmilitary_w1lg), "Not mentioned")
rawdata_WVS$countryaimmilitary_w1lg[is.na(rawdata_WVS$countryaimmilitary_w1lg)] <- "Not mentioned"
levels(rawdata_WVS$countryaimparticipation_w1lg) <- c(levels(rawdata_WVS$countryaimparticipation_w1lg), "Not mentioned")
rawdata_WVS$countryaimparticipation_w1lg[is.na(rawdata_WVS$countryaimparticipation_w1lg)] <- "Not mentioned"
levels(rawdata_WVS$countryaimbeauty_w1lg) <- c(levels(rawdata_WVS$countryaimbeauty_w1lg), "Not mentioned")
rawdata_WVS$countryaimbeauty_w1lg[is.na(rawdata_WVS$countryaimbeauty_w1lg)] <- "Not mentioned"

summary(rawdata_WVS$countryaimgrowth_w1lg)
summary(rawdata_WVS$countryaimmilitary_w1lg)
summary(rawdata_WVS$countryaimparticipation_w1lg)
summary(rawdata_WVS$countryaimbeauty_w1lg)

#create the variables countryaimfirstchoice_w1lg und countryaimsecondchoice_w1lg 
#and assign the name of the aim to this variable that was mentioned
countryaimfirstchoice_w1lg <- as.factor(ifelse(rawdata_WVS$countryaimgrowth_w1lg=="Most important",
                                     "A high level of economic growth", 
                                     ifelse(rawdata_WVS$countryaimmilitary_w1lg=="Most important",
                                            "Making sure this country has strong defense forces",
                                            ifelse(rawdata_WVS$countryaimparticipation_w1lg=="Most important",
                                                   "Seeing that people have more say about how things are done at their jobs and in their communities", "Trying to make our cities and countryside more beautiful"))))

#Since nobody so far mentioned military as most important country aim, we have to add this level manually
levels(countryaimfirstchoice_w1lg) <- c(levels(countryaimfirstchoice_w1lg), "Making sure this country has strong defense forces")

#check whether it works
summary(countryaimfirstchoice_w1lg)
class(countryaimfirstchoice_w1lg)

#label the variable
label(countryaimfirstchoice_w1lg)="Aims of country: First choice"

#include variable in rawdata_WVS dataframe
rawdata_WVS$countryaimfirstchoice_w1lg <- countryaimfirstchoice_w1lg

#now we just have to do the same with the second most important aim
countryaimsecondchoice_w1lg <- as.factor(ifelse(rawdata_WVS$countryaimgrowth_w1lg=="Second most important",
                                               "A high level of economic growth", 
                                               ifelse(rawdata_WVS$countryaimmilitary_w1lg=="Second most important",
                                                      "Making sure this country has strong defense forces",
                                                      ifelse(rawdata_WVS$countryaimparticipation_w1lg=="Second most important",
                                                             "Seeing that people have more say about how things are done at their jobs and in their communities", "Trying to make our cities and countryside more beautiful"))))

#check whether it works
summary(countryaimsecondchoice_w1lg)
class(countryaimfirstchoice_w1lg)

#label the variable
label(countryaimsecondchoice_w1lg)="Aims of country: Second choice"

#include variable in rawdata_WVS dataframe
rawdata_WVS$countryaimsecondchoice_w1lg <- countryaimsecondchoice_w1lg

#labelling did not work, so let's try again
label(rawdata_WVS$countryaimsecondchoice_w1lg)="Aims of country: Second choice"

# ------ Q53 important things I

#explore variables in more detail
class(rawdata_WVS$impoeconomy_w1lg)
levels(rawdata_WVS$impoeconomy_w1lg)

#in order to unify names of the labels we want to change "Most Important" to "Most important"
rawdata_WVS$impoeconomy_w1lg <- ifelse(rawdata_WVS$impoeconomy_w1lg=="Most Important", "Most important",
                                       ifelse(rawdata_WVS$impoeconomy_w1lg=="Second Most important", "Second most important", "Not mentioned"))
rawdata_WVS$impojustsociety_w1lg <- ifelse(rawdata_WVS$impojustsociety_w1lg=="Most Important", "Most important",
                                           ifelse(rawdata_WVS$impojustsociety_w1lg=="Second Most important", "Second most important", "Not mentioned"))
rawdata_WVS$impoideasociety_w1lg <- ifelse(rawdata_WVS$impoideasociety_w1lg=="Most Important", "Most important",
                                           ifelse(rawdata_WVS$impoideasociety_w1lg=="Second Most important", "Second most important", "Not mentioned"))
rawdata_WVS$imponocrime_w1lg <- ifelse(rawdata_WVS$imponocrime_w1lg=="Most Important", "Most important",
                                       ifelse(rawdata_WVS$imponocrime_w1lg=="Second Most important", "Second most important", "Not mentioned"))


#for each case where the respective aspect was not selected as most important or second most important
#a missing value is recorded
#we want to assign the missing values to the level "Not mentioned" 

rawdata_WVS$impoeconomy_w1lg <- ordered(rawdata_WVS$impoeconomy_w1lg, levels = c("Most important", "Second most important", "Not mentioned"))
rawdata_WVS$impojustsociety_w1lg <- ordered(rawdata_WVS$impojustsociety_w1lg, levels = c("Most important", "Second most important", "Not mentioned"))
rawdata_WVS$impoideasociety_w1lg <- ordered(rawdata_WVS$impoideasociety_w1lg, levels = c("Most important", "Second most important", "Not mentioned"))
rawdata_WVS$imponocrime_w1lg <- ordered(rawdata_WVS$imponocrime_w1lg, levels = c("Most important", "Second most important", "Not mentioned"))

levels(rawdata_WVS$impoeconomy_w1lg) <- c(levels(rawdata_WVS$impoeconomy_w1lg), "Not mentioned")
rawdata_WVS$impoeconomy_w1lg[is.na(rawdata_WVS$impoeconomy_w1lg)] <- "Not mentioned"
levels(rawdata_WVS$impojustsociety_w1lg) <- c(levels(rawdata_WVS$impojustsociety_w1lg), "Not mentioned")
rawdata_WVS$impojustsociety_w1lg[is.na(rawdata_WVS$impojustsociety_w1lg)] <- "Not mentioned"
levels(rawdata_WVS$impoideasociety_w1lg) <- c(levels(rawdata_WVS$impoideasociety_w1lg), "Not mentioned")
rawdata_WVS$impoideasociety_w1lg[is.na(rawdata_WVS$impoideasociety_w1lg)] <- "Not mentioned"
levels(rawdata_WVS$imponocrime_w1lg) <- c(levels(rawdata_WVS$imponocrime_w1lg), "Not mentioned")
rawdata_WVS$imponocrime_w1lg[is.na(rawdata_WVS$imponocrime_w1lg)] <- "Not mentioned"

#see whether it works and explore the variables
summary(rawdata_WVS$impoeconomy_w1lg)
summary(rawdata_WVS$impojustsociety_w1lg)
summary(rawdata_WVS$impoideasociety_w1lg)
summary(rawdata_WVS$imponocrime_w1lg)

#create the variables importancefirstchoice_w1lg und importancesecondchoice_w1lg 
#and assign the name of the aim to this variable that was mentioned
importancefirstchoice_w1lg <- as.factor(ifelse(rawdata_WVS$impoeconomy_w1lg=="Most important",
                                               "A stable economy", 
                                               ifelse(rawdata_WVS$impojustsociety_w1lg=="Most important",
                                                      "Progress toward a less impersonal and more humane society",
                                                      ifelse(rawdata_WVS$impoideasociety_w1lg=="Most important",
                                                             "Progress toward a society in which ideas count more than money", "The fight against crime"))))

#check whether it works
summary(importancefirstchoice_w1lg)
class(importancefirstchoice_w1lg)

#label the variable
label(countryaimfirstchoice_w1lg)="Most important: First choice"

#include variable in rawdata_WVS dataframe
rawdata_WVS$importancefirstchoice_w1lg <- importancefirstchoice_w1lg

#now we just have to do the same with the second most important aim
importancesecondchoice_w1lg <- as.factor(ifelse(rawdata_WVS$impoeconomy_w1lg=="Second most important",
                                               "A stable economy", 
                                               ifelse(rawdata_WVS$impojustsociety_w1lg=="Second most important",
                                                      "Progress toward a less impersonal and more humane society",
                                                      ifelse(rawdata_WVS$impoideasociety_w1lg=="Second most important",
                                                             "Progress toward a society in which ideas count more than money", "The fight against crime"))))

#check whether it works
summary(importancesecondchoice_w1lg)
class(importancesecondchoice_w1lg)

#include variable in rawdata_WVS dataframe
rawdata_WVS$importancesecondchoice_w1lg <-importancesecondchoice_w1lg

#label the variable
label(rawdata_WVS$importancesecondchoice_w1lg)="Most important: Second choice"

# ------ Q55 Aims of respondent

#in order to unify names of the labels we want to change "Most Important" to "Most important"
rawdata_WVS$impolaworder_w1lg <- ifelse(rawdata_WVS$impolaworder_w1lg=="Most Important", "Most important",
                                       ifelse(rawdata_WVS$impolaworder_w1lg=="Second Most important", "Second most important", "Not mentioned"))
rawdata_WVS$impomoresay_w1lg <- ifelse(rawdata_WVS$impomoresay_w1lg=="Most Important", "Most important",
                                           ifelse(rawdata_WVS$impomoresay_w1lg=="Second Most important", "Second most important", "Not mentioned"))
rawdata_WVS$impolowprices_w1lg <- ifelse(rawdata_WVS$impolowprices_w1lg=="Most Important", "Most important",
                                           ifelse(rawdata_WVS$impolowprices_w1lg=="Second Most important", "Second most important", "Not mentioned"))
rawdata_WVS$impofreespeech_w1lg <- ifelse(rawdata_WVS$impofreespeech_w1lg=="Most Important", "Most important",
                                       ifelse(rawdata_WVS$impofreespeech_w1lg=="Second Most important", "Second most important", "Not mentioned"))


#for each case where the respective aspect was not selected as most important or second most important
#a missing value is recorded
#we want to assign the missing values to the level "Not mentioned" 

rawdata_WVS$impolaworder_w1lg <- ordered(rawdata_WVS$impolaworder_w1lg, levels = c("Most important", "Second most important", "Not mentioned"))
rawdata_WVS$impomoresay_w1lg <- ordered(rawdata_WVS$impomoresay_w1lg, levels = c("Most important", "Second most important", "Not mentioned"))
rawdata_WVS$impolowprices_w1lg <- ordered(rawdata_WVS$impolowprices_w1lg, levels = c("Most important", "Second most important", "Not mentioned"))
rawdata_WVS$impofreespeech_w1lg <- ordered(rawdata_WVS$impofreespeech_w1lg, levels = c("Most important", "Second most important", "Not mentioned"))

levels(rawdata_WVS$impolaworder_w1lg) <- c(levels(rawdata_WVS$impolaworder_w1lg), "Not mentioned")
rawdata_WVS$impolaworder_w1lg[is.na(rawdata_WVS$impolaworder_w1lg)] <- "Not mentioned"
levels(rawdata_WVS$impomoresay_w1lg) <- c(levels(rawdata_WVS$impomoresay_w1lg), "Not mentioned")
rawdata_WVS$impomoresay_w1lg[is.na(rawdata_WVS$impomoresay_w1lg)] <- "Not mentioned"
levels(rawdata_WVS$impolowprices_w1lg) <- c(levels(rawdata_WVS$impolowprices_w1lg), "Not mentioned")
rawdata_WVS$impolowprices_w1lg[is.na(rawdata_WVS$impolowprices_w1lg)] <- "Not mentioned"
levels(rawdata_WVS$impofreespeech_w1lg) <- c(levels(rawdata_WVS$impofreespeech_w1lg), "Not mentioned")
rawdata_WVS$impofreespeech_w1lg[is.na(rawdata_WVS$impofreespeech_w1lg)] <- "Not mentioned"

#see whether it works and explore the variables
summary(rawdata_WVS$impolaworder_w1lg)
summary(rawdata_WVS$impomoresay_w1lg)
summary(rawdata_WVS$impolowprices_w1lg)
summary(rawdata_WVS$impofreespeech_w1lg)

#check for missing values
any(is.na(rawdata_WVS$impolaworder_w1lg))
any(is.na(rawdata_WVS$impomoresay_w1lg))
any(is.na(rawdata_WVS$impolowprices_w1lg))
any(is.na(rawdata_WVS$impofreespeech_w1lg))
#no missing values (but we also reassigned them to "Not mentioned" which would be
#problematic in theory but since we forced the response here, we should be fine)

#create the variables respondentaimfirstchoice_w1lg und respondentaimsecondchoice_w1lg 
#and assign the name of the aim to this variable that was mentioned
respondentaimfirstchoice_w1lg <- as.factor(ifelse(rawdata_WVS$impolaworder_w1lg=="Most important",
                                               "Maintaining order in the nation", 
                                               ifelse(rawdata_WVS$impomoresay_w1lg=="Most important",
                                                      "Giving people more say in important government decisions",
                                                      ifelse(rawdata_WVS$impolowprices_w1lg=="Most important",
                                                             "Fighting rising prices", "Protecting freedom of speech"))))

#check whether it works
summary(respondentaimfirstchoice_w1lg)
class(respondentaimfirstchoice_w1lg)

#label the variable
label(respondentaimfirstchoice_w1lg)="Aims of respondent: First choice"

#include variable in rawdata_WVS dataframe
rawdata_WVS$respondentaimfirstchoice_w1lg <- respondentaimfirstchoice_w1lg

#now we just have to do the same with the second most important aim
respondentaimsecondchoice_w1lg <- as.factor(ifelse(rawdata_WVS$impolaworder_w1lg=="Second most important",
                                                   "Maintaining order in the nation", 
                                                   ifelse(rawdata_WVS$impomoresay_w1lg=="Second most important",
                                                          "Giving people more say in important government decisions",
                                                          ifelse(rawdata_WVS$impolowprices_w1lg=="Second most important",
                                                                 "Fighting rising prices", "Protecting freedom of speech"))))
#check whether it works
summary(respondentaimsecondchoice_w1lg)
class(respondentaimsecondchoice_w1lg)

#label the variable
label(respondentaimsecondchoice_w1lg)="Aims of respondent: Second choice"

#include variable in rawdata_WVS dataframe
rawdata_WVS$respondentaimsecondchoice_w1lg <-respondentaimsecondchoice_w1lg

# ------ Q181 Political priorities
#explore variable in more detail
summary(rawdata_WVS$countryegovsaltru_w1lg)
class(rawdata_WVS$countryegovsaltru_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$countryegovsaltru_w1lg))
#there are 3 missing values according to summary
which(is.na(rawdata_WVS$nochoicevschoice_w1lg))
#observations 11, 12, 13

# ------ Q56 Future changes
#explore corresponding variables in more detail
summary(rawdata_WVS$lessworkgoodbad_w1lg)
summary(rawdata_WVS$moretechgoodbad_w1lg)
summary(rawdata_WVS$moreauthoritygoodbad_w1lg)

class(rawdata_WVS$lessworkgoodbad_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$lessworkgoodbad_w1lg))
any(is.na(rawdata_WVS$moretechgoodbad_w1lg))
any(is.na(rawdata_WVS$moreauthoritygoodbad_w1lg))
#no missing values

# ------ Q57 Schwartz Value Inventory
#a look at the corresponding variables showed that pvqbenevolence_w1lg is included
#in the rawdata twice. One of the pvqbenevolence variables is "It is important for this
#person to do something for the good of society, I think this should be pvquniversalism
#as we already have 1 pvquniversalism I'll relabel it as pvquniversalismsoc and pvquniversalismenv
#as one of them relates to society and the other one to the environment

colnames(rawdata_WVS)[149]<-"pvquniversalismsoc_w1lg"
colnames(rawdata_WVS)[154]<-"pvquniversalismenv_w1lg"

#explore corresponding variables in more detail
summary(rawdata_WVS$pvqselfdirection_w1lg)
summary(rawdata_WVS$pvqpower_w1lg)
summary(rawdata_WVS$pvqsecurity_w1lg)
summary(rawdata_WVS$pvqhedonism_w1lg)
summary(rawdata_WVS$pvquniversalismsoc_w1lg)
summary(rawdata_WVS$pvqbenevolence_w1lg)
summary(rawdata_WVS$pvqachievement_w1lg)
summary(rawdata_WVS$pvqstimulation_w1lg)
summary(rawdata_WVS$pvqconformity_w1lg)
summary(rawdata_WVS$pvquniversalismenv_w1lg)
summary(rawdata_WVS$pvqtradition_w1lg)

class(rawdata_WVS$pvqselfdirection_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$pvqselfdirection_w1lg))
any(is.na(rawdata_WVS$pvqpower_w1lg))
any(is.na(rawdata_WVS$pvqsecurity_w1lg))
any(is.na(rawdata_WVS$pvqhedonism_w1lg))
any(is.na(rawdata_WVS$pvquniversalismsoc_w1lg))
any(is.na(rawdata_WVS$pvqbenevolence_w1lg))
any(is.na(rawdata_WVS$pvqachievement_w1lg))
any(is.na(rawdata_WVS$pvqstimulation_w1lg))
any(is.na(rawdata_WVS$pvqconformity_w1lg))
any(is.na(rawdata_WVS$pvquniversalismenv_w1lg))
any(is.na(rawdata_WVS$pvqtradition_w1lg))
#no missing values

# ------ Q59 Protecting environment vs. economic growth
#explore variable
summary(rawdata_WVS$ecovsgrowth_w1lg)
class(rawdata_WVS$ecovsgrowth_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$ecovsgrowth_w1lg))
#no missing values

# ------ Q60 Activities in past two years
#explore variable
summary(rawdata_WVS$moneytoeco_w1lg)
summary(rawdata_WVS$demoforeco_w1lg)
class(rawdata_WVS$moneytoeco_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$moneytoeco_w1lg))
any(is.na(rawdata_WVS$demoforeco_w1lg))
#no missing values

# ------ Q62 Activities in past two years
#explore variable
summary(rawdata_WVS$activismpetition_w1lg)
summary(rawdata_WVS$activismdemo_w1lg)
summary(rawdata_WVS$activismboycott_w1lg)
summary(rawdata_WVS$activismstrike_w1lg)
summary(rawdata_WVS$activismotherprotest_w1lg)

class(rawdata_WVS$activismpetition_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$activismpetition_w1lg))
any(is.na(rawdata_WVS$activismboycott_w1lg))
any(is.na(rawdata_WVS$activismdemo_w1lg))
any(is.na(rawdata_WVS$activismstrike_w1lg))
any(is.na(rawdata_WVS$activismotherprotest_w1lg))
#no missing values

# ------ Q65 Political orientation
#explore variable
summary(rawdata_WVS$leftright_w1lg)
class(rawdata_WVS$leftright_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$leftright_w1lg))
#no missing values

# ------ Q70 Trusting people
#explore variable
summary(rawdata_WVS$trustknownpeople_w1lg)
summary(rawdata_WVS$trustmetpeople_w1lg)

class(rawdata_WVS$trustknownpeople_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$trustknownpeople_w1lg))
any(is.na(rawdata_WVS$trustmetpeople_w1lg))
#there are missing values -> find out where they are
which(is.na(rawdata_WVS$trustknownpeople_w1lg))
which(is.na(rawdata_WVS$trustmetpeople_w1lg))
#the missing value is in observation 50
#no missing values

# ------ Q73 Trusting organizations
#explore variable
summary(rawdata_WVS$confhealthsector_w1lg)
summary(rawdata_WVS$confgovernment_w1lg)
summary(rawdata_WVS$confcompanies_w1lg)
summary(rawdata_WVS$confecoorga_w1lg)
summary(rawdata_WVS$confcountry_w1lg)
summary(rawdata_WVS$confeu_w1lg)
summary(rawdata_WVS$confun_w1lg)

class(rawdata_WVS$confcompanies_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$confhealthsector_w1lg))
any(is.na(rawdata_WVS$confgovernment_w1lg))
any(is.na(rawdata_WVS$confcompanies_w1lg))
any(is.na(rawdata_WVS$confecoorga_w1lg))
any(is.na(rawdata_WVS$confcountry_w1lg))
any(is.na(rawdata_WVS$confeu_w1lg))
any(is.na(rawdata_WVS$confun_w1lg))
#no missing values

# ------ Q79 Democracy perception in country
#explore variable in more detail
summary(rawdata_WVS$undemocraticvsdemocratic_w1lg)
class(rawdata_WVS$undemocraticvsdemocratic_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$undemocraticvsdemocratic_w1lg))

# ------ Q78 Respect for human rights in country
#explore variable in more detail
summary(rawdata_WVS$respecthumanrights_w1lg)
class(rawdata_WVS$respecthumanrights_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$respecthumanrights_w1lg))

# ------ Q81 Attending religious services
#correct typo in variable name
colnames(rawdata_WVS)[which(names(rawdata_WVS)=="relegiousevents_w1lg")]<-"religiousevents_w1lg"

#explore variable in more detail
summary(rawdata_WVS$religiousevents_w1lg)
class(rawdata_WVS$religiousevents_w1lg)

#the different answer options are currently saved as numbers from 1-7
#Followingly we rename the answers with the actual given answers indicating how often people attend religious events

rawdata_WVS$religiousevents_w1lg <- ifelse(rawdata_WVS$religiousevents_w1lg=="1", "More than once a week",
                                        ifelse(rawdata_WVS$religiousevents_w1lg=="2", "Once a week", 
                                               ifelse(rawdata_WVS$religiousevents_w1lg=="3", "Once a month",
                                                      ifelse(rawdata_WVS$religiousevents_w1lg=="4","Only on special holidays",
                                                             ifelse(rawdata_WVS$religiousevents_w1lg=="5", "Once a year",
                                                                    ifelse(rawdata_WVS$religiousevents_w1lg=="6", "Less often than once a year", "Never, practically never"))))))

#change it from character to ordered factor
rawdata_WVS$religiousevents_w1lg <- ordered(rawdata_WVS$religiousevents_w1lg, levels = c("More than once a week", "Once a week", "Once a month", "Only on special holidays", "Once a year", "Less often than once a year", "Never, practically never"))

#check label
var_lab(rawdata_WVS$religiousevents_w1lg) = "How often do you attend religious services usually?"

#(double)check for missing values
any(is.na(rawdata_WVS$religiousevents_w1lg))
#no missing values

# ------ Q83 Religiousness
#explore variable in more detail
summary(rawdata_WVS$religious_w1lg)
class(rawdata_WVS$religious_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$religious_w1lg))
#no missing values

# ------ Q89 Importance of god
#explore variable in more detail
summary(rawdata_WVS$importancegod_w1lg)
class(rawdata_WVS$importancegod_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$importancegod_w1lg))
#no missing values

# ------ Q97 Perceptions of self
#correct typo in variablename amsociable_w1lg
colnames(rawdata_WVS)[which(names(rawdata_WVS)=="amsociable_w1lg")]<-"imsociable_w1lg"

#explore variable in more detail
summary(rawdata_WVS$imreserved_w1lg)
summary(rawdata_WVS$imtrusting_w1lg)
summary(rawdata_WVS$imlazy_w1lg)
summary(rawdata_WVS$imrelaxed_w1lg)
summary(rawdata_WVS$imnoartist_w1lg)
summary(rawdata_WVS$imsociable_w1lg)
summary(rawdata_WVS$impicky_w1lg)
summary(rawdata_WVS$imthourough_w1lg)
summary(rawdata_WVS$imnervous_w1lg)
summary(rawdata_WVS$imimaginative_w1lg)

class(rawdata_WVS$imreserved_w1lg)

#find out which observations are the missing values
which(is.na(rawdata_WVS$imtrusting_w1lg))
#observations 3, 29
which(is.na(rawdata_WVS$imlazy_w1lg))
#observation 23

# ------ Q101 Necessity of war for justice
#explore variable in more detail
summary(rawdata_WVS$warjustified_w1lg)
class(rawdata_WVS$warjustified_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$warjustified_w1lg))
#no missing values

# ------ Q112 Importance of science in daily life
#explore variable in more detail
summary(rawdata_WVS$sciencenotrelevant_w1lg)
class(rawdata_WVS$sciencenotrelevant_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$sciencenotrelevant_w1lg))
#no missing values

# ------ Q112 Healthier, easier, more comfortable lives through technology
#explore variable in more detail
summary(rawdata_WVS$sciencemakeseasy_w1lg)
class(rawdata_WVS$sciencemakeseasy_w1lg)

#(double)check for missing values
any(is.na(rawdata_WVS$sciencemakeseasy_w1lg))
#no missing values

# ------ Q116 Own perception of relation to the world
#explore variable in more detail
summary(rawdata_WVS$imworldcitizen_w1lg)
summary(rawdata_WVS$imlocal_w1lg)
summary(rawdata_WVS$imgerman_w1lg)
summary(rawdata_WVS$imeuropean_w1lg)
summary(rawdata_WVS$imautonomous_w1lg)

class(rawdata_WVS$imworldcitizen_w1lg)
#no missing values

# ------ Q138 Living quality in own city
#explore variable in more detail
summary(rawdata_WVS$satpublictransport_w1lg)
summary(rawdata_WVS$satroads_w1lg)
summary(rawdata_WVS$satschools_w1lg)
summary(rawdata_WVS$satair_w1lg)
summary(rawdata_WVS$satwater_w1lg)
summary(rawdata_WVS$sathealtcare_w1lg)
summary(rawdata_WVS$sathousing_w1lg)
summary(rawdata_WVS$satsetting_w1lg)
class(rawdata_WVS$importancegod_w1lg)
#no missing values

#bring the rawdata_WVS together with the original raw_data script and indicate for all 
#observations which didn't get the WVS "question not asked" for the respective WVS questions

#in case you mess something up replace the variables with the variables from the backup data frame
#rawdata_WVS$impoeconomy_w1lg <- rawdata_WVS_bu$impoeconomy_w1lg
#rawdata_WVS$impojustsociety_w1lg <- rawdata_WVS_bu$impojustsociety_w1lg
#rawdata_WVS$impoideasociety_w1lg <- rawdata_WVS_bu$impoideasociety_w1lg
#rawdata_WVS$imponocrime_w1lg <- rawdata_WVS_bu$imponocrime_w1lg


#in case you have to delete variables from a dataframe
#rawdata_WVS <- subset(rawdata_WVS, select=-c(qualitychildch_w1lg))
#or if you want to delete variables from the values section
#rm(qualitychildhardwork_w1lg)

## clean up Environment
#(I don't even know whether it makes sense to do this, if not just # it out)
rm(raw_data_fraud)
rm(raw_data_nolabels)
rm(raw_data_tests)
rm(raw_data_nonames)
rm(autonomyindex_w1lg)
rm(countryaimfirstchoice_w1lg)
rm(countryaimsecondchoice_w1lg)
rm(id_to_remove)
rm(importancefirstchoice_w1lg)
rm(importancesecondchoice_w1lg)
rm(nochoicevschoice_w1lg)
rm(qualitychild.freq)
rm(qualitychilddetermination_w1lg)
rm(qualitychilddeterminationai_w1lg)
rm(qualitychildhardwork_w1lg)
rm(qualitychildimagination_w1lg)
rm(qualitychildindependence_w1lg)
rm(qualitychildindependenceai_w1lg)
rm(qualitychildobedience_w1lg)
rm(qualitychildobedienceai_w1lg)
rm(qualitychildreligious_w1lg)
rm(qualitychildreligiousai_w1lg)
rm(qualitychildresponsibility_w1lg)
rm(qualitychildselfexpression_w1lg)
rm(qualitychildthrift_w1lg)
rm(qualitychildtolerance_w1lg)
rm(qualitychildunselfishness_w1lg)
rm(respondentaimfirstchoice_w1lg)
rm(respondentaimsecondchoice_w1lg)
rm(sat_index)

#save cleaned datasets to easily load them for working with them the next time
saveRDS(raw_data, file="raw_data.RDS")
saveRDS(rawdata_WVS, file="rawdata_WVS.RDS")

#next time open them with
#load raw-data
#raw_data <- readRDS("raw_data.RDS")
#rawdata_WVS <- readRDS("rawdata_WVS.RDS)