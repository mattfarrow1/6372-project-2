# Bank Marketing Data Set

# https://archive.ics.uci.edu/ml/datasets/bank%20marketing#
#######################################

# Setup -------------------------------------------------------------------

library(tidyverse)
library(here)       # relative location references
library(janitor)    # data cleanup tools
library(naniar)     # dealing with missing values

# Load Data ---------------------------------------------------------------

# Read data
bank <- read_delim(here("Downloads", "bank-additional", "bank-additional-full.csv"), 
                   ";", escape_double = FALSE, trim_ws = TRUE)

# Clean column names
bank <- clean_names(bank)



# Deal with Missing Values ------------------------------------------------

# Convert "unknown" to NA
bank <- bank %>% 
  replace_with_na_all(condition = ~ .x == "unknown")

# Convert 999 in pdays to NA
# 999 means client was not previously contacted
bank <- bank %>% 
  replace_with_na(replace = list(pdays = 999))

# use DF to convert char to factors
bank<-as.data.frame(unclass(bank))

# relevel factors
sizes <- ordered(sizes, levels = c("small", "medium", "large"))
bank$day_of_week <- ordered(bank$day_of_week, levels = c("mon", "tue", "wed", "thu", "fri"))
bank$month <- ordered(bank$month, levels = c("mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

# Count number of missing values in data set
colSums(is.na(bank))

# Other variables that may have missing values that we should explore
unique(bank$poutcome)

#create new variables

bank$pdays_missing<-"No"
bank$pdays_missing[is.na(bank$pdays)]<-"Yes"
bank$pdays_missing<-as.factor(bank$pdays_missing)

# Examine Data ------------------------------------------------------------

# What are the dimensions of the data
dim(bank)

# Look at the first and last rows of data
head(bank)
tail(bank)
glimpse(bank)

# Summary statistics
summary(bank)
skimr::skim(bank)

# Other possible functions for examining the data that may be helpful
Hmisc::describe(bank_clean)
psych::describe(bank_clean)
DataExplorer::create_report(bank)



# Examine Numerical Data points

numericals<-seq(1,length(bank))[unlist(lapply(bank, is.numeric))]

#correlation plot for numericals
library("corrplot")
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(bank[,sapply(bank, is.numeric)])

# scatter plot matrix
library(GGally)
ggpairs(bank, columns = numericals, ggplot2::aes(colour=y))

#box plots
b1<-bank %>% ggplot(aes(x=y, y=age)) + geom_boxplot(aes(fill=y)) + theme(legend.position = "none")
b2<-bank %>% ggplot(aes(x=y,y=duration)) + geom_boxplot(aes(fill=y)) + theme(legend.position = "none")
b3<-bank %>% ggplot(aes(x=y,y=campaign)) + geom_boxplot(aes(fill=y)) + theme(legend.position = "none")
b4<-bank %>% ggplot(aes(x=y,y=pdays)) + geom_boxplot(aes(fill=y)) + theme(legend.position = "none")
b5<-bank %>% ggplot(aes(x=y,y=previous)) + geom_boxplot(aes(fill=y)) + theme(legend.position = "none")
b6<-bank %>% ggplot(aes(x=y,y=emp_var_rate)) + geom_boxplot(aes(fill=y)) + theme(legend.position = "none")
b7<-bank %>% ggplot(aes(x=y,y=cons_conf_idx)) + geom_boxplot(aes(fill=y)) + theme(legend.position = "none")
b8<-bank %>% ggplot(aes(x=y,y=cons_price_idx)) + geom_boxplot(aes(fill=y)) + theme(legend.position = "none")
b9<-bank %>% ggplot(aes(x=y,y=nr_employed)) + geom_boxplot(aes(fill=y)) + theme(legend.position = "none")

library(gridExtra)
grid.arrange(b1,b2,b3,b4,b5, b6, b7, b8, b9)

#Bar charts

c1<-bank %>% ggplot(aes(x=job, fill=y)) + geom_bar() + theme(legend.position = "none")
c2<-bank %>% ggplot(aes(x=marital, fill=y)) + geom_bar() + theme(legend.position = "none")
c3<-bank %>% ggplot(aes(x=education, fill=y)) + geom_bar() + theme(legend.position = "none")
c4<-bank %>% ggplot(aes(x=default, fill=y)) + geom_bar() + theme(legend.position = "none")
c5<-bank %>% ggplot(aes(x=housing, fill=y)) + geom_bar() + theme(legend.position = "none")
c6<-bank %>% ggplot(aes(x=loan, fill=y)) + geom_bar() + theme(legend.position = "none")
c7<-bank %>% ggplot(aes(x=contact, fill=y)) + geom_bar() + theme(legend.position = "none")
c8<-bank %>% ggplot(aes(x=month, fill=y)) + geom_bar() + theme(legend.position = "none")
c9<-bank %>% ggplot(aes(x=day_of_week, fill=y)) + geom_bar() + theme(legend.position = "none")
c10<-bank %>% ggplot(aes(x=poutcome, fill=y)) + geom_bar() + theme(legend.position = "none")
c11<-bank %>% ggplot(aes(x=pdays_missing, fill=y)) + geom_bar() + theme(legend.position = "none")


grid.arrange(c1,c2,c3,c4,c5, c6, c7, c8, c9, c10)



############################# models
library(caTools)
set.seed(123)
split = sample.split(bank$y, SplitRatio=0.60)
train_data<-subset(bank, split == TRUE)
test_data<-subset(bank, split == FALSE)

#logistic regression model 1
model1<- glm(y ~ month+day_of_week+campaign+emp_var_rate+contact+poutcome+cons_conf_idx  , data=train_data, family=binomial)
summary(model1) 

predictTest<- predict(model1, type="response", newdata=predict_data)
summary(predictTest)
table(predict_data$y, predictTest>0.20) #return confusion matrix. repeat for various threshold values

library(ROCR)
#Use this code chunck to find ROC curve and calculate AUC
ROCRpred<-prediction(predictTest, test_data$y)
as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf<-performance(ROCRpred, "tpr","fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.005), text.adj=c(-0.2,1.7))


#Build and analyze regression tree
library(rpart)
library(rpart.plot)
cart1<-rpart(y ~ job+default+education+housing+loan+contact+poutcome, data=train_data, control=rpart.control(minsplit=1, minbucket=20, cp=0.001))
predictioncart<-predict(cart1, newdata=predict_data)
table(predict_data$y,predictioncart>0.20) #return confusion matrix. repeat for various threshold values
summary(cart1)
prp(cart1)







