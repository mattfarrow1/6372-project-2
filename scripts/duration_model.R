# linear regression for Duration

#### read in data from Matts Github (downloaded locally because of git issues on work computer)
bank_clean<-read.csv("/Users/molheiser/Downloads/bank_clean.csv")
bank_train<-read.csv("/Users/molheiser/Downloads/bank_clean_train.csv")
bank_test<-read.csv("/Users/molheiser/Downloads/bank_clean_test.csv")

# Take log transform of Duration due to skewness
bank_clean$duration[bank_clean$duration==0]<-0.1
bank_train$duration[bank_train$duration==0]<-0.1
bank_test$duration[bank_test$duration==0]<-0.1
bank_clean$log_duration<-log(bank_clean$duration)

#bit of EDA

library(tidyverse)
b1<-bank_clean %>% ggplot(aes(x=job, y=log_duration)) + geom_boxplot() + theme(legend.position = "none")
b2<-bank_clean %>% ggplot(aes(x=marital,y=log_duration)) + geom_boxplot() + theme(legend.position = "none")
b3<-bank_clean %>% ggplot(aes(x=education,y=log_duration)) + geom_boxplot() + theme(legend.position = "none")
b4<-bank_clean %>% ggplot(aes(x=housing,y=log_duration)) + geom_boxplot() + theme(legend.position = "none")
b5<-bank_clean %>% ggplot(aes(x=loan,y=log_duration)) + geom_boxplot() + theme(legend.position = "none")
b6<-bank_clean %>% ggplot(aes(x=contact,y=log_duration)) + geom_boxplot() + theme(legend.position = "none")
b7<-bank_clean %>% ggplot(aes(x=month,y=log_duration)) + geom_boxplot() + theme(legend.position = "none")
b8<-bank_clean %>% ggplot(aes(x=day_of_week,y=log_duration)) + geom_boxplot() + theme(legend.position = "none")
b9<-bank_clean %>% ggplot(aes(x=as.factor(previous),y=log_duration)) + geom_boxplot() + theme(legend.position = "none")

library(gridExtra)
grid.arrange(b1,b2,b3,b4,b5, b6, b7, b8, b9)

numericals<-seq(1,length(bank_clean))[unlist(lapply(bank_clean, is.numeric))]

#correlation plot for numericals
library("corrplot")
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(bank_clean[,sapply(bank_clean, is.numeric)])
library(GGally)
ggpairs(bank_clean, columns = numericals)

### Modeling ##########

# Fwd, Bwd, Step selection kept same variables
# Candidate model
duration_lm_1<-lm(log_duration ~ education+age+job+marital+education+housing+
                    loan+contact+day_of_week+campaign+previous+month,data = bank_clean)

#stepwise selection
library(olsrr)
step_model<-ols_step_forward_aic(duration_lm_1, details = FALSE)
step_model
# fit final model after stepwise selection
duration_lm_1.fit<-lm(log(duration) ~ education+campaign+month+day_of_week+housing+previous+job+loan+contact+age,data = bank_train)
summary(duration_lm_1.fit)

# predict
step_pred<-predict(duration_lm_1.fit, newdata=bank_test)
AIC(duration_lm_1.fit)
mean((step_pred - log(bank_test$duration))^2, na.rm=TRUE)

#residual diagnostics
par(mfrow=c(2,2))
plot(duration_lm_1.fit)

#use model to predict duration for entire data set and export
duration_lm_1.fit<-lm(log(duration) ~ campaign+month+day_of_week+housing+previous+job+loan+contact+age,data = bank_clean)
predicted_log_duration<-predict(duration_lm_1.fit, newdata=bank_clean)
Duration_Predicted<-merge(bank_clean, predicted_log_duration, by="row.names")
colnames(Duration_Predicted)[22]<-"predicted_log_duration"
Duration_Predicted$predicted_duration<-exp(Duration_Predicted$predicted_log_duration)
write.csv(Duration_Predicted,"/Users/molheiser/Downloads/Duration_Predicted.csv")