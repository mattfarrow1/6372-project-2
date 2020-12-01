# Tree and Random Forest model 

# Read in Data (locally)
bank_clean<-read.csv("/Users/molheiser/Downloads/bank_clean.csv")
bank_ds<-read.csv("/Users/molheiser/Downloads/bank_clean_downsampled.csv")
bank_train<-read.csv("/Users/molheiser/Downloads/bank_clean_train.csv")
bank_test<-read.csv("/Users/molheiser/Downloads/bank_clean_test.csv")
bank_train_ds<-read.csv("/Users/molheiser/Downloads/bank_clean_ds_train.csv")
bank_test_ds<-read.csv("/Users/molheiser/Downloads/bank_clean_ds_test.csv")


### Decision Tree model
library(tree)
mytree<-tree(subscribed~job+marital+day_of_week+housing+duration+education+contact+month+campaign+emp_var_rate+euribor3m+nr_employed+
               cons_price_idx+cons_conf_idx,bank_ds)

summary(mytree)
plot(mytree, main="Decision Tree For Subscribed")
text(mytree,pretty=0)

# CV for Pruning tree
set.seed(123)
cv.tree=cv.tree(mytree,FUN=prune.tree,method="deviance")
names(cv.tree)
cv.tree
plot(cv.tree)
par(mfrow=c(1,1))
plot(cv.tree$size,cv.tree$dev,type="b")

# The result is essnetially no pruning. 
pruned.tree=prune.tree(mytree,best=9)
plot(pruned.tree)
text(pruned.tree,pretty=0)


## test train split for tree model above
train.tree<-tree(subscribed~job+marital+day_of_week+housing+education+contact+month+duration+campaign+emp_var_rate+euribor3m+nr_employed
                 +cons_price_idx+cons_conf_idx,bank_train_ds)
pruned.train.tree=prune.tree(train.tree,best=9)
summary(pruned.train.tree)
plot(pruned.train.tree)
text(pruned.train.tree,pretty=0, cex=1)

#Make predictions on testing set, calculate perforance metrics
tree.pred<-predict(pruned.train.tree,bank_test_ds,type="class")
cm.tree.pruned<-table(tree.pred, bank_test_ds$subscribed)
#sens (recall) = TP / TP + FN
#Spec = TN / TN + FP
# accuracy = TP + TN / total
sensitivity_tree1<-cm.tree.pruned[2,2] / (cm.tree.pruned[2,2] + cm.tree.pruned[1,2])
specificity_tree1<-cm.tree.pruned[1,1] / (cm.tree.pruned[1,1] + cm.tree.pruned[2,1])
accuracy_tree1<-(cm.tree.pruned[2,2] + cm.tree.pruned[1,1]) / length(bank_test_ds$subscribed)

# ROC curve for single Decision tree
library(ROCR)

tree.pred=predict(pruned.train.tree,bank_test_ds,type="vector")
pred <- prediction(tree.pred[,2], bank_test_ds$subscribed)
roc.perf.tree = performance(pred, measure = "tpr", x.measure = "fpr")
auc.tree <- performance(pred, measure = "auc")
auc.tree <- auc.tree@y.values
plot(roc.perf.tree,main="ROC of Single Tree for Subscribed", xlab="1-Specificity", ylab="Sensitivity")
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.tree[[1]],3), sep = ""))


# Random Forest
library(randomForest)

#model removing poutcome and pdays
rf.bank<-randomForest(subscribed~.-subscribed-pdays-poutcome,bank_train_ds,mtry=5,importance=T,ntree=100)

#Making predictions on test and then observing performnace metrics
fit.pred<-predict(rf.bank,newdata=bank_test_ds,type="response")
cm.rf<-table(fit.pred,bank_test_ds$subscribed) 
sensitivity_rf1<-cm.rf[2,2] / (cm.rf[2,2] + cm.rf[1,2])
specificity_rf1<-cm.rf[1,1] / (cm.rf[1,1] + cm.rf[2,1])
accuracy_rf1<-(cm.rf[2,2] + cm.rf[1,1]) / length(bank_test_ds$subscribed)

#Plot ROC
rf.pred<-predict(rf.bank,newdata=bank_test_ds,type="prob")
pred <- prediction(rf.pred[,2], bank_test_ds$subscribed)
roc.perf.rf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.rf <- performance(pred, measure = "auc")
auc.rf <- auc.rf@y.values
plot(roc.perf.rf,main="ROC of Random Forest of for Subscribed - mtry=5", xlab="1-Specificity", ylab="Sensitivity")
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.rf[[1]],3), sep = ""))


#Which variables are important.  We can use variable importance.
# 1=mean decrease in accuracy, 2=mean decrease in node impurity
varImpPlot (rf.bank,type=1,main="Variable Importance")
varImpPlot (rf.bank,type=2,main="Variable Importance")

#compare models
model_results<-data.frame(Model = c("Tree", "Random Forest"), 
                          Specificity=c(round(specificity_tree1,3), round(specificity_rf1,3)), 
                          Sensitivity = c(round(sensitivity_tree1,3), round(sensitivity_rf1,3)), 
                          Accuracy =  c(round(accuracy_tree1,3), round(accuracy_rf1,3)),
                          AUC = c(round(auc.tree[[1]],3),round(auc.rf[[1]],3))
)
