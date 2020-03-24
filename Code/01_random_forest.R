### Load the data

source("03_load_data.R")
uci.bank$job <- as.factor(uci.bank$job)
uci.bank$marital <- as.factor(uci.bank$marital)
uci.bank$education <- as.factor(uci.bank$education)
uci.bank$default <- as.factor(uci.bank$default)
uci.bank$housing <- as.factor(uci.bank$housing)
uci.bank$loan <- as.factor(uci.bank$loan)
uci.bank$contact <- as.factor(uci.bank$contact)
uci.bank$month <- as.factor(uci.bank$month)
uci.bank$day_of_week <- as.factor(uci.bank$day_of_week)
uci.bank$poutcome <- as.factor(uci.bank$poutcome)
uci.bank$y <- as.factor(uci.bank$y)

library("skimr")

skim(uci.bank)
summary(uci.bank)

DataExplorer::plot_bar(uci.bank, ncol = 2)
DataExplorer::plot_histogram(uci.bank, ncol = 3)
DataExplorer::plot_boxplot(uci.bank, by = "y", ncol = 2)

library(ggplot2)
library(gridExtra)

g1 = ggplot(uci.bank, aes(x= poutcome,fill= y ))+geom_bar(alpha = 0.5,position = "fill")
g2 = ggplot(uci.bank, aes(x= contact,fill= y ))+geom_bar(alpha = 0.5,position = "fill")
grid.arrange(g1,g2,ncol = 2, nrow = 1)

### address missingness
bank <- uci.bank
# replace unknow with NA
bank[bank=="unknown"] = NA
sapply(bank, function(x) sum(is.na(x)))

# look into the missingness
library(VIM)
aggr(bank,prop=FALSE,number=TRUE)

# delete unknown rows in "job" and "marital"
library(dplyr)
bank <- filter(bank,is.na(job)==0)
bank <- filter(bank,is.na(marital)==0)
sapply(bank, function(x) sum(is.na(x)))

# build a model to predict the missing values for "education", "default", "housing" and "loan"
library(rpart)
education.model<-rpart(education ~ .,data = bank[-which(is.na(bank$education)==1),][,-21],
                       na.action = na.omit,method = "class")
education.pred<-predict(education.model,bank[which(is.na(bank$education)==1),][-21],type="class")
bank[which(is.na(bank$education)==1),][,4]<-education.pred

default.model<-rpart(default ~ .,data = bank[-which(is.na(bank$default)==1),][,-21],
                     na.action = na.omit,method = "class")
default.pred<-predict(default.model,bank[which(is.na(bank$default)==1),][-21],type="class")
bank[which(is.na(bank$default)==1),][,5]<-default.pred

housing.model<-rpart(housing ~ .,data = bank[-which(is.na(bank$housing)==1),][,-21],
                     na.action = na.omit,method = "class")
housing.pred<-predict(housing.model,bank[which(is.na(bank$housing)==1),][-21],type="class")
bank[which(is.na(bank$housing)==1),][,6]<-housing.pred

loan.model<-rpart(loan ~ .,data = bank[-which(is.na(bank$loan)==1),][,-21],
                  na.action = na.omit,method = "class")
loan.pred<-predict(loan.model,bank[which(is.na(bank$loan)==1),][-21],type="class")
bank[which(is.na(bank$loan)==1),][,7]<-loan.pred

sapply(bank, function(x) sum(is.na(x)))

DataExplorer::plot_bar(bank, ncol = 2)

summary(bank)

### data coding and feature engineering
# binary coding for "y", "default", "housing","loan"
bank$default <- ifelse(bank$default=='yes',1,0)
bank$default <- ifelse(bank$default=='yes',1,0)
bank$housing <- ifelse(bank$housing=='yes',1,0)
bank$loan <- ifelse(bank$loan=='yes',1,0)

# ordinal coding for "education"
bank$education<-factor(bank$education,level=c("illiterate", "basic.4y", "basic.6y", "basic.9y", "high.school","professional.course", "university.degree"),labels = c(1:7))

# one-hot coding for "job", "marital", "contact", "month", "day_of_week", "poutcome"
library(data.table)
library(mltools)
sub.bank.onehot <- subset(bank, select=c("job", "marital", "contact", "month", "day_of_week", "poutcome"))
sub.bank.onehot <-  one_hot(as.data.table(sub.bank.onehot))
sub.bank.onehot <- sub.bank.onehot[, -c(12, 16)]
bank <- bank[,-c(2,3,8,9,10,15)]
bank <- cbind(bank,sub.bank.onehot)

names(bank)[16] <- "job_admin"
names(bank)[17] <- "job_blue_collar"
names(bank)[22]<- "job_self_employed"

### train/test/validate split
library(caTools)
set.seed(123)
spec = c(train = 0.5, test = 0.25, validate = 0.25)
g = sample(cut(seq(nrow(bank)), nrow(bank)*cumsum(c(0,spec)),labels = names(spec)))
sp = split(bank, g)

bank.train = sp$train
bank.test = sp$test
bank.validate = sp$validate

# scaling
bank.train[c(1,6,7,8,9,10,11,12,13,14)] = scale(bank.train[c(1,6,7,8,9,10,11,12,13,14)])
bank.test[c(1,6,7,8,9,10,11,12,13,14)] = scale(bank.test[c(1,6,7,8,9,10,11,12,13,14)])
bank.validate[c(1,6,7,8,9,10,11,12,13,14)] = scale(bank.validate[c(1,6,7,8,9,10,11,12,13,14)])

# confusion matrics
model.performance<-function(table,n=4){
  tn = table[1,1]
  fp = table[2,1]
  fn = table[1,2]
  tp = table[2,2]
  TPR = tp / ( tp + fn )
  TNR = tn / ( tn + fp )
  FDR = fp / ( tp + fp )
  FOR = fn / ( tn + fn )
  ACC = (tp + tn)/(tp + tn + fp + fn)
  result<- paste("True positive rate = ",round(TPR,n),
                 "\nTrue negative rate = ",round(TNR,n),
                 "\nFalse discovery rate = ",round(FDR,n),
                 "\nFalse omission rate = ",round(FOR,n),
                 "\nAccuracy = ",round(ACC,n),
                 "\n",sep="")
  cat(result)
}

### Logisitic Regression
set.seed(123)
fit.logit<-glm(y~.,data = bank.train,family = binomial())
summary(fit.logit)

# validate
logit.prob<- predict(fit.logit,bank.validate,type = "response")
logit.predict.label<-factor(logit.prob<.5,levels = c(TRUE,FALSE),
                            labels = c("no","yes"))
logit.matrix<-table(bank.validate$y,logit.predict.label,
                    dnn = c("Actual","Predicted"))



# Accuracy
logit.matrix
model.performance(logit.matrix)

# ROC
library(caret)
library(ROCR)
roc.bank.lgr.pred = prediction(predictions = logit.prob, labels = bank.validate$y)
roc.perf.lgr = performance(roc.bank.lgr.pred, measure = "tpr", x.measure = "fpr")
roc.auc.lgr = as.numeric(performance(roc.bank.lgr.pred, "auc")@y.values)

plot(roc.perf.lgr,
     lwd = 3, colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1),
     main = 'ROC Curve (Logistic Regression)', sub =paste(' AUC : ', round(roc.auc.lgr, 5)))
abline(0, 1, col = "red", lty = 2)

# AUC
roc.auc.lgr

### Decision Tree
set.seed(123)
dtree<-rpart(y ~.,data = bank.train,method = "class",
             parms = list(split="information"))
dtree$cptable
plotcp(dtree)
dtree.pruned<-prune(dtree,cp=.01)
library(rpart.plot)
prp(dtree.pruned,type = 2,extra = 104,
    fallen.leaves = TRUE,main="Decision Tree")
dtree.pred<-predict(dtree.pruned,bank.validate,type="class")
dtree.pred.roc <-predict(dtree.pruned,bank.validate,type="prob")

# validate
dtree.perf<-table(bank.validate$y,dtree.pred,dnn=c("Actual","Predicted"))

# Accuracy
dtree.perf
model.performance(dtree.perf)

# ROC
roc.bank.dl.pred = prediction(predictions = dtree.pred.roc[,2], labels = bank.validate$y)
roc.perf.dl = performance(roc.bank.dl.pred, measure = "tpr", x.measure = "fpr")
roc.auc.dl = as.numeric(performance(roc.bank.dl.pred, "auc")@y.values)

plot(roc.perf.dl,
     lwd = 3, colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1),
     main = 'ROC Curve (Decision Tree)', sub =paste(' AUC : ', round(roc.auc.dl, 5)))
abline(0, 1, col = "red", lty = 2)

# AUC
roc.auc.dl

### bagging
library(ipred)
set.seed(123)

# train bagged model
fit.bag <- bagging(y ~ .,data = bank.train,coob = TRUE,control = rpart.control(minsplit = 2, cp = 0))

fit.bag

# validate
bag.pred<-predict(fit.bag,bank.validate)
bag.perf<-table(bank.validate$y,bag.pred,
                dnn=c("Actual","Predicted"))
bag.pred<-predict(fit.bag,bank.validate,type="prob")

# Accuracy
bag.perf
model.performance(bag.perf)

# ROC
roc.bank.bag.pred = prediction(predictions = bag.pred[,2], labels = bank.validate$y)
roc.perf.bag = performance(roc.bank.bag.pred, measure = "tpr", x.measure = "fpr")
roc.auc.bag = as.numeric(performance(roc.bank.bag.pred, "auc")@y.values)

plot(roc.perf.bag,
     lwd = 3, colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1),
     main = 'ROC Curve (Bagging)', sub =paste(' AUC : ', round(roc.auc.bag, 5)))
abline(0, 1, col = "red", lty = 2)

# AUC
roc.auc.bag

### Random forest
library(randomForest)
set.seed(123)
n_features <- length(setdiff(names(bank.validate), "y"))
fit.forest=randomForest(y~.,data=bank.train,importance=TRUE,ntrees=n_features*10)

fit.forest
# importance
importance(fit.forest,type=2)
varImpPlot (fit.forest,cex=0.6,main = "Importance rank")

# validate
forest.pred<-predict(fit.forest,bank.validate)
forest.perf<-table(bank.validate$y,forest.pred,
                   dnn=c("Actual","Predicted"))
forest.pred.roc<-predict(fit.forest,bank.validate,type="prob")

# Accuracy
forest.perf
model.performance(forest.perf)

# ROC
roc.bank.rf.pred = prediction(predictions = forest.pred.roc[,2], labels = bank.validate$y)
roc.perf.rf = performance(roc.bank.rf.pred, measure = "tpr", x.measure = "fpr")
roc.auc.rf = as.numeric(performance(roc.bank.rf.pred, "auc")@y.values)

plot(roc.perf.rf,
     lwd = 3, colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1),
     main = 'ROC Curve (Random Forest)', sub =paste(' AUC : ', round(roc.auc.rf, 5)))
abline(0, 1, col = "red", lty = 2)

# AUC
roc.auc.rf

### Random forest Test
# performance on test set
forest.pred.test<-predict(fit.forest,bank.test)
forest.perf<-table(bank.test$y,forest.pred,
                   dnn=c("Actual","Predicted"))
forest.pred.roc<-predict(fit.forest,bank.test,type="prob")

# Accuracy
forest.perf
model.performance(forest.perf)

# ROC
roc.bank.rf.pred = prediction(predictions = forest.pred.roc[,2], labels = bank.test$y)
roc.perf.rf = performance(roc.bank.rf.pred, measure = "tpr", x.measure = "fpr")
roc.auc.rf = as.numeric(performance(roc.bank.rf.pred, "auc")@y.values)

roc.lgr<-plot(roc.perf.rf,
              lwd = 3, colorize = TRUE,
              print.cutoffs.at = seq(0, 1, by = 0.1),
              main = 'ROC Curve (Random Forest)', sub =paste(' AUC : ', round(roc.auc.rf, 5)))
abline(0, 1, col = "red", lty = 2)

# AUC
roc.auc.rf

# Calibration Curve (Random Forest)
library(rpart)
library(gbm)
calibrate.plot(bank.test$y,forest.pred.roc[,2], xlim=c(0,0.6), ylim=c(0,0.6),main="Calibration Curve (Random Forest)")

# comparison
TPR = c(0.6728, 0.6516, 0.558,0.6967)
TNR = c(0.9283, 0.9337, 0.9405, 0.931)
FDR = c(0.5877, 0.5376, 0.4676, 0.5653)
FOR = c(0.0257, 0.0316, 0.054, 0.0242)
ACC = c(0.9106, 0.911, 0.8991, 0.9144)
AUC = c(0.9339502, 0.8774277, 0.9235334, 0.9454665)
par(xpd=TRUE)
barplot(rbind(TPR,TNR,FDR,FOR,ACC,AUC), beside=TRUE, names.arg=c("Logistic regression","Decision tree","Bagging","Random forest"),legend.text  = c("True positive rate", "True negative rate", "False discovery rate", "False omission rate", "Accuracy", "AUC"), main = "Model Comparison",cex.names = 0.8)


barplot(rbind(TPR,TNR,FDR,FOR,ACC,AUC), beside=TRUE, names.arg=c("Logistic regression","Decision tree","Bagging","Random forest"), main = "Model Comparison",cex.names = 0.8,ylim=c(0,1))
legend(18,6,c("True positive rate", "True negative rate", "False discovery rate", "False omission rate", "Accuracy", "AUC"))

if(FALSE){
library(h2o)
n_features <- length(setdiff(names(bank.validate), "y"))

h2o.no_progress()
h2o.init(max_mem_size = "5g")
train_h2o <- as.h2o(bank.train)
response <- "y"
predictors <- setdiff(colnames(bank.validate), response)

h2o_rf1 <- h2o.randomForest(
  x = predictors,
  y = response,
  training_frame = train_h2o,
  ntrees = n_features * 10,
  seed = 123
)

h2o_rf1

# hyperparameter grid
hyper_grid <- list(
  mtries = floor(n_features * c(.05, .15, .25, .333, .4)),
  min_rows = c(1, 3, 5, 10),
  max_depth = c(10, 20, 30),
  sample_rate = c(.55, .632, .70, .80)
)

# random grid search strategy
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.001,   # stop if improvement is < 0.1%
  stopping_rounds = 10,         # over the last 10 models
  max_runtime_secs = 60*5      # or stop search after 5 min.
)

# perform grid search
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_random_grid",
  x = predictors,
  y = response,
  training_frame = train_h2o,
  hyper_params = hyper_grid,
  ntrees = n_features * 10,
  seed = 123,
  stopping_metric = "RMSE",
  stopping_rounds = 10,           # stop if last 10 trees added
  stopping_tolerance = 0.005,     # don't improve RMSE by 0.5%
  search_criteria = search_criteria
)

random_grid_perf <- h2o.getGrid(
  grid_id = "rf_random_grid",
  sort_by = "mse",
  decreasing = FALSE
)
random_grid_perf
}
# best parameters
set.seed(123)
fit.forest=randomForest(y~.,data=bank.train,importance=TRUE,ntree=n_features * 10,mtry=19, maxnodes=10,nodesize=10,sampsize = 0.632)

fit.forest
# importance
#importance(fit.forest,type=2)
varImpPlot (fit.forest,cex=0.6 )

# validate
forest.pred<-predict(fit.forest,bank.validate)
forest.perf<-table(bank.validate$y,forest.pred,
                   dnn=c("Actual","Predicted"))
forest.pred.roc<-predict(fit.forest,bank.validate,type="prob")

# Accuracy
forest.perf
model.performance(forest.perf)

# ROC
roc.bank.rf.pred = prediction(predictions = forest.pred.roc[,2], labels = bank.validate$y)
roc.perf.rf = performance(roc.bank.rf.pred, measure = "tpr", x.measure = "fpr")
roc.auc.rf = as.numeric(performance(roc.bank.rf.pred, "auc")@y.values)

plot(roc.perf.rf,
     lwd = 3, colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1),
     main = 'ROC Curve (Random Forest)', sub =paste(' AUC : ', round(roc.auc.rf, 5)))
abline(0, 1, col = "red", lty = 2)

# AUC
roc.auc.rf
