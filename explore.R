
play.train <- train;
play.validation <- validation;
play.test <- test;


f1 <- function (data, lev = NULL, model = NULL) {
  precision <- posPredValue(data$pred, data$obs, positive = "yes")
  recall  <- sensitivity(data$pred, data$obs, postive = "yes")
  f1_val <- (2 * precision * recall) / (precision + recall)
  names(f1_val) <- c("F1")
  f1_val
}

#_________________________

play.train[,"SALEDATE"] <- as.Date(play.train[,"SALEDATE"])
play.validation[,"SALEDATE"] <- as.Date(play.validation[,"SALEDATE"])
play.test[,"SALEDATE"] <- as.Date(play.test[,"SALEDATE"])

play.attrs <- 'SALEDATE';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)
  
play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        na.action = na.omit,
        metric = "F1")
}


play.model <- play.my_train(my_method = 'rf');
#play.model.bak <- play.model
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.confusionMatrix <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw, positive = "yes")
cbind(Accuracy=play.confusionMatrix$overall[1], FScore = play.model$results$F1)
# as-is - accuracy: 66.89%, F1: 0.6366465

play.train[,"SALEDATE"] <- as.numeric(play.train[,"SALEDATE"])
play.validation[,"SALEDATE"] <- as.numeric(play.validation[,"SALEDATE"])
play.test[,"SALEDATE"] <- as.numeric(play.test[,"SALEDATE"])

set.seed(3433)
play.train$SALEDATE <- (optbin(play.train[,c("SALEDATE", "QUALIFIED")], na.omit = F))[,"SALEDATE"]
#Levels: (-2.56e+04,1.28e+04] (1.28e+04,1.78e+04]
play.validation$SALEDATE <- sapply(play.validation[,"SALEDATE"], function(x) factor(ifelse(is.na(x), "NA", ifelse(x<=1.55e+04,"(-2.56e+04,1.28e+04]","(1.28e+04,1.78e+04]")), levels=levels(play.train$SALEDATE)))
play.test$SALEDATE <- sapply(play.test[,"SALEDATE"], function(x) factor(ifelse(is.na(x), "NA", ifelse(x<=1.55e+04,"(-2.56e+04,1.28e+04]","(1.28e+04,1.78e+04]")), levels=levels(play.train$SALEDATE)))

play.model2 <- play.my_train(my_method = 'rf');
#play.model2.bak <- play.model2
play.predict.prob2 <- predict(play.model2, play.validation, type = "prob")
play.predict.raw2 <- predict(play.model2, play.validation, type = "raw")
play.confusionMatrix2 <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw2, positive = "yes")
cbind(Accuracy=play.confusionMatrix2$overall[1], FScore = play.model2$results$F1)
# transformed - accuracy: 65.28%, F1: 0.5579962

##Better to leave saledate as is!!

#_________________________

# EYB

#_________________________


play.train <- train;
play.validation <- validation;
play.test <- test;

f1 <- function (data, lev = NULL, model = NULL) {
  precision <- posPredValue(data$pred, data$obs, positive = "yes")
  recall  <- sensitivity(data$pred, data$obs, postive = "yes")
  f1_val <- (2 * precision * recall) / (precision + recall)
  names(f1_val) <- c("F1")
  f1_val
}


play.attrs <- 'EYB';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        na.action = na.omit,
        metric = "F1")
}


play.model <- play.my_train(my_method = 'rf');
#play.model.bak <- play.model
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.confusionMatrix <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw, positive = "yes")
cbind(Accuracy=play.confusionMatrix$overall[1], FScore = play.model$results$F1)
#Accuracy    FScore
#Accuracy 0.6758667 0.6615788

play.train[,"EYB"] <- sapply(play.train[,"EYB"],function(x) ifelse(x < 1750, 1900, x))
play.validation[,"EYB"] <- sapply(play.validation[,"EYB"],function(x) ifelse(x < 1750, 1900, x))
play.test[,"EYB"] <- sapply(play.test[,"EYB"],function(x) ifelse(x < 1750, 1900, x))

set.seed(3433)
play.train[,"EYB"] <- (optbin(play.train[,c("EYB", "QUALIFIED")], method="infogain", na.omit = F))[,"EYB"]
#Levels: "(1.9e+03,1.96e+03]"  "(1.96e+03,2.02e+03]"
play.validation[,"EYB"] <- sapply(play.validation[,"EYB"], function(x) factor(ifelse(is.na(x), "NA", ifelse(x<=1.96e+03,"(1.9e+03,1.96e+03]","(1.96e+03,2.02e+03]")), levels=levels(play.train$EYB)))
play.test[,"EYB"] <- sapply(play.test[,"EYB"], function(x) factor(ifelse(is.na(x), "NA", ifelse(x<=1.96e+03,"(1.9e+03,1.96e+03]","(1.96e+03,2.02e+03]")), levels=levels(play.train$EYB)))


play.model2 <- play.my_train(my_method = 'rf');
play.model2.bak <- play.model2
play.predict.prob2 <- predict(play.model2, play.validation, type = "prob")
play.predict.raw2 <- predict(play.model2, play.validation, type = "raw")
play.confusionMatrix2 <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw2, positive = "yes")
cbind(Accuracy=play.confusionMatrix2$overall[1], FScore = play.model2$results$F1)
#Accuracy    FScore
#0.6530667 0.5746924

##EYB - leave as is

#_________________________

# AYB

#_________________________

play.train <- train;
play.validation <- validation;
play.test <- test;

play.attrs <- 'AYB';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        na.action = na.omit,
        metric = "F1")
}


play.model <- play.my_train(my_method = 'rf');
#play.model.bak <- play.model
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.confusionMatrix <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw, positive = "yes")
cbind(Accuracy=play.confusionMatrix$overall[1], FScore = play.model$results$F1)
#Accuracy    FScore
#Accuracy   0.5904 0.7764366

set.seed(3433)
play.train[,"AYB"] <- (optbin(play.train[,c("AYB", "QUALIFIED")], method="infogain", na.omit = F))[,"AYB"]
#Levels: (1.75e+03,2.01e+03] (2.01e+03,2.02e+03]
play.validation[,"AYB"] <- sapply(play.validation[,"AYB"], function(x) factor(ifelse(is.na(x), "NA", ifelse(x<=2.01e+03,"(1.75e+03,2.01e+03]","(2.01e+03,2.02e+03]")), levels=levels(play.train$AYB)))
play.test[,"AYB"] <- sapply(play.test[,"AYB"], function(x) factor(ifelse(is.na(x), "NA", ifelse(x<=2.01e+03,"(1.75e+03,2.01e+03]","(2.01e+03,2.02e+03]")), levels=levels(play.train$AYB)))


play.model2 <- play.my_train(my_method = 'rf');
play.model2.bak <- play.model2
play.predict.prob2 <- predict(play.model2, play.validation, type = "prob")
play.predict.raw2 <- predict(play.model2, play.validation, type = "raw")
play.confusionMatrix2 <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw2, positive = "yes")
cbind(Accuracy=play.confusionMatrix2$overall[1], FScore = play.model2$results$F1)
#Accuracy    FScore
#Accuracy 0.5802667 0.8701871

##AYB - LEAVE AS IS!

#_________________________

# YR_RMDL

#Missing -> Median
#Missing -> decision tree
#Missing -> optbin tree
#_________________________

play.train <- train;
play.validation <- validation;
play.test <- test;

play.attrs <- 'YR_RMDL';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

# 1 - replace missing with median

play.train[is.na(play.train$YR_RMDL),"YR_RMDL"] <- median(play.train$YR_RMDL, na.rm = TRUE)
play.validation[is.na(play.validation$YR_RMDL),"YR_RMDL"] <- median(play.validation$YR_RMDL, na.rm = TRUE)
play.test[is.na(play.test$YR_RMDL),"YR_RMDL"] <- median(play.test$YR_RMDL, na.rm = TRUE)


play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        #na.action = na.omit,
        metric = "F1")
}


play.model <- play.my_train(my_method = 'rf');
play.model.bak <- play.model
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.confusionMatrix <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw, positive = "yes")
cbind(Accuracy=play.confusionMatrix$overall[1], FScore = play.model$results$F1)
#Accuracy    FScore
# 0.6686667 0.7137699

# 2 - optbin

play.train <- train;
play.validation <- validation;
play.test <- test;

set.seed(3433)
play.train[,"YR_RMDL"] <- (optbin(play.train[,c("YR_RMDL", "QUALIFIED")], na.omit = F))[,"YR_RMDL"]
#Levels: (1.88e+03,2e+03] (2e+03,2.02e+03] NA
play.validation[,"YR_RMDL"] <- sapply(play.validation[,"YR_RMDL"], function(x) factor(ifelse(is.na(x), "NA", ifelse(x<=2e+03,"(1.88e+03,2e+03]","(2e+03,2.02e+03]")), levels=levels(play.train$YR_RMDL)))
play.test[,"YR_RMDL"] <- sapply(play.test[,"YR_RMDL"], function(x) factor(ifelse(is.na(x), "NA", ifelse(x<=2e+03,"(1.88e+03,2e+03]","(2e+03,2.02e+03]")), levels=levels(play.train$YR_RMDL)))


play.model2 <- play.my_train(my_method = 'rf');
play.model2.bak <- play.model2
play.predict.prob2 <- predict(play.model2, play.validation, type = "prob")
play.predict.raw2 <- predict(play.model2, play.validation, type = "raw")
play.confusionMatrix2 <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw2, positive = "yes")
cbind(Accuracy=play.confusionMatrix2$overall[1], FScore = play.model2$results$F1)
#Accuracy    FScore
# 0.6650667 0.7241976


# 3 Impute by building tree

play.attrs <- 'YR_RMDL';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        #na.action = na.omit,
        metric = "F1")
}


play.train <- train;
play.validation <- validation;
play.test <- test;

play.train[is.na(play.train$YR_RMDL), "YR_RMDL"] <- 0;
play.validation[is.na(play.validation$YR_RMDL), "YR_RMDL"] <- 0;
play.test[is.na(play.test$YR_RMDL), "YR_RMDL"] <- 0;

play.sub_model <- train(QUALIFIED~YR_RMDL, 
                        data = play.train, 
                        method = "rpart",
                        #preProc = c("center", "scale"),
                        tuneLength=5,
                        trControl=trainControl(method = "cv", number = 10, verboseIter = TRUE),
)

#text(play.sub_model$finalModel)
#cut-points: 
play.sub_model$finalModel$splits[,4]
##1997.5

play.train$YR_RMDL <- sapply(play.train$YR_RMDL, function(x) factor(ifelse(x<=1997.5,"A","B"), levels=c("A", "B")))
play.validation$YR_RMDL <- sapply(play.validation$YR_RMDL, function(x) factor(ifelse(x<=1997.5,"A","B"), levels=c("A", "B")))
play.test$YR_RMDL <- sapply(play.test$YR_RMDL, function(x) factor(ifelse(x<=1997.5,"A","B"), levels=c("A", "B")))

set.seed(3433)
play.model3 <- play.my_train(my_method = 'rf');
play.model3.bak <- play.model3
play.predict.prob3 <- predict(play.model3, play.validation, type = "prob")
play.predict.raw3 <- predict(play.model3, play.validation, type = "raw")
play.confusionMatrix3 <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw3, positive = "yes")
play.acc3 <- cbind(Accuracy=play.confusionMatrix3$overall[1], FScore = play.model3$results$F1)
print(play.acc3)
#Accuracy    FScore
# 0.6686667 0.7158727

##YR_RMDL - TRANSFORM with optbin!

#_________________________

# AYB_TO_EYB

#_________________________

play.train <- train;
play.validation <- validation;
play.test <- test;

play.attrs <- 'AYB_TO_EYB';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        #na.action = na.omit,
        metric = "F1")
}


play.model <- play.my_train(my_method = 'rf');
play.model.bak <- play.model
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.confusionMatrix <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw, positive = "yes")
cbind(Accuracy=play.confusionMatrix$overall[1], FScore = play.model$results$F1)
#Accuracy    FScore
#Accuracy 0.5966667 0.6668178

set.seed(3433)
play.train[,"AYB_TO_EYB"] <- (optbin(play.train[,c("AYB_TO_EYB", "QUALIFIED")], na.omit = F))[,"AYB_TO_EYB"]
#Levels: (-55.3,36] (36,229]
play.validation[,"AYB_TO_EYB"] <- sapply(play.validation[,"AYB_TO_EYB"], function(x) factor(ifelse(is.na(x), "NA", ifelse(x<=36,"(-55.3,36]","(36,229]")), levels=levels(play.train$AYB_TO_EYB)))
play.test[,"AYB_TO_EYB"] <- sapply(play.test[,"AYB_TO_EYB"], function(x) factor(ifelse(is.na(x), "NA", ifelse(x<=36,"(-55.3,36]","(36,229]")), levels=levels(play.train$AYB_TO_EYB)))

play.model2 <- play.my_train(my_method = 'rf');
play.model2.bak <- play.model2
play.predict.prob2 <- predict(play.model2, play.validation, type = "prob")
play.predict.raw2 <- predict(play.model2, play.validation, type = "raw")
play.confusionMatrix2 <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw2, positive = "yes")
cbind(Accuracy=play.confusionMatrix2$overall[1], FScore = play.model2$results$F1)
#Accuracy   FScore
#Accuracy 0.5770667 0.575673

#AYB_TO_EYB - Don't transform!


#_________________________

# AYB_TO_YR_RMDL

#_________________________

#1 impute with -1
play.train <- train;
play.validation <- validation;
play.test <- test;

play.train[is.na(play.train$AYB_TO_YR_RMDL), "AYB_TO_YR_RMDL"] <- -1;
play.validation[is.na(play.validation$AYB_TO_YR_RMDL), "AYB_TO_YR_RMDL"] <- -1;
play.test[is.na(play.test$AYB_TO_YR_RMDL), "AYB_TO_YR_RMDL"] <- -1;

play.attrs <- 'AYB_TO_YR_RMDL';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        #na.action = na.omit,
        metric = "F1")
}


play.model <- play.my_train(my_method = 'rf');
play.model.bak <- play.model
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.confusionMatrix <- confusionMatrix(play.validation[!is.na(play.validation$AYB_TO_YR_RMDL),"QUALIFIED"], play.predict.raw, positive = "yes")
cbind(Accuracy=play.confusionMatrix$overall[1], FScore = play.model$results$F1)
#Accuracy    FScore
#0.6446667 0.6719668

# 2 impute with optbin

play.train <- train;
play.validation <- validation;
play.test <- test;

set.seed(3433)
play.train[,"AYB_TO_YR_RMDL"] <- (optbin(play.train[,c("AYB_TO_YR_RMDL", "QUALIFIED")], na.omit = F))[,"AYB_TO_YR_RMDL"]
#Levels: (-68.3,72.5] (72.5,258] NA
play.validation[,"AYB_TO_YR_RMDL"] <- sapply(play.validation[,"AYB_TO_YR_RMDL"], function(x) factor(ifelse(is.na(x), "NA", ifelse(x<=72.5,"(-68.3,72.5]","(72.5,258]")), levels=levels(play.train$AYB_TO_YR_RMDL)))
play.test[,"AYB_TO_YR_RMDL"] <- sapply(play.test[,"AYB_TO_YR_RMDL"], function(x) factor(ifelse(is.na(x), "NA", ifelse(x<=64,"(-68.3,72.5]","(72.5,258]")), levels=levels(play.train$AYB_TO_YR_RMDL)))

play.model2 <- play.my_train(my_method = 'rf');
play.model2.bak <- play.model2
play.predict.prob2 <- predict(play.model2, play.validation, type = "prob")
play.predict.raw2 <- predict(play.model2, play.validation, type = "raw")
play.confusionMatrix2 <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw2, positive = "yes")
cbind(Accuracy=play.confusionMatrix2$overall[1], FScore = play.model2$results$F1)
#Accuracy   FScore
#  0.6344 0.709833


# 3 Impute by building tree

play.train <- train;
play.validation <- validation;
play.test <- test;

play.train[is.na(play.train$AYB_TO_YR_RMDL), "AYB_TO_YR_RMDL"] <- -1;
play.validation[is.na(play.validation$AYB_TO_YR_RMDL), "AYB_TO_YR_RMDL"] <- -1;
play.test[is.na(play.test$AYB_TO_YR_RMDL), "AYB_TO_YR_RMDL"] <- -1;

play.attrs <- 'AYB_TO_YR_RMDL';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        #na.action = na.omit,
        metric = "F1")
}


play.sub_model <- train(QUALIFIED~AYB_TO_YR_RMDL, 
                        data = play.train, 
                        method = "rpart",
                        #preProc = c("center", "scale"),
                        tuneLength=10,
                        trControl=trainControl(method = "cv", number = 10, verboseIter = TRUE),
)

#text(play.sub_model$finalModel)
#cut-points: 
play.sub_model$finalModel$splits[,4]
##60.5

play.train$AYB_TO_YR_RMDL <- sapply(play.train$AYB_TO_YR_RMDL, function(x) factor(ifelse(x<=60.5,"A","B"), levels=c("A", "B")))
play.validation$AYB_TO_YR_RMDL <- sapply(play.validation$AYB_TO_YR_RMDL, function(x) factor(ifelse(x<=60.5,"A","B"), levels=c("A", "B")))
play.test$AYB_TO_YR_RMDL <- sapply(play.test$AYB_TO_YR_RMDL, function(x) factor(ifelse(x<=60.5,"A","B"), levels=c("A", "B")))

set.seed(3433)
play.model3 <- play.my_train(my_method = 'rf');
play.model3.bak <- play.model3
play.predict.prob3 <- predict(play.model3, play.validation, type = "prob")
play.predict.raw3 <- predict(play.model3, play.validation, type = "raw")
play.confusionMatrix3 <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw3, positive = "yes")
play.acc3 <- cbind(Accuracy=play.confusionMatrix3$overall[1], FScore = play.model3$results$F1)
print(play.acc3)
#Accuracy    FScore
#  0.6468 0.6718247

#AYB_TO_YR_RMDL - transform using optbin!


#_________________________

# [ALL DATES]

#_________________________

play.train <- train;
play.validation <- validation;
play.test <- test;

play.attrs <- 'AYB + YR_RMDL + EYB + SALEDATE + AYB_TO_EYB + AYB_TO_SALEDATE + AYB_TO_YR_RMDL + EYB_TO_YR_RMDL + EYB_TO_SALEDATE + YR_RMDL_TO_SALEDATE';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        na.action = na.omit,
        metric = "F1")
}


play.model <- play.my_train(my_method = 'rf');
play.model.bak <- play.model
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.confusionMatrix <- confusionMatrix(play.validation[!is.na(play.validation$AYB_TO_YR_RMDL),"QUALIFIED"], play.predict.raw, positive = "yes")
cbind(Accuracy=play.confusionMatrix$overall[1], FScore = play.model$results$F1)
#Chosen mtry = 2
#Accuracy    FScore
#0.7500622 0.7424736

#dt_GKtauDF <- GKtauDataframe(play.train[,c('AYB', 'YR_RMDL', 'EYB', 'SALEDATE', 'AYB_TO_EYB', 'AYB_TO_SALEDATE', 'AYB_TO_YR_RMDL', 'EYB_TO_YR_RMDL', 'EYB_TO_SALEDATE', 'YR_RMDL_TO_SALEDATE')])

play.attrs <- 'AYB + EYB + SALEDATE + AYB_TO_EYB + AYB_TO_SALEDATE + AYB_TO_YR_RMDL + EYB_TO_YR_RMDL + EYB_TO_SALEDATE';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        #na.action = na.omit,
        metric = "F1")
}

set.seed(3433)
play.model2 <- play.my_train(my_method = 'rf');
play.model2.bak <- play.model2
play.predict.prob2 <- predict(play.model2, play.validation, type = "prob")
play.predict.raw2 <- predict(play.model2, play.validation, type = "raw")
play.confusionMatrix2 <- confusionMatrix(play.validation[!is.na(play.validation$AYB_TO_YR_RMDL),"QUALIFIED"], play.predict.raw2, positive = "yes")
cbind(Accuracy=play.confusionMatrix2$overall[1], FScore = play.model2$results$F1)
#Accuracy    FScore
# 0.7612646 0.7195244

# Remove un-needed?



#_________________________

# NUMERIC

#_________________________

play.train <- train;
play.validation <- validation;
play.test <- test;

play.attrs <- 'BATHRM + HF_BATHRM + NUM_UNITS + ROOMS + BEDRM + STORIES + KITCHENS + FIREPLACES + GBA + LANDAREA';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        na.action = na.omit,
        metric = "F1")
}


set.seed(3433)
play.model <- play.my_train(my_method = 'rf');
play.model.bak <- play.model
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.confusionMatrix <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw, positive = "yes")
play.acc <- cbind(Accuracy=play.confusionMatrix$overall[1], FScore = play.model$results$F1)
print(play.acc)
#Chosen mtry = 2
#Accuracy    FScore
#0.6536 0.7105570

numeric_attrs_skew <- c("BATHRM", "HF_BATHRM", "NUM_UNITS", "ROOMS", "BEDRM", "STORIES", "KITCHENS", "FIREPLACES", "GBA", "LANDAREA")

play.train[,numeric_attrs_skew] = as.data.frame(lapply(play.train[,numeric_attrs_skew], function(x) ifelse(log10(x) == -Inf, 0, log10(x))))
play.validation[,numeric_attrs_skew] = as.data.frame(lapply(play.validation[,numeric_attrs_skew], function(x) ifelse(log10(x) == -Inf, 0, log10(x))))
play.test[,numeric_attrs_skew] = as.data.frame(lapply(play.test[,numeric_attrs_skew], function(x) ifelse(log10(x) == -Inf, 0, log10(x))))


set.seed(3433)
play.model2 <- play.my_train(my_method = 'rf');
play.model2.bak <- play.model2
play.predict.prob2 <- predict(play.model2, play.validation, type = "prob")
play.predict.raw2 <- predict(play.model2, play.validation, type = "raw")
play.confusionMatrix2 <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw2, positive = "yes")
play.acc2 <- cbind(Accuracy=play.confusionMatrix2$overall[1], FScore = play.model2$results$F1)
print(play.acc2)
#Accuracy    FScore
#0.6486667 0.7012513

# stay with original - slightly better


#_________________________

# CNTDN & GRADE

#_________________________

play.train <- train;
play.validation <- validation;
play.test <- test;

play.attrs <- 'CNDTN + GRADE';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        #na.action = na.omit,
        metric = "F1")
}


set.seed(3433)
play.model <- play.my_train(my_method = 'rf');
play.model.bak <- play.model
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.confusionMatrix <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw, positive = "yes")
play.acc <- cbind(Accuracy=play.confusionMatrix$overall[1], FScore = play.model$results$F1)
print(play.acc)
#Accuracy    FScore
#Accuracy 0.6670667 0.6645792

play.train[,ord_cat_attrs] <- sapply(play.train[,ord_cat_attrs],function(x) BBmisc::normalize(as.numeric(x), method = "range", range=c(0,1)))
play.validation[,ord_cat_attrs] <- sapply(play.validation[,ord_cat_attrs],function(x) BBmisc::normalize(as.numeric(x), method = "range", range=c(0,1)))
play.test[,ord_cat_attrs] <- sapply(play.test[,ord_cat_attrs],function(x) BBmisc::normalize(as.numeric(x), method = "range", range=c(0,1)))


set.seed(3433)
play.model2 <- play.my_train(my_method = 'rf');
play.model2.bak <- play.model2
play.predict.prob2 <- predict(play.model2, play.validation, type = "prob")
play.predict.raw2 <- predict(play.model2, play.validation, type = "raw")
play.confusionMatrix2 <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw2, positive = "yes")
play.acc2 <- cbind(Accuracy=play.confusionMatrix2$overall[1], FScore = play.model2$results$F1)
print(play.acc2)
#Accuracy    FScore
#0.6670667 0.6645792

# stay with original - slightly better


#_________________________

# PRICE

#_________________________

play.train <- train;
play.validation <- validation;
play.test <- test;

play.attrs <- 'PRICE';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        #na.action = na.omit,
        metric = "F1")
}

# 1 Impute with 0
play.train[is.na(play.train$PRICE), "PRICE"] <- 0;
play.validation[is.na(play.validation$PRICE), "PRICE"] <- 0;
play.test[is.na(play.test$PRICE), "PRICE"] <- 0;

set.seed(3433)
play.model <- play.my_train(my_method = 'rf');
play.model.bak <- play.model
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.confusionMatrix <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw, positive = "yes")
play.acc <- cbind(Accuracy=play.confusionMatrix$overall[1], FScore = play.model$results$F1)
print(play.acc)
#Accuracy    FScore
#Accuracy 0.8909333 0.8254867


play.train <- train;
play.validation <- validation;
play.test <- test;

# 2 Impute with median
play.train[is.na(play.train$PRICE), "PRICE"] <- median(play.train$PRICE, na.rm = TRUE);
play.validation[is.na(play.validation$PRICE), "PRICE"] <- median(play.validation$PRICE, na.rm = TRUE);
play.test[is.na(play.test$PRICE), "PRICE"]  <- median(play.test$PRICE, na.rm = TRUE);


set.seed(3433)
play.model2 <- play.my_train(my_method = 'rf');
play.model2.bak <- play.model2
play.predict.prob2 <- predict(play.model2, play.validation, type = "prob")
play.predict.raw2 <- predict(play.model2, play.validation, type = "raw")
play.confusionMatrix2 <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw2, positive = "yes")
play.acc2 <- cbind(Accuracy=play.confusionMatrix2$overall[1], FScore = play.model2$results$F1)
print(play.acc2)
#Accuracy    FScore
# 0.7082667 0.8255044


play.train <- train;
play.validation <- validation;
play.test <- test;

# 3 Impute by building tree

play.train[is.na(play.train$PRICE), "PRICE"] <- 0;
play.validation[is.na(play.validation$PRICE), "PRICE"] <- 0;
play.test[is.na(play.test$PRICE), "PRICE"] <- 0;

play.sub_model <- train(QUALIFIED~PRICE, 
                        data = play.train, 
                        method = "rpart",
                        #preProc = c("center", "scale"),
                        tuneLength=5,
                        trControl=trainControl(method = "cv", number = 10, verboseIter = TRUE),
)

#text(play.sub_model$finalModel)
#cut-points: 
play.sub_model$finalModel$splits[,4]
#64304.5 257992.0  82473.5 

play.train$PRICE <- sapply(play.train$PRICE, function(x) factor(ifelse(x<=64304.5,"A",ifelse(x<=82473.5,"B",ifelse(x<=257992.0,"C","D"))), levels=c("A", "B", "C", "D")))
play.validation$PRICE <- sapply(play.validation$PRICE, function(x) factor(ifelse(x<=64304.5,"A",ifelse(x<=82473.5,"B",ifelse(x<=257992.0,"C","D"))), levels=c("A", "B", "C", "D")))
play.test$PRICE <- sapply(play.test$PRICE, function(x) factor(ifelse(x<=64304.5,"A",ifelse(x<=82473.5,"B",ifelse(x<=257992.0,"C","D"))), levels=c("A", "B", "C", "D")))

set.seed(3433)
play.model3 <- play.my_train(my_method = 'rf');
play.model3.bak <- play.model3
play.predict.prob3 <- predict(play.model3, play.validation, type = "prob")
play.predict.raw3 <- predict(play.model3, play.validation, type = "raw")
play.confusionMatrix3 <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw3, positive = "yes")
play.acc3 <- cbind(Accuracy=play.confusionMatrix3$overall[1], FScore = play.model3$results$F1)
print(play.acc3)
#Accuracy    FScore
#[0.8944 0.8132235

play.train <- train;
play.validation <- validation;
play.test <- test;


# use 3


#_________________________

# PRICE_BY_GBA

#_________________________

play.train <- train;
play.validation <- validation;
play.test <- test;

play.attrs <- 'PRICE_BY_GBA';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        #na.action = na.omit,
        metric = "F1")
}

# 3 Impute by building tree

play.train[is.na(play.train$PRICE_BY_GBA), "PRICE_BY_GBA"] <- 0;
play.validation[is.na(play.validation$PRICE_BY_GBA), "PRICE_BY_GBA"] <- 0;
play.test[is.na(play.test$PRICE_BY_GBA), "PRICE_BY_GBA"] <- 0;
play.sub_model <- train(QUALIFIED~PRICE_BY_GBA, 
                        data = play.train, 
                        method = "rpart",
                        #preProc = c("center", "scale"),
                        tuneLength=5,
                        trControl=trainControl(method = "cv", number = 10, verboseIter = TRUE),
)

#text(play.sub_model$finalModel)
#cut-points: 
play.sub_model$finalModel$splits[,4]
#64304.5 257992.0  82473.5 

play.train$PRICE_BY_GBA <- sapply(play.train$PRICE_BY_GBA, function(x) factor(ifelse(x<=64304.5,"A",ifelse(x<=82473.5,"B",ifelse(x<=257992.0,"C","D"))), levels=c("A", "B", "C", "D")))
play.validation$PRICE_BY_GBA <- sapply(play.validation$PRICE_BY_GBA, function(x) factor(ifelse(x<=64304.5,"A",ifelse(x<=82473.5,"B",ifelse(x<=257992.0,"C","D"))), levels=c("A", "B", "C", "D")))
play.test$PRICE_BY_GBA <- sapply(play.test$PRICE_BY_GBA, function(x) factor(ifelse(x<=64304.5,"A",ifelse(x<=82473.5,"B",ifelse(x<=257992.0,"C","D"))), levels=c("A", "B", "C", "D")))

set.seed(3433)
play.model3 <- play.my_train(my_method = 'rf');
#play.model3.bak <- play.model3
play.predict.prob3 <- predict(play.model3, play.validation, type = "prob")
play.predict.raw3 <- predict(play.model3, play.validation, type = "raw")
play.confusionMatrix3 <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw3, positive = "yes")
play.acc3 <- cbind(Accuracy=play.confusionMatrix3$overall[1], FScore = play.model3$results$F1)
print(play.acc3)
#Accuracy    FScore
#0.8946667 0.8131267

play.train <- train;
play.validation <- validation;
play.test <- test;


# use 3



#_________________________

# NUM_UNITS, KITCHES (correlation) - which to drop

#_________________________

play.train <- train;
play.validation <- validation;
play.test <- test;

play.attrs <- 'NUM_UNITS';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        #na.action = na.omit,
        metric = "F1")
}

set.seed(3433)
play.model <- play.my_train(my_method = 'rf');
play.model.bak <- play.model
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.confusionMatrix <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw, positive = "yes")
play.acc <- cbind(Accuracy=play.confusionMatrix$overall[1], FScore = play.model$results$F1)
print(play.acc)
#Accuracy FScore
# 0.5690667      0


play.attrs <- 'KITCHENS';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        #na.action = na.omit,
        metric = "F1")
}

set.seed(3433)
play.model2 <- play.my_train(my_method = 'rf');
play.model2.bak <- play.model2
play.predict.prob2 <- predict(play.model2, play.validation, type = "prob")
play.predict.raw2 <- predict(play.model2, play.validation, type = "raw")
play.confusionMatrix2 <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw2, positive = "yes")
play.acc2 <- cbind(Accuracy=play.confusionMatrix2$overall[1], FScore = play.model2$results$F1)
print(play.acc2)
#Accuracy    FScore
#A 0.5721333 0.6589051

#keep KITCHENS

#_________________________

# AYB_TO_SALEDATE, EYB_TO_SALEDATE (correlation) - which to drop

#_________________________

play.train <- train;
play.validation <- validation;
play.test <- test;

play.attrs <- 'AYB_TO_SALEDATE';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        #na.action = na.omit,
        metric = "F1")
}

set.seed(3433)
play.model <- play.my_train(my_method = 'rf');
play.model.bak <- play.model
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.confusionMatrix <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw, positive = "yes")
play.acc <- cbind(Accuracy=play.confusionMatrix$overall[1], FScore = play.model$results$F1)
print(play.acc)
#Accuracy    FScore
#0.636 0.5431612

play.attrs <- 'EYB_TO_SALEDATE';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        #na.action = na.omit,
        metric = "F1")
}

set.seed(3433)
play.model2 <- play.my_train(my_method = 'rf');
play.model2.bak <- play.model2
play.predict.prob2 <- predict(play.model2, play.validation, type = "prob")
play.predict.raw2 <- predict(play.model2, play.validation, type = "raw")
play.confusionMatrix2 <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw2, positive = "yes")
play.acc2 <- cbind(Accuracy=play.confusionMatrix2$overall[1], FScore = play.model2$results$F1)
print(play.acc2)
#Accuracy    FScore
#0.6972 0.6046808

#keep EYB_TO_SALEDATE


#_________________________

# USECODE

#_________________________

play.train <- train;
play.validation <- validation;
play.test <- test;

play.attrs <- 'USECODE';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        #na.action = na.omit,
        metric = "F1")
}

# 1 as number

set.seed(3433)
play.model <- play.my_train(my_method = 'rf');
play.model.bak <- play.model
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.confusionMatrix <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw, positive = "yes")
play.acc <- cbind(Accuracy=play.confusionMatrix$overall[1], FScore = play.model$results$F1)
print(play.acc)
#Accuracy    FScore
# 0.5677333 0.6540437

# 2 as category

play.train <- train;
play.validation <- validation;
play.test <- test;

play.train[,"USECODE"] <- factor(play.train[,"USECODE"])
play.validation[,"USECODE"] <- factor(play.validation[,"USECODE"])
play.test[,"USECODE"] <- factor(play.test[,"USECODE"])

set.seed(3433)
play.model2 <- play.my_train(my_method = 'rf');
play.model2.bak <- play.model2
play.predict.prob2 <- predict(play.model2, play.validation, type = "prob")
play.predict.raw2 <- predict(play.model2, play.validation, type = "raw")
play.confusionMatrix2 <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw2, positive = "yes")
play.acc2 <- cbind(Accuracy=play.confusionMatrix2$overall[1], FScore = play.model2$results$F1)
print(play.acc2)
#Accuracy    FScore
#0.6344 0.6448977

# use category

#_________________________

# CATEGORICAL CORRELATION

#_________________________

play.train <- train[,full_cat_attrs]
play.validation <- validation[,full_cat_attrs]
play.test <- test[,full_cat_attrs]


#"HEAT", "AC", "STYLE", "STRUCT", "EXTWALL", "ROOF", "INTWALL", "PRICE", "AYB_TO_YR_RMDL", "EYB_TO_YR_RMDL"

attr_pairs <- expand.grid(attr1 = full_cat_attrs, attr2 = full_cat_attrs)

full_cram <- data.frame(attr.attr1 = character(), attr.attr2 = character(), cramer = numeric(), stringsAsFactors = FALSE)


for(i in 1 : nrow(attr_pairs)) {
  attr_pair <- attr_pairs[i,]
  if(attr_pair$attr1 != attr_pair$attr2) {
    xt <- xtabs(as.formula(paste("~",attr_pair$attr1, " + ", attr_pair$attr2)),data = play.train)
    cram <- cbind(attr=attr_pair, cramer=summary(assocstats(xt))$object$cramer)
    full_cram <- rbind(full_cram, cram)
  }
}

head(full_cram[order(full_cram$cramer, decreasing = TRUE),], 6)


#_________________________

# CATEGORICAL ONE HOT ENCODING

#_________________________

play.train <- train
play.validation <- validation
play.test <- test

# 1 Categorical

play.attrs <- 'HEAT + AC + STYLE + STRUCT + EXTWALL + ROOF + INTWALL + PRICE + AYB_TO_YR_RMDL';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        #na.action = na.omit,
        metric = "F1")
}

set.seed(3433)
play.model <- play.my_train(my_method = 'rf');
play.model.bak <- play.model
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.confusionMatrix <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw, positive = "yes")
play.acc <- cbind(Accuracy=play.confusionMatrix$overall[1], FScore = play.model$results$F1)
print(play.acc)
#Fitting mtry = 2 on full training set
#Accuracy    FScore
#0.7470667 0.8559204

# 2 NZV dropped

play.train <- train
play.validation <- validation
play.test <- test

play.formula <- as.formula(paste('QUALIFIED ~ ', BBmisc::collapse(nzv_keep, " + ")))

play.train_onehot <- predict(dummyVars(~., data=play.train[,full_cat_attrs]), play.train[,full_cat_attrs])
nzv_full <- nearZeroVar(play.train_onehot, saveMetrics = TRUE)
nzv_keep <- row.names(nzv_full[nzv_full$nzv == FALSE,])
play.train_onehot <- as.data.frame(cbind(play.train_onehot[,nzv_keep], QUALIFIED = play.train$QUALIFIED))
play.train_onehot$QUALIFIED = factor(play.train_onehot$QUALIFIED, labels = c("no", "yes"))

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train_onehot,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        #na.action = na.omit,
        metric = "F1")
}

set.seed(3433)
play.model2 <- play.my_train(my_method = 'rf');



play.model2.bak <- play.model2
play.validation_onehot <- predict(dummyVars(~., data=play.validation[,full_cat_attrs]), play.validation[,full_cat_attrs])
play.validation_onehot <- as.data.frame(cbind(play.validation_onehot[,nzv_keep], QUALIFIED = play.validation$QUALIFIED))
play.validation_onehot$QUALIFIED = factor(play.validation_onehot$QUALIFIED, labels = c("no", "yes"))
##FIXME!
play.predict.prob2 <- predict(play.model2, play.validation_onehot, type = "prob")
play.predict.raw2 <- predict(play.model2, play.validation_onehot, type = "raw")
play.confusionMatrix2 <- confusionMatrix(play.validation_onehot$QUALIFIED, play.predict.raw2, positive = "yes")
play.acc2 <- cbind(Accuracy=play.confusionMatrix2$overall[1], FScore = play.model2$results$F1)
print(play.acc2)
#Fitting mtry = 27
#Accuracy    FScore
#[1,] 0.8885333 0.8123743


#_________________________

# ATTRIBUTE SELECTION

#_________________________

# StepAIC

play.train <- train

#method = "cv", number = 10, 
play.train_control <-  trainControl(method = none, verboseIter = TRUE)

set.seed(3433)
step.model <- train(QUALIFIED ~ ., data = play.train,
                    method = "glmStepAIC", 
                    preProcess=c("scale","center"),
                    tuneLength = 1, 
                    trControl = play.train_control,
                    trace = 1
)
#step.model_bak <- step.model

#step.cfm <- confusionMatrix(validation$QUALIFIED, predict(step.model, validation), positive = "yes")
#step.summ <- summary(step.model$finalModel)$coefficients
#step.pred <- predict(step.model, train)
step.imp <- varImp(step.model$finalModel)
#step.imp_top <- head(data.frame(attr = rownames(step.imp), step.imp)[order(step.imp, decreasing = TRUE),], 10)
#step.imp_bottom <- head(data.frame(attr = rownames(step.imp), step.imp)[order(step.imp, decreasing = FALSE),], 10)
#FULL LIST
step.imp <- varImp(step.model$finalModel)
c(rownames(step.imp)[order(step.imp, decreasing = TRUE)], colnames(train[,!(colnames(train) %in% rownames(step.imp))]))

#[1] "PRICE.A"          "PRICE.D"          "CNDTN"            "GBA"              "PRICE.C"          "STRUCT.8"         "INTWALL.6"        "USECODE"          "BATHRM"          
#[10] "STYLE.4"          "FIREPLACES"       "STRUCT.7"         "AYB_TO_SALEDATE"  "AYB"              "EXTWALL.14"       "SALEDATE"         "INTWALL.11"       "STYLE.6"         
#[19] "INTWALL.3"        "AYB_TO_YR_RMDL.A" "STORIES"          "EYB"              "HF_BATHRM"        "AC.Y"             "AC.N"             "BEDRM"            "HEAT.1"          
#[28] "HEAT.7"           "HEAT.13"          "STYLE.7"          "STRUCT.1"         "STRUCT.6"         "EXTWALL.22"       "ROOF.1"           "ROOF.2"           "ROOF.6"          
#[37] "ROOF.11"          "AYB_TO_YR_RMDL.B" "AYB_TO_YR_RMDL.C" "PRICE.B"          "ROOMS"            "SALE_NUM"         "BLDG_NUM"         "GRADE"            "KITCHENS"        
#[46] "LANDAREA"

# Alternative - BARUTA

play.train <- train
set.seed(3433)
#importance_boruta <- Boruta(QUALIFIED ~ ., data=play.train, doTrace=2)  
#importance_boruta_bak <- importance_boruta

full_importance_boruta <- TentativeRoughFix(importance_boruta)
baruta_imp <- imps[attStats(full_importance_boruta)$decision != 'Rejected', c('meanImp', 'decision')]
baruta_full <- imps2[order(-imps2$meanImp), ]
plot(full_importance_boruta, cex.axis=.7, las=2, xlab="", main="Variable Importance")  
#full
#meanImp  decision
#GBA              53.664969 Confirmed
#PRICE.A          46.377138 Confirmed
#EYB              46.070174 Confirmed
#CNDTN            45.725595 Confirmed
#PRICE.D          43.150707 Confirmed
#AYB              41.484240 Confirmed
#AYB_TO_SALEDATE  40.390072 Confirmed
#LANDAREA         39.870061 Confirmed
#SALEDATE         35.486008 Confirmed
#ROOMS            33.229982 Confirmed
#PRICE.C          33.048678 Confirmed
#GRADE            31.521675 Confirmed
#BEDRM            27.588943 Confirmed
#BATHRM           26.424226 Confirmed
#STORIES          22.900505 Confirmed
#FIREPLACES       21.753287 Confirmed
#PRICE.B          21.273825 Confirmed
#KITCHENS         21.039131 Confirmed
#USECODE          20.269425 Confirmed
#AYB_TO_YR_RMDL.C 20.064766 Confirmed
#SALE_NUM         19.727831 Confirmed
#STRUCT.1         19.482189 Confirmed
#AC.N             17.900689 Confirmed
#AC.Y             17.613062 Confirmed
#STYLE.4          17.171398 Confirmed
#ROOF.1           16.891788 Confirmed
#HF_BATHRM        16.563341 Confirmed
#EXTWALL.14       16.512574 Confirmed
#AYB_TO_YR_RMDL.B 15.877437 Confirmed
#HEAT.1           13.346459 Confirmed
#STRUCT.8         13.272337 Confirmed
#STRUCT.7         12.777903 Confirmed
#AYB_TO_YR_RMDL.A 12.545897 Confirmed
#ROOF.6           12.406772 Confirmed
#HEAT.13          11.912266 Confirmed
#STYLE.7          11.687453 Confirmed
#HEAT.7           11.621337 Confirmed
#INTWALL.6        11.583257 Confirmed
#ROOF.11          11.410316 Confirmed
#ROOF.2           10.478304 Confirmed
#INTWALL.3        10.331621 Confirmed
#STYLE.6           9.367148 Confirmed
#INTWALL.11        8.031034 Confirmed
#EXTWALL.22        6.802767 Confirmed
#STRUCT.6          2.893964 Confirmed




######## BUILDING MODELS ##########


# GBM

play.train <- train;
play.validation <- validation;
play.test <- test;

play.attrs <- '.';
play.formula <- as.formula(paste('QUALIFIED ~ ', play.attrs))

play.train_control <-  trainControl(method = "cv", number = 10, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 10, 
        metric = "F1")
}


set.seed(3433)
play.model_gbm <- play.my_train(my_method = 'gbm');
play.model <- play.model_gbm
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.prediction <- prediction(play.predict.prob[[2]], ifelse(play.validation[,"QUALIFIED"] == "yes", 1, 0));

# Accuracy & F score
play.confusionMatrix <- confusionMatrix(play.validation[,"QUALIFIED"], play.predict.raw, positive = "yes")
play.accuracy <- play.confusionMatrix[4]$byClass["Balanced Accuracy"]
play.fscore <- play.confusionMatrix[4]$byClass["F1"]

# ROC curve
play.roc <- performance(play.prediction, "tpr", "fpr");
plot(play.roc,
     #avg= "threshold",
     colorize=TRUE,
     lwd= 3,
     main= "ROC curve")

# ROC area under the curve
play.auc <- as.numeric(performance(play.prediction,"auc")@y.values)

# Summary of results
play.summ_stats <- as.data.frame(rbind(play.accuracy, play.fscore, play.auc))
colnames(play.summ_stats) <- "Measure"
#play.accuracy 0.8987067
#play.fscore   0.8899804
#play.auc      0.9468100
print(play.summ_stats)

model_gbm <- play.model_gbm
play.summ_stats_gbm <- play.summ_stats


# Decision Tree - c5.0

play.train_control <-  trainControl(method = "cv", number = 10, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 10, 
        metric = "F1")
}

set.seed(3433)
play.model_c50 <- play.my_train(my_method = 'C5.0');
play.model <- play.model_c50 
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.prediction <- prediction(play.predict.prob[[2]], ifelse(play.validation[,"QUALIFIED"] == "yes", 1, 0));

# Accuracy & F score
play.confusionMatrix <- confusionMatrix(play.validation[,"QUALIFIED"], play.predict.raw, positive = "yes")
play.accuracy <- play.confusionMatrix[4]$byClass["Balanced Accuracy"]
play.fscore <- play.confusionMatrix[4]$byClass["F1"]

# ROC curve
play.roc <- performance(play.prediction, "tpr", "fpr");
plot(play.roc,
     #avg= "threshold",
     colorize=TRUE,
     lwd= 3,
     main= "ROC curve")

# ROC area under the curve
play.auc <- as.numeric(performance(play.prediction,"auc")@y.values)

# Summary of results
play.summ_stats <- as.data.frame(rbind(play.accuracy, play.fscore, play.auc))
colnames(play.summ_stats) <- "Measure"
#play.accuracy         0.8998836
#play.fscore           0.8911002
#play.auc              0.9430783
print(play.summ_stats)

play.summ_stats_c50 <- play.summ_stats
model_c50 <- play.model_c50




# Random forest

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        metric = "F1")
}

set.seed(3433)
#play.model_rf <- play.my_train(my_method = 'rf');
play.model <- play.model_rf
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.prediction <- prediction(play.predict.prob[[2]], ifelse(play.validation[,"QUALIFIED"] == "yes", 1, 0));

# Accuracy & F score
play.confusionMatrix <- confusionMatrix(play.validation[,"QUALIFIED"], play.predict.raw, positive = "yes")
play.accuracy <- play.confusionMatrix[4]$byClass["Balanced Accuracy"]
play.fscore <- play.confusionMatrix[4]$byClass["F1"]

# ROC curve
play.roc <- performance(play.prediction, "tpr", "fpr");
plot(play.roc,
     #avg= "threshold",
     colorize=TRUE,
     lwd= 3,
     main= "ROC curve")

# ROC area under the curve
play.auc <- as.numeric(performance(play.prediction,"auc")@y.values)

# Summary of results
play.summ_stats <- as.data.frame(rbind(play.accuracy, play.fscore, play.auc))
colnames(play.summ_stats) <- "Measure"
#play.accuracy         0.8814610
#play.fscore           0.8726748
#play.auc              0.9304893
print(play.summ_stats)

play.summ_stats_rf <- play.summ_stats
model_rf <- play.model_rf



# K Nearest Neighbour

play.train_control <-  trainControl(method = "cv", number = 10, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 10, 
        metric = "F1")
}

set.seed(3433)
play.model_knn <- play.my_train(my_method = 'knn');
play.model <- play.model_knn
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.prediction <- prediction(play.predict.prob[[2]], ifelse(play.validation[,"QUALIFIED"] == "yes", 1, 0));

# Accuracy & F score
play.confusionMatrix <- confusionMatrix(play.validation[,"QUALIFIED"], play.predict.raw, positive = "yes")
play.accuracy <- play.confusionMatrix[4]$byClass["Balanced Accuracy"]
play.fscore <- play.confusionMatrix[4]$byClass["F1"]

# ROC curve
play.roc <- performance(play.prediction, "tpr", "fpr");
plot(play.roc,
     #avg= "threshold",
     colorize=TRUE,
     lwd= 3,
     main= "ROC curve")

# ROC area under the curve
play.auc <- as.numeric(performance(play.prediction,"auc")@y.values)

# Summary of results
play.summ_stats <- as.data.frame(rbind(play.accuracy, play.fscore, play.auc))
colnames(play.summ_stats) <- "Measure"
#play.accuracy         0.8814610
#play.fscore           0.8726748
#play.auc              0.9304893
print(play.summ_stats)

model_knn <- play.model_knn
play.summ_stats_knn <- play.summ_stats



# Neural Networks

play.train_control <-  trainControl(method = "cv", number = 10, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 10, 
        metric = "F1")
}

set.seed(3433)
play.model_nnet <- play.my_train(my_method = 'nnet');
play.model <- play.model_nnet
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.prediction <- prediction(play.predict.prob[[2]], ifelse(play.validation[,"QUALIFIED"] == "yes", 1, 0));

# Accuracy & F score
play.confusionMatrix <- confusionMatrix(play.validation[,"QUALIFIED"], play.predict.raw, positive = "yes")
play.accuracy <- play.confusionMatrix[4]$byClass["Balanced Accuracy"]
play.fscore <- play.confusionMatrix[4]$byClass["F1"]

# ROC curve
play.roc <- performance(play.prediction, "tpr", "fpr");
plot(play.roc,
     #avg= "threshold",
     colorize=TRUE,
     lwd= 3,
     main= "ROC curve")

# ROC area under the curve
play.auc <- as.numeric(performance(play.prediction,"auc")@y.values)

# Summary of results
play.summ_stats <- as.data.frame(rbind(play.accuracy, play.fscore, play.auc))
colnames(play.summ_stats) <- "Measure"
#play.accuracy 0.8987067
#play.fscore   0.8899804
#play.auc      0.9468100
print(play.summ_stats)

model_nnet <- play.model_nnet
play.summ_stats_nnet <- play.summ_stats


# SVM
#TRAINING ON ONLY 30% records!!

set.seed(3433)
play.train_sample <- train[sample(nrow(train), 0.30*nrow(train)),];

play.train_control <-  trainControl(method = "cv", number = 5, verboseIter = TRUE, classProbs = TRUE, summaryFunction = f1)

play.my_train <- function (my_method = "rf") {
  train(play.formula,
        data = play.train_sample,
        method = my_method,
        trControl=play.train_control,
        preProcess=c("scale","center"),
        tuneLength = 5, 
        metric = "F1")
}

set.seed(3433)
#play.model_svm <- play.my_train(my_method = 'svmRadial');
play.model <- play.model_svm
play.predict.raw <- predict(play.model, play.validation, type = "raw")
play.predict.prob <- predict(play.model, play.validation, type = "prob")
play.prediction <- prediction(play.predict.prob[[2]], ifelse(play.validation[,"QUALIFIED"] == "yes", 1, 0));

# Accuracy & F score
play.confusionMatrix <- confusionMatrix(play.validation[,"QUALIFIED"], play.predict.raw, positive = "yes")
play.accuracy <- play.confusionMatrix[4]$byClass["Balanced Accuracy"]
play.fscore <- play.confusionMatrix[4]$byClass["F1"]

# ROC curve
play.roc <- performance(play.prediction, "tpr", "fpr");
plot(play.roc,
     #avg= "threshold",
     colorize=TRUE,
     lwd= 3,
     main= "ROC curve")

# ROC area under the curve
play.auc <- as.numeric(performance(play.prediction,"auc")@y.values)

# Summary of results
play.summ_stats <- as.data.frame(rbind(play.accuracy, play.fscore, play.auc))
colnames(play.summ_stats) <- "Measure"
#play.accuracy 0.8971668
#play.fscore   0.8888575
#play.auc      0.9340695
print(play.summ_stats)

model_svm <- play.model_svm
play.summ_stats_svm <- play.summ_stats



######## BALANCING ##########

play.train_pos <- ((play.train[play.train$QUALIFIED == "yes",])[sample(nrow(play.train[play.train$QUALIFIED == "yes",]),sum(play.train$QUALIFIED == "no"), replace = TRUE),])
play.train_neg <- play.train[play.train$QUALIFIED == "no",]
play.train <- as.data.frame(rbind(play.train_pos, play.train_neg))

train_pos <- ((train[train$QUALIFIED == "yes",])[sample(nrow(train[train$QUALIFIED == "yes",]),sum(train$QUALIFIED == "no"), replace = TRUE),])
train_neg <- train[train$QUALIFIED == "no",]
train <- as.data.frame(rbind(train_pos, train_neg))


# SUBMISSIONS

best_model <- model_c50
test_prediction <- predict(best_model, test, type = "raw")
test_output <- as.data.frame(cbind("Row ID" = raw_test$row.ID, "Predict-Qualified" = ifelse(test_prediction == "yes", 1, 0)))
submission_num <- "01"
write.csv(test_output,paste0("C:\\Users\\d309144\\OneDrive - Telstra\\Microcredentials\\Advanced Analytics\\assignment\\submission_",submission_num,".csv"), row.names = FALSE, quote = FALSE)

#Submission 01 - C5.0 with ~ PRICE.A + PRICE.D + PRICE.C + CNDTN + GBA (# Tuning: 5, # folds: 10)
#Submission 02 - GBM with ~ . (# Tuning: 10, # folds: 10)
#Submission 03 - ENSEMBLE with ~ . with weights: 




# PLAYYY

#play.predict.raw <- predict(play.model, play.validation, type = "raw")

#play.confusionMatrix <- confusionMatrix(play.validation$QUALIFIED, play.predict.raw, positive = "yes")
#cbind(Accuracy=play.confusionMatrix$overall[1], FScore = play.model$results$F1)
#Accuracy    FScore
#Accuracy 0.5966667 0.6668178
#play.auc <- roc(ifelse(play.validation[,"QUALIFIED"] == "yes", 1, 0), play.predict.prob[[2]])$auc
#play.fscore2 <- performance(play.prediction, "f");
#play.fscore2 <- play.fscore2@y.values[[1]]
#play.fscore2 <- performance(play.prediction, "prec", "rec")
#play.precision <- play.confusionMatrix[4]$byClass["Pos Pred Value"]
#play.recall <- play.confusionMatrix[4]$byClass["Sensitivity"]
#play.fscore <- (2 * play.precision * play.recall) / (play.precision + play.recall)
