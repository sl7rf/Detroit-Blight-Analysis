#Setup
rm(list = ls(all = TRUE)) 
gc(reset=TRUE)
set.seed(17) 

# Preditive modelling
library(caret)
library(e1071)
library(doMC)
library(caret)
library(rpart)
library(caretEnsemble)

registerDoMC(cores = 5)
set.seed(7)
setwd("~/Documents/Coursera/Capstone Project Detroit/data")
data <- read.csv('final_clean_data.csv')
data$X <- NULL
data$Lat <- NULL
data$Lon <- NULL
data$blighted <- as.factor(ifelse(data$blighted==1, 'Y', 'N'))
summary(data)

# create samples
train_0 <- sample(data[data$blighted=='N',1], size = 1600, replace = F)
train_1 <- sample(data[data$blighted=='Y',1], size = 1600, replace = F)
train_data <- data[data$cluster_id %in% c(train_0, train_1), ]

test_0 <- sample(data[(data$blighted=='N') & !(data$cluster_id %in% train_0), 1], size=320, replace = F)
test_1 <- data[(data$blighted=='Y') & !(data$cluster_id %in% train_1), 1]
test_data <- data[data$cluster_id %in% c(test_0, test_1), ]

set.seed(3)
ctrl <- trainControl(method = "repeatedcv",
                     number = 5, 
                     repeats = 3,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     savePredictions = 'all',
                     allowParallel = TRUE,
                     index=createMultiFolds(train_data$blighted, k=5, times=3)
                     )
# Model 0: logistic
model_0 <- train(blighted ~.,
                 data = train_data[,-1],
                 trControl = ctrl,
                 preProcess = c('center', 'scale'), 
                 method = "glm",
                 family = binomial(logit)
                 )
model_0

# model 1: gbm
model_1 <- train(blighted ~.,
                data = train_data[,-1],
                trControl = ctrl,
                preProcess = c('center', 'scale'), 
                method = "gbm",
                tuneGrid = expand.grid(.n.trees=500, 
                                     .interaction.depth=c(3, 5, 7), 
                                     .n.minobsinnode = c(5,10),
                                     .shrinkage = c(0.01,0.001)
                                     )
                )
model_1
varImp(model_1)

# Model 2: knn
model_2 <- train(blighted ~.,
                 data = train_data[,-1],
                 trControl = ctrl,
                 preProcess = c('center', 'scale'), 
                 method = "knn"
                 )
model_2

# Model 3: xgboost
model_3 <- train(blighted ~.,
                 data = train_data[,-1],
                 trControl = ctrl,
                 preProcess = c('center', 'scale'), 
                 method = "xgbTree"
                 )
model_3
varImp(model_3)

# Model 4: svm radial
model_4 <- train(blighted ~.,
                 data = train_data[,-1],
                 trControl = ctrl,
                 preProcess = c('center', 'scale'), 
                 method = 'svmRadial',
                 tuneGrid = expand.grid(.C=c(0.1, 0.5, 0.75, 1), 
                                        .sigma= c(.01, .015, 0.2))
)
model_4

# Model 5: svm linear
model_5 <- train(blighted ~.,
                 data = train_data[,-1],
                 trControl = ctrl,
                 preProcess = c('center', 'scale'), 
                 method = 'svmLinear'
                 )
model_5

#Make a list of all the models
all.models <- list(model_0, model_1, model_2, model_3, model_4, model_5)
names(all.models) <- sapply(all.models, function(x) x$method)
sort(sapply(all.models, function(x) min(x$results$ROC)))
# set the model list as caretlist
class(all.models) <- 'caretList'
results <- resamples(all.models)
summary(results)
dotplot(results)
# correlation between results
modelCor(results)
splom(results)

# # stack using glm
# stackControl <- trainControl(method="repeatedcv", 
#                              number=5, 
#                              repeats=3, 
#                              savePredictions=TRUE, 
#                              classProbs=TRUE,
#                              summaryFunction = twoClassSummary)
# set.seed(7)
# stack.glm <- caretStack(all.models, method="glm", metric="ROC", trControl=stackControl)
# print(stack.glm)
# # stack using random forest
# set.seed(7)
# stack.rf <- caretStack(all.models, method="rf", metric="ROC", trControl=stackControl)
# print(stack.rf)

#Predict for test set:
library(caTools)
preds <- data.frame(sapply(all.models, function(x){predict(x, test_data[,2:63], type='prob')[,2]}))
#preds$stack.glm <-  ifelse(predict(stack.glm, newdata=test_data[,2:63])=='Y', 1, 0)
#preds$stack.rf <- ifelse(predict(stack.rf, newdata=test_data[,2:63])=='Y', 1, 0)
sort(data.frame(colAUC(preds, test_data[,64])))

pred_gbm = ifelse(preds$gbm<0.5,'N','Y')
confusionMatrix(pred_gbm, test_data[,64])

