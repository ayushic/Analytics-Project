setwd('Desktop/Data Analytics/A7')

# read the dataset
wine_red <- read.table("winequality-red.csv", sep=";", header=TRUE)
wine_white <- read.table("winequality-white.csv", sep=";", header=TRUE)

#change variable names from here
train_sample <- sample(nrow(wine_white), 0.7*nrow(wine_white)) #60% split of data
train.white<-wine_white[train_sample,]
test.white<-wine_white[-train_sample,]

train_sample <- sample(nrow(wine_red), 0.7*nrow(wine_red)) #60% split of data
train.red<-wine_white[train_sample,]
test.red<-wine_white[-train_sample,]


#Random forests for white wine
  barplot(table(wine_white$quality))

  wine_white$taste <- ifelse(wine_white$quality < 6, 'bad', 'good')
  wine_white$taste[wine_white$quality == 6] <- 'normal'
  wine_white$taste <- as.factor(wine_white$taste)  

  table(wine_white$taste)
  set.seed(12)
  sample1 <- sample(nrow(wine_white), 0.7 * nrow(wine_white))
  train <- wine_white[sample1, ]
  test <- wine_white[-sample1, ]

  library(randomForest)
  model <- randomForest(taste ~ . - quality, data = train, ntree=1002, mtry=5)
  randomForest(taste ~ . - quality, data = train, ntree=1002, mtry=5)
  pred <- predict(model, newdata = test)
  table(pred, test$taste) 
  varImpPlot(model)

  #Random forests for red wine
  barplot(table(wine_red$quality))
  
  wine_red$taste[wine_red$quality < 5] <- 'bad'
  wine_red$taste[wine_red$quality > 6] <- 'good'
  wine_red$taste[wine_red$quality == 5] <- 'normal'
  wine_red$taste[wine_red$quality == 6] <- 'normal'
  wine_red$taste <- as.factor(wine_red$taste)  
  
  table(wine_red$taste)
  set.seed(125)
  sample2 <- sample(nrow(wine_red), 0.7 * nrow(wine_red))
  train <- wine_red[sample2, ]
  test <- wine_red[-sample2, ]
  
  library(randomForest)
  model <- randomForest(taste ~ . - quality, data = train, ntree=1000, mtry=5)
  randomForest(taste ~ . - quality, data = train, ntree=1000, mtry=5)
  pred <- predict(model, newdata = test)
  table(pred, test$taste)  
  varImpPlot(model)
  
#Decision tree for red wine
  library(MASS)
  library(rpart)
  library(caret)

  wine_red$taste <- ifelse(wine_red$quality < 6, 'bad', 'good')
  wine_red$taste[wine_red$quality == 6] <- 'normal'
  wine_red$taste <- as.factor(wine_red$taste) 
  wine_red$quality <- as.factor(wine_red$quality)
  
  set.seed(1)
  sample2 <- sample(nrow(wine_red), 0.70 * nrow(wine_red))
  train <- wine_red[sample2, ]
  test <- wine_red[-sample2, ]
  
  wine_redTree <- rpart(formula = quality~ . -taste , data = train, method= 'class')
  rpart.plot(wine_redTree,fallen.leaves= FALSE,type = 0)
  
  wine_redPred <- predict(wine_redTree,newdata=test[-12], type = "class")
  cm = table(test[, 12], wine_redPred)
  confusionMatrix(table(wine_redPred, test$quality))
  
  #Tree Pruning
  min(wine_redTree$cptable[,"xerror"])
  which.min(wine_redTree$cptable[,"xerror"])
  wine_redTree.cp = wine_redTree$cptable[3,"CP"]
  prune.wine_redTree = prune(wine_redTree, cp= wine_redTree.cp)
  rpart.plot(prune.wine_redTree,fallen.leaves= FALSE,type = 0)
  
  predictions = predict(prune.wine_redTree, test, type="class")
  table(test$quality, predictions)
  confusionMatrix(table(predictions, test$quality))
  
  #Decision tree for white wine with 3 levels of factors as bad, good and normal
  library(MASS)
  library(rpart)
  library(caret)
  
  wine_white$taste <- ifelse(wine_white$quality < 6, 'bad', 'good')
  wine_white$taste[wine_white$quality == 6] <- 'normal'
  wine_white$taste <- as.factor(wine_white$taste) 
  wine_white$quality <- as.factor(wine_white$quality)
  
  set.seed(1)
  sample2 <- sample(nrow(wine_white), 0.75 * nrow(wine_white))
  train <- wine_white[sample2, ]
  test <- wine_white[-sample2, ]
  
  wine_whiteTree <- rpart(formula = quality~ . -taste , data = train, method= 'class')
  rpart.plot(wine_whiteTree,fallen.leaves= FALSE,type = 0)
  
  wine_whitePred <- predict(wine_whiteTree,newdata=test[-12], type = "class")
  cm = table(test[, 12], wine_whitePred)
  confusionMatrix(table(wine_whitePred, test$quality))
  
  #Tree Pruning
  min(wine_whiteTree$cptable[,"xerror"])
  which.min(wine_whiteTree$cptable[,"xerror"])
  wine_whiteTree.cp = wine_whiteTree$cptable[3,"CP"]
  prune.wine_whiteTree = prune(wine_whiteTree, cp= wine_whiteTree.cp)
  rpart.plot(prune.wine_whiteTree,fallen.leaves= FALSE,type = 0)
  
  predictions = predict(prune.wine_whiteTree, test, type="class")
  table(test$quality, predictions)
  confusionMatrix(table(predictions, test$quality))
  
  #EDA
  #statistics for both datasets
  #red wine 
  #par(mar=c(1,1,1,1))
  par(mfrow=c(3,4))
  for (i in 1:12) {
    hist(wine_red[,i], xlab=names(wine_red)[i],main = "")
  }
  
  #white wine
  par(mfrow=c(3,4))
  for (i in 1:12) {
    hist(wine_white[,i], xlab=names(wine_white)[i],main = " ")
  }
  
  #red wine to find the outliers
  par(mfrow=c(3,4))
  for (i in 1:11) {
    boxplot(wine_red[,i], xlab=names(wine_red)[i],main = " ")
  }
  
  #white wine to find the outliers
  par(mfrow=c(3,4))
  for (i in 1:11) {
    boxplot(wine_white[,i], xlab=names(wine_white)[i],main = " ")
  }
  
  #summary
  summary(wine_red)
  summary(wine_white)
  
  #scatter plot
  pairs(wine_white[,-12], gap=0, pch=4, cex=0.5, col="black")
  pairs(wine_red[,-12], gap=0, pch=4, cex=0.5, col="black")
  title(sub="Scatterplot of Chemical Attributes")
  
  #correlation matrix to interpret
  par(mfrow = c(1,1))
  cor.white <- cor(wine_white[,-12])
  corrplot(cor.white, method = 'number')
  
  par(mfrow = c(1,1))
  cor.red <- cor(wine_red[,-12])
  corrplot(cor.red, method = 'number', order = "FPC")
  
  # Fit a linear regression model with stepwise variable selection
  
  Qfit1 <- lm(quality ~ ., data= train.white)
  summary(Qfit1)
  library(car)
  vif(Qfit1)
  Qfit1 <- lm(quality ~ .- density, data= train.red)
  step <- stepAIC(Qfit1)
  Qfit2 <- lm(quality ~ volatile.acidity + residual.sugar + free.sulfur.dioxide + 
                total.sulfur.dioxide + pH + sulphates + alcohol, data = train.white)
  residualPlots(Qfit2, pch=19, col="blue", cex=0.6)
  
  summary(Qfit2)
  
  #predict regression
  library(caret)
  q <- lm(quality ~ volatile.acidity + residual.sugar + free.sulfur.dioxide +  total.sulfur.dioxide +
              pH + sulphates + alcohol,data= train.red)
  View(predictedML) <- predict(q,test.red,na.action =na.pass)
  #wine_train$quality
  # confusion matrix
  caret::confusionMatrix(as.factor(predictedML),as.factor(test.red$quality))
  table(factor(predictedML, levels=min(test.red$quality):max(test.red$quality)), factor(test.red$quality, levels=min(test.red$quality):max(test.red$quality)))
  
  mean <- predictedML
  
  #SVM
  wine <- read.csv("winequality-white.csv", sep=";")
  install.packages("e1071")
  library(e1071)
  attach(wine)
  as.factor(quality)
  hist(as.numeric(quality))
  
  # with high
  wine3level <- ifelse(wine$quality < 6, "bad", "good")
  wine3level[wine$quality == 5] <- "normal"
  wine3level[wine$quality == 6] <- "normal"
  wine3level <- as.factor(wine3level)
  str(wine3level)
  wine =data.frame(wine ,wine3level)
  set.seed(2)
  train = sample(nrow(wine),0.7*nrow(wine))
  wine.train = wine[train,]
  wine.test=wine[-train ,]
  level.test=wine3level[-train ]
  
