#set working directory
setwd(" ")

# Download the dataset
data <- read.csv("credit.csv",stringsAsFactors = FALSE,header = TRUE)

#Data Preprocessing
#Checks the dataset for missing values
sum(is.na(data))
#checks for structure and summary of the dataset
str(data)
summary(data)

#Boxplot to visually check for outliers
par(mfrow=c(4,5))
for (i in 1:20) {
  boxplot(data[,i], xlab=names(data)[i],main = " ")
}

#treatment of outliers
#check for outliers in numeric variables and if outliers found than carry out capping and flooring
boxplot(data$Age.in.Years)
quantile(data$Age.in.Years,seq(0,1,0.01))
data$Age.in.Years[which(data$Age.in.Years>63)]<-quantile(data$Age.in.Years,0.97)

boxplot(data$Duration.in.month)
quantile(data$Duration.in.month,seq(0,1,0.01))
data$Duration.in.month[which(data$Duration.in.month>42)]<-quantile(data$Duration.in.month,0.92)

boxplot(data$Credit.amount)
quantile(data$Credit.amount,seq(0,1,0.01))
data$Credit.amount[which(data$Credit.amount>7687.88)]<-quantile(data$Credit.amount,0.92)

# Exploratory Data Analysis
#univariate plots
library(ggplot2)
#plot credit history
credit_history <-  ggplot(data,aes(x=Credit.history,fill=factor(Creditability)))+geom_bar()+ scale_x_discrete(limits=c("A32","A34","A33","A31","A30"))+guides(fill=FALSE)

#plot loan purpose
purpose <-  ggplot(data,aes(x=Purpose,fill=factor(Creditability)))+geom_bar( )+ scale_x_discrete(limits=c("A43","A40","A42","A41","A49","A46","A45","A44","A410","A48"))+guides(fill=FALSE)

#plot credit amount
credit_amount <-  ggplot(data,aes(x=Credit.amount,
                                           fill=factor(Creditability)))+geom_histogram(binwidth = 500)+guides(fill=FALSE)
#plot Installment_rate
Installment_rate <- ggplot(data,aes(x=factor(Installment.rate.in.percentage.of.disposable.income), fill=factor(Creditability)))+geom_bar()+
guides(fill=guide_legend(reverse=TRUE))+scale_fill_discrete(labels=c("Good","Bad"))+labs(fill='Loan status')+scale_x_discrete(limits=c("4","2","3","1"))

#plot loan property
property <-  ggplot(data,aes(x=Property,fill=factor(Creditability)))+geom_bar( )+
  scale_x_discrete(limits=c("A123","A121","A122","A124"))+guides(fill=FALSE)

#plot loan telephone
telephone <- ggplot(data,aes(x=Telephone,fill=factor(Creditability)))+geom_bar( )+
  guides(fill=guide_legend(reverse=TRUE))+
  labs(fill='Loan status')+scale_fill_discrete(labels=c("Good","Bad"))

#plot housing                           
housing <-  ggplot(data,aes(x=Housing,fill=factor(Creditability)))+geom_bar( )+
  scale_x_discrete(limits=c("A152","A151","A153"))+guides(fill=FALSE)

#plot loan job
job <-  ggplot(data,aes(x=Job_status,fill=factor(Creditability)))+geom_bar( )+
  scale_x_discrete(limits=c("A173","A172","A174","A171"))+guides(fill=FALSE)

#plot checking account
checking_account <- ggplot(data,aes(x=Status.of.existing.checking.account,fill=factor(Creditability)))+geom_bar( )+
  scale_x_discrete(limits=c("A14","A11","A12","A13"))+guides(fill=FALSE)

#plot savings bonds
savings_bonds <- ggplot(data,aes(x=Savings.account.bonds,fill=factor(Creditability)))+geom_bar( )+
  scale_x_discrete(limits=c("A61","A65","A62","A63","A64"))+guides(fill=FALSE)

#plot loan duration
duration <- ggplot(data,aes(x=Duration.in.month,fill=factor(Creditability)))+geom_histogram(binwidth = 5)+
  guides(fill=FALSE)

#plot loan other debtors
debtors <- ggplot(data,aes(x=Other.debtors...guarantors,fill=factor(Creditability)))+geom_bar( )+
  guides(fill=guide_legend(reverse=TRUE))+scale_x_discrete(limits=c("A101","A103","A102"))+
  labs(fill='Loan status')+scale_fill_discrete(labels=c("Good","Bad"))

#Plot individual barplots in grid
grid.arrange(credit_history, purpose,credit_amount,Installment_rate, property, housing,job,
             telephone, checking_account,debtors,duration,savings_bonds, ncol=4,top = "Univariate Analysis")

#Set seed for same results
#data needs to be split into train and test data 
#Ensure the appropriate proportion of class labels in train and test datasets
set.seed(123)
sample2 <- sample(nrow(data), 0.70 * nrow(data))
xtrain <- data[sample2, ]
xtest <- data[-sample2, ]

#Logistic Regression
#regression with all attributes
model1 <- glm(Creditability ~.,family=binomial(link='logit'),data=xtrain)
summary(model1)
# Perform Stepwise selection using AIC to be able to remove insignificant variables
model <- stepAIC(model1,direction="both")
vif(model)
#FInal model is shown here
model <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Length.of.current.employment + Sex...Marital.Status, family=binomial, data = xtrain)
#summary of model
summary(model)
fit <- fitted.values(model)
#done for 50% and 75% thresholds
thresh <- rep(0,500)
for (i in 1:500)
  if(fit[i] >= 0.5) thresh[i] <- 1
results <- predict(model,newdata=xtest,type='response')
results <- ifelse(results > 0.5,1,0)
misclass <- mean(results != xtest$Creditability)
#calculates and prints error rate
print(paste('Accuracy',1-misclass))

#Linear Discriminant Analysis
library(MASS)
ldamodel <- lda(Creditability~Account.Balance+Payment.Status.of.Previous.Credit+Purpose+Credit.Amount+
                  Value.Savings.Stocks+Length.of.current.employment+Instalment.per.cent+Sex...Marital.Status+Concurrent.Credits,data=xtrain)
ldamodel
#plot of classification attribute from training set
plot(ldamodel)
ldapredict <- predict(ldamodel,xtest)
ldapred <- ldapredict$class
#Confusion Matrix
table(lda_pred_class,xtest$Creditability)

#Decision Trees
library(rpart)
library(rpart.plot)
dtree<- rpart(Creditability ~ . , data =xtrain, method= 'class')
rpart.plot(dtree,fallen.leaves= FALSE)
# display the results 
printcp(dtree) 
# visualizing cross validation results 
plotcp(dtree) 
#split summary
summary(dtree)
#testing decision tree
pred <- predict(dtree, xtest, type="class")
#confusion matrix
table(pred, xtest$Creditability)

#Random Forests
library(randomForest)
xtrain$Creditability <- as.factor(xtrain$Creditability)
xtest$Creditability <- as.factor(xtest$Creditability)
#training random forests with 300 trees
rdmodel <- randomForest(Creditability ~., data = xtrain, ntree=300, importance=T, proximity=T, mtry=5)
plot(rdmodel, main="")
rdmodel
rf_pred <- predict(rdmodel, xtest, type="class")
table(rf_pred, xtest$Creditability)
#variable importance plot
varImpPlot(rdmodel,  main="", cex=0.8)
