library(e1071)

Data <- read.csv("C:\\Users\\coep\\Desktop\\eNSEMBLE\\DATA\\default_credit_card.csv",header = TRUE)



# Construct bag-free model for comparison





response <- 'default.payment.next.month'
#Data$default.payment.next.month <- as.factor(Data$default.payment.next.month)
# set seed
set.seed(1)

# divide the data in training and test data set

#training<-sample(nrow(Data),nrow(Data)/2)


training <- seq(from =1, to= 400, by=1)
train <- Data[training, ]
train$default.payment.next.month <- as.factor(train$default.payment.next.month)
rm(training)
testing <- seq(from =1000, to=1500, by=1)
test <-Data[testing,]

#attach the training and test data

attach(train)
attach(test)
summary(test)

## apply the tree function i.e. build a tree model using training data

names(train)
NBmodel <- naiveBayes(train[,response]~., data = train[,-1],type = 'c')
# look at summary
summary(NBmodel)


## check how the model is doing using the testing dataset

fitted.results<-predict(NBmodel,test[,-25],type = 'class')
fitted.results
summary(fitted.results)

y_test <- test[,25]

misClasificError <- mean(fitted.results != y_test)
print(paste('Accuracy',1-misClasificError))


B=1000 # number of trees

# Generate k learners

length_divisor<-2

rm(predictions) # remove predictions matrix if exist any

predictions<-cbind(1:nrow(test)) # create predictions matrix to store bagged trees columnwise

#apply bagging

for (b in 1:B)
{
  # Take a bootsrapes sample from the training data
  
  sampledata<-sample(nrow(train),size=floor((nrow(train)/length_divisor)))
  
  
  sampledata### Fit a decision tree on bootstraped sample
  
  
  NBmodel <- naiveBayes(train[sampledata,][,response]~ ., data = train[sampledata,][,-1])
  #summary(NBmodel)
  
  
  
  ## check how the model is doing using the testing dataset
  # Make the prediction
  
  fitted.results <- data.frame(predict(NBmodel,test[,-1], type = 'class') )
  
  #Store the predictions for each bootstrapped sample
  predictions<-cbind(predictions,fitted.results)
  
}

#finalresult <- ifelse(rowSums(predictions[,-1])>500, 1,0)
#predictions
dim(predictions)

rm(x1)
orig_levels <- c(0,1)
x1<-cbind(1:nrow(test))
m = B+1
for(i in 3:m){
  x1 <- cbind(x1,orig_levels[predictions[,i]])
  i=i+1
  # print("hello")
}

#rowSums(x1[,-1])
finalresult <- ifelse(rowSums(x1[,-1])>300, 1,0)
y_test <- test[,25]
summary(y_test)
summary(finalresult)

misClasificError <- mean(finalresult != y_test)
print(paste('Accuracy',1-misClasificError))
