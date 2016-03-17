Data <- read.csv("C:\\Users\\coep\\Desktop\\eNSEMBLE\\DATA\\default_credit_card.csv",header = TRUE)



# Construct bag-free model for comparison



head(Data)#Check the dataset
names(Data)# Check the column names
attach(Data) # attach dataset

############## Set the response variable################

#response<-'total_UPDRS'
response <- 'default.payment.next.month'
Data$default.payment.next.month <- as.factor(Data$default.payment.next.month)
# set seed
set.seed(1)

# divide the data in training and test data set

#training<-sample(nrow(Data),nrow(Data)/2)

#test<- -training
training <- seq(from =1, to= 300, by=1)
train <- Data[training, ]

rm(training)
testing <- seq(from =10000, to=15000, by=1)
test <-Data[testing,]

#attach the training and test data

attach(train)
attach(test)


## apply the tree function i.e. build a tree model using training data

bagfree_tree_model<-glm(train[,response]~.,data=train[,-1],family = 'binomial', control = list(maxit = 50))

# look at summary
#summary(bagfree_tree_model)

#plot the tree
#plot(bagfree_tree_model)
#text(bagfree_tree_model,pretty=0)

## check how the model is doing using the testing dataset

bagfree_tree_pred<-predict(bagfree_tree_model,test[,-25],type = 'response')

plot(bagfree_tree_pred,test[,response])

# Calcualte the Mean squared error

#MSE<-mean((bagfree_tree_pred-test[,response])^2)  #25.04559

#cat("The mean sqared error of bag_free_model is ",MSE)

y_test <- test[,25]
fitted.results <- ifelse(bagfree_tree_pred > 0.5,1,0)
fitted.results <- as.factor(fitted.results)
misClasificError <- mean(fitted.results != y_test)
print(paste('Accuracy',1-misClasificError))


#cbind(fitted.results,y_test)

#summary(fitted.results)
#summary(y_test)



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
  #sampledata <-  sample(1000,500)
  
  sampledata### Fit a decision tree on bootstraped sample
  
  logistic <- glm(train[sampledata,][,25]~ ., data=train[sampledata,][,-1], family = 'binomial', control = list(maxit = 50))
  #tree_model<-tree(train[sampledata,][,6] ~ .,train[sampledata,]) # fhat
  summary(logistic)
  ## check how the model is doing using the testing dataset
  # Make the prediction
  tree_pred<-data.frame(ifelse(predict(logistic,test[,-1],type = 'response')> 0.5,1,0)) # fhat(x)
  #  tree_pred<-data.frame(predict(object=tree_model,test)) # fhat(x)
  tree_pred
  summary(tree_pred)#Store the predictions for each bootstrapped sample
  predictions<-cbind(predictions,tree_pred)
  
}
#predictions
finalresult <- ifelse(rowSums(predictions[,-1])>500, 1,0)
y_test <- test[,25]
finalresult <- as.factor(finalresult)
misClasificError <- mean(finalresult != y_test)

summary(finalresult)
summary(y_test)
print(paste('Accuracy',1-misClasificError))

