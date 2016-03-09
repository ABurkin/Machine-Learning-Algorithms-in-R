library(e1071)
library(pROC)

data <- read.csv("C:\\Users\\coep\\Desktop\\eNSEMBLE\\DATA\\default_credit_card.csv",header = TRUE)
head(data)
training<-sample(nrow(data),3*nrow(data)/4)
dataTraining <- data[training,]
dataTest<- data[-training,]
x_train <- dataTraining[,-25]
y_train <- dataTraining[,25]
x_test <- dataTest[,-25]
x <- cbind(x_train,y_train)
#lapply(x, class)
x$y_train <- as.factor(x$y_train)
#head(x$y_train)

naiveBayesModel <- naiveBayes(y_train ~ .,data = x)

summary(naiveBayesModel)

fitted.results_NB <- predict(naiveBayesModel,x_test)
#fitted.results_svm <- ifelse(fitted.results_svm > 0.5,1,0)
misClasificError <- mean(fitted.results_NB != dataTest$default.payment.next.month)
print(paste('Accuracy',1-misClasificError))
#[1] "Accuracy 0.702133333333333"

