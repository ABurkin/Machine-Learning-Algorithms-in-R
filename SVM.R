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

SVMmodel <- svm(y_train ~ ., data = x)

summary(SVMmodel)

fitted.results_svm <- predict(SVMmodel,x_test)
fitted.results_svm <- ifelse(fitted.results_svm > 0.5,1,0)
misClasificError <- mean(fitted.results_svm != dataTest$default.payment.next.month)
print(paste('Accuracy',1-misClasificError))
#[1] "Accuracy 0.816666666666667"


ROC1 <- roc(dataTest$default.payment.next.month, fitted.results_svm)
plot(ROC1, col = "blue")
