
data <- read.csv("C:\\Users\\coep\\Desktop\\eNSEMBLE\\DATA\\default_credit_card.csv",header = TRUE)
head(data)
training<-sample(nrow(data),3*nrow(data)/4)
dataTraining <- data[training,]
dataTest<- data[-training,]
x_train <- dataTraining[,-25]
y_train <- dataTraining[,25]
x_test <- dataTest[,-25]
x <- cbind(x_train,y_train)
lapply(x, class)
x$y_train <- as.factor(x$y_train)
head(x$y_train)
logistic <- glm(y_train ~ ., data=x, family = 'binomial')
summary(logistic)

fitted.results <- predict(logistic,x_test,type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != dataTest$default.payment.next.month)
print(paste('Accuracy',1-misClasificError))
#[1] "Accuracy 0.815466666666667"


x_train <- data[,-25]
y_train <- data[,25]
x <- cbind(x_train,y_train)
x$y_train <- as.factor(x$y_train)
head(x$y_train)
logistic <- glm(y_train ~ ., data=x, family = 'binomial')
summary(logistic)
fitted.results <- predict(logistic,x_train,type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != y_train)
print(paste('Accuracy',1-misClasificError))
