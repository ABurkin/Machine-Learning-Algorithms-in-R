library(caret)
library(ggplot2)
data <- read.csv("C:\\Users\\coep\\Desktop\\eNSEMBLE\\DATA\\default_credit_card.csv",header = TRUE)
summary(data)
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
fitcontrol <- trainControl(method = "repeatedcv" , number = 2 ,repeats = 40)

GBAmodel <- train(y_train ~ ., data= x , method = "gbm" , trControl = fitcontrol, verbose= FALSE)
qplot(GBAmodel)
summary(GBAmodel)

fitted.results_gba <- predict(GBAmodel,x_test)
#fitted.results_svm <- ifelse(fitted.results_svm > 0.5,1,0)
misClasificError <- mean(fitted.results_gba != dataTest$default.payment.next.month)
print(paste('Accuracy',1-misClasificError))
#[1] "Accuracy 0.817466666666667" number = 4 ,repeats = 4
# [1] "Accuracy 0.8188" number = 4 ,repeats = 34
# [1] "Accuracy 0.8196" number = 10 ,repeats = 34
# [1] "Accuracy 0.819866666666667" number = 5 ,repeats = 40
