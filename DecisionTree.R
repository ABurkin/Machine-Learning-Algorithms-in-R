library(rpart)
# data frame preparattion
data <- read.csv("C:\\Users\\coep\\Desktop\\eNSEMBLE\\DATA\\default_credit_card.csv",header = TRUE)
data1<- data
sample1 <- seq(from = 1, to = 300, by = 1)
#sample1 <- sample(nrow(data1), 300, replace = FALSE)
data<-data1[sample1,]
x_train <- data[,-25]
y_train <- data[,25]
x <- cbind(x_train,y_train)
x$y_train <- as.factor(x$y_train)

#model Creation
dt <- rpart(y_train ~ .,data = x_train ,  method = "class")

summary(rfModel)

#Accuracy check
sample2 <- seq(from = 1000, to = 1500, by = 1)
#sample2 <- sample(nrow(data1), 300, replace = FALSE)
dataTest <- data1[sample2,]
x_test<- dataTest[,-25]
y_test<- dataTest[,25]
fitted.results <- predict(dt,x_test,type = 'class')

misClasificError <- mean(fitted.results != y_test)
print(paste('Accuracy',1-misClasificError))
