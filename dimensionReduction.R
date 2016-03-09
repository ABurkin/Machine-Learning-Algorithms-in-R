library(stats)
data <- read.csv("C:\\Users\\coep\\Desktop\\eNSEMBLE\\DATA\\default_credit_card.csv",header = TRUE)
x_train <- data[,-25]
y_train <- data[,25]
count(data)
pca <- princomp(data, cor = TRUE)
head(pca)
dataReduced <- predict(pca,data)
dataReduced
summary(dataReduced)
x <- cbind(dataReduced,y_train)
x$y_train <- as.factor(x$y_train)
y_train <- as.factor(y_train)
logistic <- glm(y_train ~ ., data=x, family = 'binomial')

