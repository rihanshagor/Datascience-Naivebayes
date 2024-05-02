
data <-read.csv("C:/Users/usago/OneDrive/Desktop/Final Project/Weather.csv",header=TRUE,sep=",")
options(max.print=100000)
data

mydata <- head(data, 500)
mydata
names(mydata)

colSums(is.na(mydata))

str(mydata)

unique_values <- unique(mydata$weather)
unique_values

mydata$weather<-factor(mydata$weather,levels = c("rain", "drizzle","sun","snow","fog"),labels = c(1,2,3,4,5))
mydata$weather <- as.numeric(mydata$weather)
mydata$weather

mydata$date <- seq_along(mydata$date)
mydata$date <- as.numeric(mydata$date)
mydata$date

co_relation<-(cor(mydata[,c('date','precipitation','temp_min','temp_max','wind','weather')]))
co_relation

mydata1<-mydata
mydata1$date<- NULL
mydata1

mydata2<-mydata1
mydata2$weather<-factor(mydata2$weather,levels = c(1,2,3,4,5),labels = c("rain", "drizzle","sun","snow","fog"))
mydata2

install.packages("e1071")
library(e1071)

set.seed(123)
sample_index <- sample(1:nrow(mydata2), 0.7 * nrow(mydata2))
train_data <- mydata2[sample_index, ]
test_data <- mydata2[-sample_index, ]

nb_model_train <- naiveBayes(weather ~ precipitation + temp_max + temp_min + wind, data = train_data)

predictions_test <- predict(nb_model_train, test_data)
predictions_test 

accuracy_test <- sum(predictions_test == test_data$weather) / nrow(test_data)
accuracy_test

conf_matrix <- table(predictions_test, test_data$weather)
conf_matrix

TP <- conf_matrix[2, 2] 
FP <- conf_matrix[1, 2] 
FN <- conf_matrix[2, 1] 
TN <- conf_matrix[1, 1]  
TP
FP
FN
TN

recall <- TP / (TP + FN)
recall
FP_rate <- FP / (FP + TN)
FP_rate
precision <- TP / (TP + FP)
precision
f_measure <- 2 * (precision * recall) / (precision + recall)
f_measure    

