setwd('/Users/yourrepository')
#####overall business module 

library(caret)
library(e1071)
set.seed(21112)

sudy = read.csv("sudy.csv",stringsAsFactors = TRUE)
sudy = sudy[,-c(1,3,5,7,11)]
sudy = sudy[,-c(9)]

str(sudy)
sudy$Satisfied_y.n = factor(sudy$Satisfied_y.n)
sudy$Project...4 = factor(sudy$Project...4)
sudy$Monthly.hour...190 = factor(sudy$Monthly.hour...190)
sudy$Time_spend_over.5 = factor(sudy$Time_spend_over.5)
sudy$Work_accident = factor(sudy$Work_accident)
sudy$left = factor(sudy$left)
str(sudy)

index = sample(1:dim(sudy)[1],dim(sudy)[1]*0.8, replace=FALSE)
train = sudy[index,]
test = sudy[-index,]

nb = naiveBayes(left~.,train$left)
nbpredict = predict(nb,test[,-c(6)])
levels(sudy$left)
confusionMatrix(nbpredict,test$left,positive = "1")
