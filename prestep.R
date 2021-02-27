#PRE-STEP
library (lattice)
library(ggplot2)


#user -> umur berapa 
pie(table(users$Age), main = "Perbandingan Umur Pengguna")

#user -> gender 
pie(table(users$Gender), main = "Perbandingan Gender Pengguna")


# distribusi plot
library(ggplot2)
ggplot(data=ratings, aes(x=Rating)) +
  geom_histogram(binwidth = 1, color="black", fill="pink") +
  xlab('Rating') + ylab('Count')

# urutkan data
userPlot <- ggplot(users, aes(x = reorder(Occupation, Occupation, function(x) -length(x)), 
                                   fill = Gender)) + geom_bar()
# buat axis
userPlot <- userPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
userPlot <- userPlot + ylab("jumlah pengguna") + xlab("occupation")
# flip axis
userPlot <- userPlot + coord_flip()
userPlot

#Density 
plot(density(iris$Sepal.Width))

density <- ggplot(data=users, aes(x=users$Age))
density + 
  geom_density(stat="density", alpha=I(0.2), fill="blue") +
  xlab("Age") + ylab("Density") + 
  ggtitle("Histogram & Density Curve")

plot(density(users$Age))

#yang paling laku (sebelum pra-proses)
barplot(table(movies$Genre))


#data duplikat
library(dplyr)
clear.movies<- movies %>% distinct()
clear.users<- users %>% distinct()
clear.ratings<- ratings %>% distinct()

movi3s <- movies

#merubah jadi karakter
movies$Genre <- as.character(movies$Genre)

#melakukan split data Genre
library(tidyr)
movies.split <- movies %>%
  mutate(Genre=strsplit(Genre,"[|]")) %>%
  unnest(Genre)

#memasukkan nilai 1 dan 0 pada data
movies.split2 <- movies.split %>%
  mutate(value=1) %>% spread(Genre,value,fill=0)
View(movies.split2)

#Gender user
library(tidyr)
library(dplyr)
users2 <- users %>% mutate(value=1) %>% spread(Gender, value, fill=0)

View(users2)
#memasukkan nilai 1 dan 0 pada data
movies.split2 <- movies.split %>%
  mutate(value=1) %>% spread(Genre,value,fill=0)
View(movies.split2)


#merge
merge1 <- merge(movies.split2,ratings, by.x ="MovieID", by.y ="MovieID" )
merge2 <- merge(merge1,users2, by.x ="UserID", by.y ="UserID" )


merge0 <- merge(movies,ratings, by.x ="MovieID", by.y ="MovieID" )
merge3 <- merge(merge1,users, by.x ="UserID", by.y ="UserID" )

#drop fitur
dropf <- c("UserID", "MovieID", "Title", "Timestamp", "Zip.code")
data_dropf <- merge2 [,!(names(merge2) %in% dropf)]

data_dropf <- data_dropf[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,21,22,23,19)]

cor(data_dropf)


#sampling
data_sample <- data_dropf[sample(nrow(data_dropf), 10000), ]


#data train-test
set.seed(1234)

smp_size <- floor(0.7 * nrow(data_sample))
train_ind <- sample(seq_len(nrow(data_sample)), size = smp_size)
train <- data_sample[train_ind, ]
test <- data_sample[-train_ind, ]

#train test data
set.seed(1234)

testcb <- test[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,21,22,23,19)]

#======================Decision Tree============================#
install.packages("rpart")
install.packages("rpart.plot")
install.packages("e1071")
install.packages("caret")
install.packages("Rcpp")

library(rpart)
library(rpart.plot)
library(e1071)
library(mlbench)
library(caret)
library(Rcpp)

#=====CART (gini) ======#
#600 -> 0,368

train$Rating = factor(train$Rating)
train2 <- train[1:900,]
test2 <- test[1:400,]

fit <- rpart(Rating~., data=train2, parms = list(split = 'gini'))
rpart.plot(fit, box.palette="Blues")
summary(fit)

test_pred_cartG <- predict(fit, type='class', newdata = test2[,1:22])
pred_act_cartG <- table(test_pred_cartG, test2$Rating)
cmcartG <- confusionMatrix(table(test_pred_cartG, test2$Rating))
cmcartG


cmcartG$overall[1]
cmcartG$byClass

#=====CART (gini) ======#
#600 -> 0,368
train2 <- train
test2 <- test

train$Rating = factor(train$Rating)
fit <- rpart(Rating~., data=train2, parms = list(split = 'gini'))
rpart.plot(fit, box.palette="Blues")
summary(fit)

test_pred_cartG <- predict(fit, type='class', newdata = test2[,1:22])
pred_act_cartG <- table(test_pred_cartG, test2$Rating)
cmcartG <- confusionMatrix(table(test_pred_cartG, test2$Rating))
cmcartG$overall[1]
cmcartG$byClass

splot <- plot(x=merge3$Age, y=merge3$Rating, xlab = "Age", ylab="Rating", main="Age-Rating")
splot


?boxplot
boxplot(users$Age, col="pink", main = "Boxplot Age")
users$Age <- as.numeric(users$Age)
box <- ggplot(data=users, aes(y=Age))
box+geom_boxplot(aes(fill=Age) + ylab("Age")) + ggtitle("Boxplot Age")+
    stat_summary(fun.y=mean, geom="point", shape=5, size=4)

#=====CART (information) ======#
train2 <- train
test2 <- test

train$Rating = factor(train$Rating)
fit <- rpart(Rating~., data=train2, parms = list(split = 'information'))
rpart.plot(fit, box.palette="Blues")
summary(fit)

test_pred <- predict(fit, type='class', newdata = test2[,1:22])
confusionMatrix(table(test_pred, test2$Rating))

#holdout - CART

library(ggplot2)
install.packages("plyr")
library(plyr)
library(RColorBrewer)
library(grid)

# prepare table for analysis of users
mlDat_user <- ddply(merge3, ~UserID + Age + Gender + Occupation, summarize, 
                    mean_rating = mean(Rating))
agePlot <- ggplot(merge3, aes(Age)) + geom_histogram(aes(y = ..density..), 
                                                         binwidth = 1, colour = "black", fill = "white")
agePlot <- agePlot + geom_density(alpha = 0.2, fill = "#FF6666")
print(agePlot)

#Graph of movies with more than 10000 ratings and a mean rating greater than 4. 
avg_rating_greater_than_4 <- merge3 %>% group_by(Title) %>%
  summarize(mean_rating= mean(Rating), n = n()) %>% filter(mean_rating >=4) %>% arrange(desc(n, mean_rating))

avg_rating_greater_than_4 %>% filter(n >=10000) %>%
  ggplot(aes(reorder(title, n), n, fill = n)) +
  geom_bar(stat = "identity") + coord_flip() + scale_fill_distiller(palette = "PuBuGn") + xlab("Movie") +ylab('Number of Ratings') +
  ggtitle("Movies with an average rating\ngreater than or equal to 4\nand Number of Ratings  > 10000") 

#Genre
mlDat_avgRating <- ddply(merge3, ~Genre, summarize, Gender = "Both", rating = mean(rating))
mlDat_gender <- ddply(merge3, ~genre + gender, summarize, rating = mean(rating))
mlDat_gender <- rbind(merge3_gender, mlDat_avgRating)

genderRatingPlot <- ggplot(mlDat_gender, aes(genre, rating)) + geom_histogram(stat = "identity")
genderRatingPlot <- genderRatingPlot + facet_wrap(~gender)
# fix axis
genderRatingPlot <- genderRatingPlot + theme(axis.text.x = element_text(angle = 90, 
                                                                        hjust = 1))
genderRatingPlot <- genderRatingPlot + coord_flip()
print(genderRatingPlot)

data_sample$Rating <- as.factor(data_sample$Rating)
library(rminer)
full_accuray_tree=0
list_acc <- list()
for(b in 1:100)
{
  H=holdout(data_sample$Rating, ratio = 2/3, mode = "random", seed=NULL)
  fit <- rpart(Rating~., data=data_sample[H$tr,], parms = list(split = 'gini'))
  cart_pred <- predict(fit, type='class', newdata = data_sample[H$ts,-23])
  resultcart <- confusionMatrix(table(cart_pred, data_sample[H$ts,]$Rating))
  accuracy <- resultcart$overall['Accuracy']
  cat("batch :",b,
      "accuracy:",accuracy,"\n")
  full_accuray_tree=full_accuray_tree+accuracy
  list_acc[[b]] <- accuracy
} 
cat("Tree :",
    "accuracy:", full_accuray_tree/100, "\n")

x <- c(1:100)
plot(x, list_acc, type="o");

#holdout - CART (information)
library(rminer)
full_accuray_tree=0
list_acc <- list()
for(b in 1:100)
{
  H=holdout(train$Rating, ratio = 2/3, mode = "random", seed=NULL)
  fit <- rpart(Rating~., data=train[H$tr,], parms = list(split = 'information'))
  cart_pred <- predict(fit, type='class', newdata = train[H$ts,-23])
  resultcart <- confusionMatrix(table(cart_pred, train[H$ts,]$Rating))
  accuracy <- resultcart$overall['Accuracy']
  cat("batch :",b,
      "accuracy:",accuracy,"\n")
  full_acuracy_tree=full_acuracy_tree+accuracy
  list_acc[[b]] <- accuracy
} 
cat("Tree :",
    "accuracy:", full_acuracy_tree/100, "\n")

x <- c(1:100)
plot(x, list_acc, type="o");

#cross validation - CART (gini)
folds <- cut(seq(1, nrow(data_sample)), breaks=10, labels=FALSE)
full_acuracy_tree=0
list_acc <- list()

for (i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- data_sample[testIndexes, ]
  trainData <- train[-testIndexes, ]
  CARTginimodel <- rpart(Rating~., data=trainData, parms = list(split = 'gini'))
  test_pred <- predict(CARTginimodel, type='class', newdata = testData[,-23])
  resultCG <- confusionMatrix(table(test_pred, testData$Rating))
  accuracy <- resultCG$overall['Accuracy']
  cat("batch :",i,
      "accuracy:",accuracy,"\n")
  full_acuracy_tree=full_acuracy_tree+accuracy
  list_acc[[i]] <- accuracy
}
cat("Tree :",
    "accuracy:", full_acuracy_tree/10, "\n")

x <- c(1:10)
plot(x, list_acc, type="o");

#cross validation - C45
data_sample$Rating <- as.factor(data_sample$Rating)
folds <- cut(seq(1, nrow(data_sample)), breaks=10, labels=FALSE)
full_accuracy_tree=0
list_acc <- list()

for (i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- data_sample[testIndexes, ]
  trainData <- data_sample[-testIndexes, ]
  C45model <- J48(Rating~., data=trainData)
  test_pred <- predict(C45model, type='class', newdata = testData[,-23])
  resultCG <- confusionMatrix(table(test_pred, testData$Rating))
  accuracy <- resultCG$overall['Accuracy']
  cat("batch :",i,
      "accuracy:",accuracy,"\n")
  full_accuracy_tree=full_accuracy_tree+accuracy
  list_acc[[i]] <- accuracy
}
cat("Tree :",
    "accuracy:", full_accuracy_tree/10, "\n")

x <- c(1:10)
plot(x, list_acc, type="o");

#cross validation - CART (gini)
folds <- cut(seq(1, nrow(train)), breaks=10, labels=FALSE)
full_acuracy_tree=0
list_acc <- list()

for (i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- train[testIndexes, ]
  trainData <- train[-testIndexes, ]
  CARTginimodel <- rpart(Rating~., data=trainData, parms = list(split = 'gini'))
  test_pred <- predict(CARTginimodel, type='class', newdata = testData[,-23])
  resultCG <- confusionMatrix(table(test_pred, testData$Rating))
  accuracy <- resultCG$overall['Accuracy']
  cat("batch :",i,
      "accuracy:",accuracy,"\n")
  full_acuracy_tree=full_acuracy_tree+accuracy
  list_acc[[i]] <- accuracy
}
cat("Tree :",
    "accuracy:", full_acuracy_tree/10, "\n")

x <- c(1:10)
plot(x, list_acc, type="o");

install.packages("grid")
install.packages("libcoin")
install.packages("mvtnorm")

#=======C45
library(RWeka)

train$Rating = factor(train$Rating)
c45model <- J48(Rating~., data = train)
print(c45model)

c45predict <- predict(c45model, test[,1:22], type="class")
cmc45 <- confusionMatrix(table(c45predict, test$Rating))

cmc45
#holdout - C45
library(rminer)
full_accuray_tree=0
list_acc <- list()
data_sample$Rating <- as.factor(data_sample$Rating)
for(b in 1:100)
{
  H=holdout(data_sample$Rating, ratio = 2/3, mode = "random", seed=NULL)
  c45model <- J48(Rating~., data=data_sample[H$tr,])
  c45_pred <- predict(fit, type='class', newdata = data_sample[H$ts,-23])
  resultc45 <- confusionMatrix(table(c45_pred, data_sample[H$ts,]$Rating))
  accuracy <- resultc45$overall['Accuracy']
  cat("batch :",b,
      "accuracy:",accuracy,"\n")
  full_accuray_tree=full_accuray_tree+accuracy
  list_acc[[b]] <- accuracy
} 
cat("Tree :",
    "accuracy:", full_accuray_tree/100, "\n")

x <- c(1:100)
plot(x, list_acc, type="o");


#=======CART
library(caret)
trainControl<- trainControl(method = "cv", number="5")
View(train)
c45Fit <- train(Rating~., method="J48", data=train,
              trControl = trainControl, parms = list(split='gini') )
cpred <- predict(cFit, data=test2[,1:22])

confusionMatrix(table(cpred, test2$Rating))


evaluation <- function(data,accuracy){
  accuracy <- accuracy
  precision <- precision(data)
  recall <- recall(data)
  f <- F_meas(data)
  
  cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}

evaluation(pred_act_cartG, cmcartG$overall[1])


#=====Bayes======#


#===KNN====
#cross validation - KNN
folds <- cut(seq(1, nrow(data_sample)), breaks=10, labels=FALSE)
full_accuracy =0
list_acc <- list()
data_sample$Rating <- as.factor(data_sample$Rating)
for (i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- data_sample[testIndexes, ]
  trainData <- data_sample[-testIndexes, ]
  fit.knn <- train(Rating~., data=trainData, method="knn", 
                   preProcess=c("center", "scale"))
  knnpredict2 <- predict(fit.knn, newdata = testData[,-23], type ="prob")
  result <- confusionMatrix(table(knnpredict2, testData$Rating))
  View(knnpredict2)
  View(testData$Rating)
  accuracy <- result$overall['Accuracy']
  cat("nbbatch :",i,
      "accuracy:",accuracy,"\n")
  full_accuracy  =full_accuracy + accuracy
  list_acc[[i]] <- accuracy
}
cat("KNN :",
    "accuracy:", full_accuracy /10, "\n")

Cross_Validation_KNN <- c(1:10)
plot(Cross_Validation_KNN, list_acc, type="o")

#---------------NEURAL NETWORK---------#
install.packages("neuralnet")
library(nnet)
library(neuralnet)
nn <- nnet(Rating~., data=train, size=2, maxit = 500)
View(nn)
#npredict <- predict(nn, test3, type = 'c')
library(dplyr)
names(train)[names(train) == "Children's"] <- "children"
names(train)[names(train) == "Film-Noir"] <- "Film.Noir"
names(train)[names(train) == "Sci-Fi"] <- "Sci.Fi"
nn1 <- neuralnet(Rating~., train, hidden = 3, linear.output = F)
plot(nn1)

trainControl <- trainControl(method="cv", number=10)
fit.kkn <- train(Rating~., data=train2, method="knn", trControl=trainControl)
#summarize fit
print(fit.kkn)
print(fit.kkn2)
fit.kkn$finalModel
fit.kkn2$finalModel
#make prediction
knnpredict2 <- predict(fit.kkn, newdata=test2, na.action = na.pass)
knnpredict2<-round(knnpredict2)
#accuracy
View(knnpredict2)
table(knnpredict2, test2$Rating)
confusionMatrix(knnpredict2, test2$Rating)


library(neuralnet)
nn <- neuralnet(Rating~Action+Adventure+Animation+Comedy+Crime+Documentary+Fantasy+Drama+M+Horror+Musical+Mystery, data=train2, hidden=c(2,1),linear.output=FALSE, threshold=0.01)
nn <- neuralnet(Rating~Action+Adventure, data=train2, hidden=c(5,5),linear.output=FALSE, threshold=0.01)
nn <- neuralnet(Rating~Action+Adventure, data=train2,hidden=c(10,10), rep = 5, err.fct = "ce", linear.output = F, lifesign = "minimal", stepmax = 1000000, threshold = 0.001)
nn$result.matrix
plot(nn)

nn.prediction <- compute(nn, iris.valid[-5:-8])
idx <- apply(iris.prediction$net.result, 1, which.max)
predicted <- c('setosa', 'versicolor', 'virginica')[idx]
table(predicted, iris.valid$Species)

#Test the resulting output
temp_test <- subset(test2, select = c("Action","Adventure"))
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = test2$Rating, prediction = nn.results$net.result)
View(results)

install.packages("e1071")
library(e1071)
train3 <- traincb[1:500,]
test3 <- testcb[1:500,]

#fitmodel 

install.packages("mblench")
library(caret)
library(mblench)
train3 <- traincb
test3 <- testcb

#fit model 
knnmodel <- knn3(Rating~., data = train3, k=9)
print(knnmodel)

#make prediction 
knnpredict2 <- predict(knnmodel, test3[, 1:22], type="class")
library(caret)
confusionMatrix(table(knnpredict2, test3$Rating))
nbmodel <- naiveBayes(Rating~., data=train2)
print(nbmodel)

#make prediction 
nbpred <- predict(nbmodel, type='class', newdata = test2[,1:22])
View(nbpred)


install.packages("klaR")
library(klaR)
#accuracy 
confusionMatrix(table(nbpredict, test2$Rating))

trainControl<- trainControl(method="cv", number=5)
fit.knn <- train(Rating~., data=train2, method="knn", 
                 metric="Accuracy", preProcess=c("center","scale"), 
                 trControl=trainControl)
c45Fit <- train(Rating~., data=train2, method="J48", 
                 metric="Accuracy", trControl=trainControl)
fit.nb <- train(Rating~., data=train2, method="nb",
                 metric="Accuracy", trControl=trainControl)
compare <- resamples(list(C4.5=C45Fit, KNN = fot.knn, NB=fit.nb))
summary(compare)

install.packages("ROCR")
library(ROCR)
resultCART <- holdout(data=train2, method="J48", repeats = 50)


