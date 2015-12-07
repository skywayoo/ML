library(httr)
library(XML)
library(stringr)
library(rjson)
#parse data
data <- list()
for(i in 1 :3){
  time <- c("2013-14","2014-15","2015-16")
  url <- sprintf("http://stats.nba.com/stats/playerdashptshotlog?DateFrom&DateTo&GameSegment&LastNGames=0&LeagueID=00&Location&Month=0&OpponentTeamID=0&Outcome&Period=0&PlayerID=201939&Season=%s&SeasonSegment&SeasonType=Regular+Season&TeamID=0&VsConference&VsDivision",time)
  res <- GET(url[i])
  restext <- content(res,'text')
  data[i] <- restext
}
nbadata <- data
nbadata <- unlist(data)
#json  
jsonData.1 = fromJSON(nbadata[1])
jsondata.1 <- data.frame(do.call(rbind,jsonData.1$resultSets[[1]]$rowSet))
jsonData.2 = fromJSON(nbadata[2])
jsondata.2 <- data.frame(do.call(rbind,jsonData.2$resultSets[[1]]$rowSet))
jsonData.3 = fromJSON(nbadata[3])
jsondata.3 <- data.frame(do.call(rbind,jsonData.3$resultSets[[1]]$rowSet))
jsondata <- rbind(jsondata.1,jsondata.2,jsondata.3)
#colname
colnames(jsondata) <- c("GAME_ID",
                     "MATCHUP",
                     "LOCATION",
                     "WL",
                     "FINAL_MARGIN",
                     "SHOT_NUMBER",
                     "PERIOD",
                     "GAME_CLOCK",
                     "SHOT_CLOCK",
                     "DRIBBLES",
                     "TOUCH_TIME",
                     "SHOT_DIST",
                     "PTS_TYPE",
                     "SHOT_RESULT",
                     "CLOSEST_DEFENDER",
                     "CLOSEST_DEFENDER_PLAYER_ID",
                     "CLOSE_DEF_DIST",
                     "FGM",
                     "PTS")
#clear data-GameID,MATCHUP,W/L,GAMECLOCK,SHOTCLOCK,PLAYER,PLAYER_ID,FGM,PTS
data <- jsondata[,c(-1,-2,-8,-9,-15,-16,-18,-19)]
for(i in 1:ncol(data)){
        data[,i] <- unlist(data[,i])
}
#clear null data
data <- subset(data,data$TOUCH_TIME!=0)
missingvalue <- subset(data,data$TOUCH_TIME==0)
str(missingvalue$TOUCH_TIME)
#give character as factor
data$LOCATION <-as.factor(data$LOCATION)
data$SHOT_RESULT <-as.factor(data$SHOT_RESULT)
data$WL <- as.factor(data$WL)
str(data)



############################################creat model####################################################
#write.table(data,"nba_data.txt")
#10% testing
index <- 1:nrow(data)
np <- ceiling(0.1*nrow(data))   

##random 10% testing & 90% training
set.seed(10)
test.index <- sample(1:nrow(data),np)
data.test<-data[test.index,]
data.train <-data[-test.index,]

############################################CART Decision Tree#############################################
library(rpart)
tree_model <- rpart(SHOT_RESULT~.,data.train,control = rpart.control(minsplit = 1000))
summary(tree_model)

plot(tree_model,margin=0.2)
text(tree_model,use.n=T,cex=1)
# train data
tree_pred_train<- predict(tree_model,data.train,type='class')
tree_table.train <- table(data.train$SHOT_RESULT,tree_pred_train)
tree_table.train

tree_correct_train <- sum(diag(tree_table.train))/sum(tree_table.train)
cat("accuracy:",tree_correct_train*100,"%")

# test data
tree_pred<- predict(tree_model,data.test,type='class')
tree_table.test <- table(data.test$SHOT_RESULT,tree_pred)
tree_table.test

tree_correct <- sum(diag(tree_table.test))/sum(tree_table.test)
cat("accuracy:",tree_correct*100,"%")

############################################Random Forest#############################################
library(randomForest)
rf_model <- randomForest(SHOT_RESULT~.,data=data.train,ntree = 100)

#train data
rf_pred.train <- predict(rf_model,newdata=data.train)
rf_table.train <- table(data.train$SHOT_RESULT,rf_pred.train)
rf_table.train
rf_correct_train <- sum(diag(rf_table.train))/sum(rf_table.train)
cat("accuracy:",rf_correct_train*100,"%")

#test data
rf_pred <- predict(rf_model,newdata=data.test)
rf_table.test <- table(data.test$SHOT_RESULT,rf_pred)
rf_table.test
rf_correct <- sum(diag(rf_table.test))/sum(rf_table.test)
cat("accuracy:",rf_correct*100,"%")

############################################ SVM #######################################################
library(e1071)
svm.model <- svm(SHOT_RESULT~ .,data=data.train,cost=100,gamma=1e-05)
svm.pred <- predict(svm.model,data.test[,-10])
svm.pred.train <- predict(svm.model,data.train[,-10])
plot(svm.model,data.train,TOUCH_TIME~SHOT_DIST)
str(data.train)
#train
table.svm.train <- table(pred=svm.pred.train,true=data.train[,10])
correct.svm.train <- sum(diag(table.svm.train))/sum(table.svm.train)
cat("accuracy:",correct.svm.train*100,"%")

#test
table.svm.test <- table(pred=svm.pred,true=data.test[,10])
correct.svm <- sum(diag(table.svm.test))/sum(table.svm.test)
cat("accuracy:",correct.svm*100,"%")

#search beat gmma and cost
tuned <- tune.svm(SHOT_RESULT~.,data=data.train,gamma=10^(-5:-1),cost=10^(-2:2))
summary(tuned)

################################naiveBayes################################
library(e1071)
nb_model <-naiveBayes(SHOT_RESULT ~ ., data = data.train)
nb.pred.train<-predict(nb_model,data.train,type="class")
nb.pred.test <- predict(nb_model,data.test,type="class")

#train
table.nb.train <- table(pred=nb.pred.train,true=data.train[,10])
correct.nb.train <- sum(diag(table.nb.train))/sum(table.nb.train)
cat("accuracy:",correct.nb.train*100,"%")

#test
table.nb.test <- table(pred=nb.pred.test,true=data.test[,10])
correct.nb <- sum(diag(table.nb.test))/sum(table.nb.test)
cat("accuracy:",correct.nb*100,"%")


######################################## neural  ##############################################
library(nnet)
nn_model<-nnet(SHOT_RESULT ~., data = data.train,size = 30,maxit = 300)
#train
nn_pred.train <- predict(nn_model,data.train[,-10],type="class")
nntable.train <- table(nn_pred.train, data.train[,10])
correct.nn.train <- sum(diag(nntable.train))/sum(nntable.train)
cat("accuracy:",correct.nn.train*100,"%")
#test
nn_pred.test <- predict(nn_model,data.test[,-10],type="class")
nntable.test <- table(nn_pred.test, data.test[,10])
correct.nn.test <- sum(diag(nntable.test))/sum(nntable.test)
cat("accuracy:",correct.nn.test*100,"%")



cat(" decision tree","   train：",tree_correct_train*100,"   test：",tree_correct*100,"\n",
    "random forest","   train：",rf_correct_train*100,"        test：",rf_correct*100,"\n",
    "svm          ","   train：",correct.svm.train*100,"   test：",correct.svm*100,"\n",
    "naive bayes  ","   train：",correct.nb.train*100,"   test：",correct.nb*100,"\n",
    "neuralnetwork","   train：",correct.nn.train*100,"   test：",correct.nn.test*100)



