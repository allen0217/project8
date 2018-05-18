
library(dplyr)
library(ggplot2)
library(fpc)
library(caret)
library(glmnet)
library(ranger)
library(e1071)
library(clValid)
  
 train <- read.csv('D:/train.csv', header = TRUE, stringsAsFactors = FALSE)
 train$Dataset <- "train"
 test <- read.csv('D:/test.csv', header = TRUE, stringsAsFactors = FALSE)
 test$Dataset <- "test"
 full <- bind_rows(train, test)
 train_c <- full[full$Dataset == 'train', ]
 test_c <- full[full$Dataset == 'test', ]
 	 
	pairs(full[,2:5], col = full$type, labels = c("Bone Length", "Rotting Flesh", "Hair Length", "Soul"))
	  full <- full %>%
          mutate(hairsoul = hassoul*hairlength)
      full <- full %>%
          mutate(boneflesh = bonelength * rottingflesh,fleshhair = rotting_flesh * hairlength,bonesoul = bonelength * hassoul,bonehair = bonelength * hairlength,
                 fleshsoul = rottingflesh * hassoul)
		 
	 summary(full)  
	  train_c <- full[full$Dataset == 'train', ]	 
      test_c <- full[full$Dataset == 'test', ] 	 
      myControler <- trainControl(method = "cv",number = 10,repeats = 50,verboseIter = TRUE)
   
   //random forest model
   set.seed(10)
   randomf_model <- train(
         type ~ bonelength + rottingflesh + hairlength + hassoul + color + hairsoul + boneflesh + bonehair + 
               bonesoul + fleshhair + fleshsoul,
	     data = train_c, 
         method = "ranger", trControl = myControler,tuneLength = 5,importance = 'impurity'
     )
   models1 <- list(rf = randomf_model)
   resampled1 <- resamples(models1)
   summary(resampled1)
   
   // glmnet model
   set.seed(10)
   glmn_model <- train(
        type ~ bonelength + rottingflesh + hairlength + hassoul + color + hairsoul + boneflesh + bonehair +  bonesoul + fleshhair + fleshsoul, 
        tuneGrid = expand.grid(alpha = 0:1, lambda = seq(0.0001, 1, length = 50)),
		method = "glmnet",data = train_c, trControl = myControler
    )
   models2 <- list(glmnet = glmn_model)
   resampled2 <- resamples(models2)
   summary(resampled2)
  