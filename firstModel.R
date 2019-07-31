################################################################################################################################################

                                                          #Chi Square X Class Imbalance methods(Over, under, both)

###############################################################################################################################################



##1. Chi square feature selection

      library(FSelector)
      library(dplyr)
      chiWeights1<- chi.squared(ADDEPEV2~., finalData1)
      chiSel1<- cutoff.k.percent(chiWeights1, .75) #pick the top 75% of features
      chiSel1 #view selected features
      
      
      
      #Create a new dataset with just the variables selected by chi square feature selection test
      chiSel1<-as.vector(chiSel1)
      ADDEPEV2 <- as.vector(finalData1$ADDEPEV2)#retain dependent variable in a seperate variable
      finalDataChi<-subset(finalData1, select=chiSel1) #store data frame with the selected variables
      finalDataChi<-cbind(ADDEPEV2,finalDataChi) #add the dependent variable to data frame
      
      structChi<-capture.output(str(finalDataChi,list.len=ncol(finalDataChi)))
      
      #Divide into test and train datasets
      

      trainSize <- floor(0.7 * nrow(finalDataChi))
      set.seed(123)
      train_ind <- sample(seq_len(nrow(finalDataChi)), size = trainSize)
      trainChi <- finalDataChi[train_ind, ]
      testChi<- finalDataChi[-train_ind, ]
      
      
      #1.1. Oversampling
      library(ROSE)
      overChi <- ovun.sample(ADDEPEV2~.,data = trainChi, method = "over", p=.5)$data 
      table(overChi$ADDEPEV2) #view class frequency
      
      #1.2.Undersampling
      library(ROSE)
      underChi <- ovun.sample(ADDEPEV2~.,data = trainChi, method = "under", p=.5)$data 
      table(underChi$ADDEPEV2)
      
      #1.3. Both Undersampling and Oversampling
      library(ROSE)
      bothChi <- ovun.sample(ADDEPEV2~.,data = trainChi, method = "both", p=.5)$data 
      table(bothChi$ADDEPEV2)
      
      
###############################      Random Forest Model       ###############################################
      
      #train data using random forests
      library(randomForest)
      
##First random forest model using oversampled dataset, 50 trees, and sampsize = number of observations from the smaller class (2)
  #Train model
      start.time <- Sys.time()
      rf.fit<-randomForest(ADDEPEV2~., data = overChi, sampsize = c(56062,56062), ntree = 50, mtry=13) #samp size=lower frequency of the two classes.
                                                                                                        #mtry set to (#ofvariables * .1)
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      time.taken
      
  #Test model
      start.time <- Sys.time()
      pred <- predict(rf.fit,testChi)
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      time.taken
      
      library(mlbench)
      library(caret)
      confMatrix <- confusionMatrix(pred, testChi$ADDEPEV2, positive = levels(testChi$ADDEPEV2)[1])
      
    #Evaluation
      
        # Precision: tp/(tp+fp):
        confMatrix1.1 <- as.table(confMatrix) #convert 
        precisionRF<- confMatrix1.1[1,1]/sum(confMatrix1.1[1,1:2])
        
        
        # Recall: tp/(tp + fn):
        recallRF <- confMatrix1.1[1,1]/sum(confMatrix1.1[1:2,1])
        
        # F-Score: 2 * precision * recall /(precision + recall):
        2 * precisionRF * recallRF / (precisionRF + recallRF)
        
        
     #ROC curve and AUC analysis
        library(ROCR)
        testChi$predAUC <- predict(rf.fit,testChi, type = "prob")[,2]
  
        library(pROC)
        roc0 <- roc(testChi$ADDEPEV2, testChi$predAUC, levels = levels(overChi$ADDEPEV2))
        predictions <- prediction(testChi$predAUC,testChi$ADDEPEV2)
        auc <- performance(predictions, "auc")
        unlist(slot(auc, "y.values"))
        plot(roc0, col = 1, lty = 2, main = "ROC")
  
        
     #*******Remove predAUC column from test dataset
        testChi<-subset(testChi,select = -c(predAUC))
        
      
##Second random forest model, using oversampled dataset, 100 trees and sampsize = number of features * .1
    #Train model    
        start.time <- Sys.time()
        rf.fit1<-randomForest(ADDEPEV2~., data = overChi, sampsize = c(56062,56062), ntree = 100, mtry=13) #samp size=lower frequency of the two classes.
                                                                                                             #mtry set to (#ofvariables * .1)
       
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
        
    #Test model    
        start.time <- Sys.time()
        pred1 <- predict(rf.fit1,testChi)
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
        
        
        confMatrix1 <- confusionMatrix(pred1, testChi$ADDEPEV2, positive = levels(testChi$ADDEPEV2)[1])
        
      #Evaluation
        
        # Precision: tp/(tp+fp):
        confMatrix1.2 <- as.table(confMatrix1) #convert 
        precisionRF1<- confMatrix1.2[1,1]/sum(confMatrix1.2[1,1:2])
        
        
        # Recall: tp/(tp + fn):
        recallRF1 <- confMatrix1.2[1,1]/sum(confMatrix1.2[1:2,1])
        
        # F-Score: 2 * precision * recall /(precision + recall):
        2 * precisionRF1 * recallRF1 / (precisionRF1 + recallRF1)
        
        
      ##ROC curve and AUC analysis
        library(ROCR)
        testChi$predAUC <- predict(rf.fit1,testChi, type = "prob")[,2]
        
        
        library(pROC)
        roc1 <- roc(testChi$ADDEPEV2, testChi$predAUC, levels = levels(overChi$ADDEPEV2))
        plot(roc1, col = 2, lty = 3, add = TRUE) #plot ROC
        
        predictions1 <- prediction(testChi$predAUC,testChi$ADDEPEV2)
        auc1 <- performance(predictions1, "auc")
        unlist(slot(auc1, "y.values"))
        
        #*******Remove predAUC column from test dataset
        testChi<-subset(testChi,select = -c(predAUC))
        
##Third random forest model, using oversampled dataset, 500 trees and sampsize = number of features * .1
        
    #Train model
        start.time <- Sys.time()
        rf.fit2<-randomForest(ADDEPEV2~., data = overChi, sampsize = c(56062,56062), ntree = 500, mtry=13) 
        
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
    
    #Test model
        start.time <- Sys.time()
        pred2 <- predict(rf.fit2,testChi)
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
        
        confMatrix2 <- confusionMatrix(pred2, testChi$ADDEPEV2, positive = levels(testChi$ADDEPEV2)[1])
        
    #Evaluation
        
        # Precision: tp/(tp+fp):
        confMatrix2.2 <- as.table(confMatrix2) #convert 
        precisionRF2<- confMatrix2.2[1,1]/sum(confMatrix2.2[1,1:2])
        
        
        # Recall: tp/(tp + fn):
        recallRF2 <- confMatrix2.2[1,1]/sum(confMatrix2.2[1:2,1])
        
        # F-Score: 2 * precision * recall /(precision + recall):
        2 * precisionRF2 * recallRF2 / (precisionRF2 + recallRF2)
        
        
    ##ROC curve and AUC analysis
        library(ROCR)
        testChi$predAUC <- predict(rf.fit2,testChi, type = "prob")[,2]
        
        
        library(pROC)
        roc2 <- roc(testChi$ADDEPEV2, testChi$predAUC, levels = levels(overChi$ADDEPEV2))
        plot(roc2, col = 3, lty = 4, add = TRUE) #plot ROC curve
        predictions2 <- prediction(testChi$predAUC,testChi$ADDEPEV2)
        auc2 <- performance(predictions2, "auc")
        unlist(slot(auc2, "y.values"))
        
        
        #*******Remove predAUC column from test dataset
        testChi<-subset(testChi,select = -c(predAUC))
        
      
        
##Fourth random forest model, using undersampled dataset, 50 trees and sampsize = number of features * .1       
       
  #Train model
        start.time <- Sys.time()
        rf.fit3<-randomForest(ADDEPEV2~., data = underChi, sampsize = c(13828,13828), ntree = 50, mtry=13) 
       
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
        
   #Test model     
        start.time <- Sys.time()
        pred3 <- predict(rf.fit3,testChi)
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
        
        
        confMatrix3 <- confusionMatrix(pred3, testChi$ADDEPEV2, positive = levels(testChi$ADDEPEV2)[1])
        
    #Evaluation
        
        # Precision: tp/(tp+fp):
        confMatrix3.1 <- as.table(confMatrix3) #convert 
        precisionRF3<- confMatrix3.1[1,1]/sum(confMatrix3.1[1,1:2])
        
        
        # Recall: tp/(tp + fn):
        recallRF3 <- confMatrix3.1[1,1]/sum(confMatrix3.1[1:2,1])
        
        # F-Score: 2 * precision * recall /(precision + recall):
        2 * precisionRF3 * recallRF3 / (precisionRF3 + recallRF3)
        
        
    ##ROC curve and AUC analysis
        library(ROCR)
        testChi$predAUC <- predict(rf.fit3,testChi, type = "prob")[,2]
        
        library(pROC)
        roc3 <- roc(testChi$ADDEPEV2, testChi$predAUC, levels = levels(underChi$ADDEPEV2))
        
        plot(roc3, col = 1, lty = 2, main = "ROC")
        
        predictions3 <- prediction(testChi$predAUC,testChi$ADDEPEV2)
        auc3 <- performance(predictions3, "auc")
        unlist(slot(auc3, "y.values")) #view auc value
        
      #*******Remove predAUC column from test dataset
        testChi<-subset(testChi,select = -c(predAUC))
        
##Fifth random forest model, using undersampled dataset, 100 trees and sampsize = number of features * .1
    #Train model
        start.time <- Sys.time()
        rf.fit4<-randomForest(ADDEPEV2~., data = underChi, sampsize = c(13828,13828), ntree = 100, mtry=13) 
        
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
        
    #Test model
        start.time <- Sys.time()
        pred4 <- predict(rf.fit4,testChi)
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
        
       
        confMatrix4 <- confusionMatrix(pred4, testChi$ADDEPEV2, positive = levels(testChi$ADDEPEV2)[1])
        
    #Evaluation
        
        # Precision: tp/(tp+fp):
        confMatrix4.1 <- as.table(confMatrix4) #convert 
        precisionRF4<- confMatrix4.1[1,1]/sum(confMatrix4.1[1,1:2])
        
        
        # Recall: tp/(tp + fn):
        recallRF4 <- confMatrix4.1[1,1]/sum(confMatrix4.1[1:2,1])
        
        # F-Score: 2 * precision * recall /(precision + recall):
        2 * precisionRF4 * recallRF4 / (precisionRF4 + recallRF4)
        
        
     ##ROC curve and AUC analysis
        library(ROCR)
        testChi$predAUC <- predict(rf.fit4,testChi, type = "prob")[,2]
        
        library(pROC)
        roc4 <- roc(testChi$ADDEPEV2, testChi$predAUC, levels = levels(underChi$ADDEPEV2))
      
        plot(roc4, col = 2, lty = 2, add = TRUE)
        
        predictions4 <- prediction(testChi$predAUC,testChi$ADDEPEV2)
        auc4 <- performance(predictions4, "auc")
        unlist(slot(auc4, "y.values")) #view auc value
        
        
     #*******Remove predAUC column from test dataset
        testChi<-subset(testChi,select = -c(predAUC))
        
        

##Sixth random forest model, using over and undersampled dataset, 50 trees and sampsize = number of features * .1
   #Train model     
        start.time <- Sys.time()
        rf.fit6<-randomForest(ADDEPEV2~., data = bothChi, sampsize = c(34942,34942), ntree = 50, mtry=13) 
        
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
    
    #Test model
        start.time <- Sys.time()
        pred6 <- predict(rf.fit6,testChi)
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
        
        
        confMatrix6 <- confusionMatrix(pred6, testChi$ADDEPEV2, positive = levels(testChi$ADDEPEV2)[1])
        
    #Evaluation
        
        # Precision: tp/(tp+fp):
        confMatrix6.1 <- as.table(confMatrix6) #convert 
        precisionRF6<- confMatrix6.1[1,1]/sum(confMatrix6.1[1,1:2])
        
        
        # Recall: tp/(tp + fn):
        recallRF6 <- confMatrix6.1[1,1]/sum(confMatrix6.1[1:2,1])
        
        # F-Score: 2 * precision * recall /(precision + recall):
        2 * precisionRF6 * recallRF6 / (precisionRF6 + recallRF6)
        
        
    ##ROC curve and AUC analysis
        library(ROCR)
        testChi$predAUC <- predict(rf.fit6,testChi, type = "prob")[,2]
        
        library(pROC)
        roc6 <- roc(testChi$ADDEPEV2, testChi$predAUC, levels = levels(bothChi$ADDEPEV2))
        
        plot(roc6, col = 1, lty = 2, main = "ROC")
        
        predictions6 <- prediction(testChi$predAUC,testChi$ADDEPEV2)
        auc6 <- performance(predictions6, "auc")
        unlist(slot(auc6, "y.values")) #view auc value
        
        
        #*******Remove predAUC column from test dataset
        testChi<-subset(testChi,select = -c(predAUC))
        
##Seventh random forest model, using over and undersampled dataset, 100 trees and sampsize = number of features * .1
   #Train model     
        start.time <- Sys.time()
        rf.fit7<-randomForest(ADDEPEV2~., data = bothChi, sampsize = c(34942,34942), ntree = 100, mtry=13) 
        
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
    
    #Test model   
        start.time <- Sys.time()
        pred7 <- predict(rf.fit7,testChi)
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
        
        
        confMatrix7 <- confusionMatrix(pred7, testChi$ADDEPEV2, positive = levels(testChi$ADDEPEV2)[1])
        
    #Evaluation
        
        # Precision: tp/(tp+fp):
        confMatrix7.1 <- as.table(confMatrix7) #convert 
        precisionRF7<- confMatrix7.1[1,1]/sum(confMatrix7.1[1,1:2])
        
        
        # Recall: tp/(tp + fn):
        recallRF7 <- confMatrix7.1[1,1]/sum(confMatrix7.1[1:2,1])
        
        # F-Score: 2 * precision * recall /(precision + recall):
        2 * precisionRF7 * recallRF7 / (precisionRF7 + recallRF7)
        
        
     ##ROC curve and AUC analysis
        library(ROCR)
        testChi$predAUC <- predict(rf.fit7,testChi, type = "prob")[,2]
        
        library(pROC)
        roc7 <- roc(testChi$ADDEPEV2, testChi$predAUC, levels = levels(bothChi$ADDEPEV2))
        
        plot(roc7, col = 2, lty = 2, add = TRUE)
        
        predictions7 <- prediction(testChi$predAUC,testChi$ADDEPEV2)
        auc7 <- performance(predictions7, "auc")
        unlist(slot(auc7, "y.values")) #view auc value
        
        
    #*******Remove predAUC column from test dataset
        testChi<-subset(testChi,select = -c(predAUC))


##Eigth random forest model, using over and undersampled dataset, 500 trees and sampsize = number of features * .1
    #Train model     
        start.time <- Sys.time()
        rf.fit8<-randomForest(ADDEPEV2~., data = bothChi, sampsize = c(34942,34942), ntree = 500, mtry=13) 
        
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
        
     #Test model   
        start.time <- Sys.time()
        pred8 <- predict(rf.fit8,testChi)
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
        
        
        confMatrix8 <- confusionMatrix(pred8, testChi$ADDEPEV2, positive = levels(testChi$ADDEPEV2)[1])
        
      #Evaluation
        
        # Precision: tp/(tp+fp):
        confMatrix8.1 <- as.table(confMatrix8) #convert 
        precisionRF8<- confMatrix8.1[1,1]/sum(confMatrix8.1[1,1:2])
        
        
        # Recall: tp/(tp + fn):
        recallRF8 <- confMatrix8.1[1,1]/sum(confMatrix8.1[1:2,1])
        
        # F-Score: 2 * precision * recall /(precision + recall):
        2 * precisionRF8 * recallRF8 / (precisionRF8 + recallRF8)
        
        
      ##ROC curve and AUC analysis
        library(ROCR)
        testChi$predAUC <- predict(rf.fit8,testChi, type = "prob")[,2]
        
        library(pROC)
        roc8 <- roc(testChi$ADDEPEV2, testChi$predAUC, levels = levels(bothChi$ADDEPEV2))
        
        plot(roc8, col = 3, lty = 2, add = TRUE)
        
        predictions8 <- prediction(testChi$predAUC,testChi$ADDEPEV2)
        auc8 <- performance(predictions8, "auc")
        unlist(slot(auc8, "y.values")) #view auc value
        
        
        #*******Remove predAUC column from test dataset
        testChi<-subset(testChi,select = -c(predAUC))
      
        
        
        
        
        
      
##3. Recursive feature selection
      
      library(caret)
      set.seed(400)
     # finalData2<- finalData1[,c(which(colnames(finalData1)=="ADDEPEV2"),which(colnames(finalData1)!="ADDEPEV2"))] #move target variable to first col
      controlRFE <- rfeControl(functions=rfFuncs, method="cv", number=10)
      # run the RFE algorithm
      resultsRFE <- rfe(finalData1[,1:2,4:171], finalData1[,3], sizes=c(1:2,4:171), 
                        rfeControl=controlRFE)
     
      # summarize the results
      print(resultsRFE)
      # list the chosen features
      predictors(resultsRFE) 
      # plot the results
      plot(resultsRFE, type=c("g", "o"))
      
      #Conclusion: RFE has not produced more than 1 significant variable, leading to no further analysis
      
      
      
##################################################################################################################################################################
      # Logistic regression Model
      
##################################################################################################################################################################
      
# First Baseline model to find significant features using over train set for chi squared selected features
      log.model <- glm(ADDEPEV2~., data = overChi, family = binomial)
      
      #write coefficients + significance levels to excel file for feature selection
      options(max.print=999999)
      structLog <- capture.output(summary(log.model,list.len=nrow(summary(log.model)))) 
      write.csv(structLog, "C:/Users/tanna_000/Documents/CKME136/logModStructOver.csv", sep = "," )
      
      
 #Train model based on variables selected from baseline model using 10-fold cross validation
      
      start.time <- Sys.time()
      train_control <- trainControl(method = "cv", number = 10)
      log.modeltrain <- train(ADDEPEV2 ~ MENTHLTH+ X_MENT14D+ DECIDE +POORHLTH+ PHYSHLTH+ GENHLTH+ X_LMTSCL1+
                           X_PHYS14D+EMPLOY1+DIFFALON+X_LMTACT1+X_LMTWRK1+HAVARTH3+INCOME2+X_SMOKER3+
                           X_ECIGSTS+CHCCOPD1+ECIGARET+ASTHMA3+MEDCOST+X_CASTHM1+
                           MARITAL+ X_BMI5+ HIVTST6+ SEX+BLIND+RENTHOM1+CADULT+FC60_+X_BMI5CAT+X_PACAT1+
                           TOLDHI2+FVGREEN1+HTIN4+GRENDA1_+HTM4+EXRACT11+FRUIT2+DIABETE3+EXRACT21+FRUTDA2_+ACTIN11_+X_STATE+CHCKIDNY+HIVRISK5+PAFREQ1_ 
                           +greenIntake+X_FRTLT1A+X_AGE65YR+CVDCRHD4+X_RACE+EXERHMM1+fruitIntake+PADUR1_+ X_IMPRACE+WTKG3+DEAF+X_PRACE1+PERSDOC2+vegIntake+
                             PREDIAB1+X_RACEGR3+FRENCHF1+X_DRNKWEK+PNEUVAC3+DRNKANY5+DROCDY3_+VEGEDA2_, data = overChi, trControl = train_control, method = "glm",
                               family=binomial()) 
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      time.taken
      
  #TEST baseline model
      start.time <- Sys.time()
      predict.model<- predict(log.modeltrain,testChi)
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      time.taken
     
      
      confMatrix25 <- confusionMatrix(predict.model, testChi$ADDEPEV2, positive = levels(testChi$ADDEPEV2)[1])
      
  #Evaluation
      
      # Precision: tp/(tp+fp):
      confMatrix25.1 <- as.table(confMatrix25) #convert 
      precisionLR<- confMatrix25.1[1,1]/sum(confMatrix25.1[1,1:2])
      
      
      # Recall: tp/(tp + fn):
      recallLR <- confMatrix25.1[1,1]/sum(confMatrix25.1[1:2,1])
      
      # F-Score: 2 * precision * recall /(precision + recall):
      2 * precisionLR * recallLR / (precisionLR + recallLR)
      
      
  ##ROC curve and AUC analysis
      library(ROCR)
      testChi$predict.model <- predict(log.modeltrain,testChi, type = "prob")[,2]
      
      library(pROC)
      rocLR <- roc(testChi$ADDEPEV2, testChi$predict.model, levels = levels(overChi$ADDEPEV2))
      
      plot(rocLR, col = 1, lty = 2, main = "ROC")
      
      predictionsLR <- prediction(testChi$predict.model,testChi$ADDEPEV2)
      aucLR <- performance(predictionsLR, "auc")
      unlist(slot(aucLR, "y.values")) #view auc value
      
      #*******Remove predAUC column from test dataset
      testChi<-subset(testChi,select = -c(predAUC))
     
      
    #Misclassification error
      
      1- sum(diag(confMatrix25.1)/sum(confMatrix25.1))
      
      #Conclusion: Fairly low misclassification rate of 22%. Low positive class predictive value signifies lack of true positives in this model. 
      
      #*******Remove predict.model column from test dataset
      testChi<-subset(testChi,select = -c(predict.model))
      
# Second Baseline model to find significant features using both - under and oversampled train set for chi squared selected features
      log.model3 <- glm(ADDEPEV2~., data = bothChi, family = binomial)
      
      #write coefficients to excel file for feature selection
      options(max.print=999999)
      structLog3 <- capture.output(summary(log.model3,list.len=nrow(summary(log.model3)))) 
      write.csv(structLog3, "C:/Users/tanna_000/Documents/CKME136/logModStructBoth.csv", sep = "," )
      
      
  #Build train model with the following variables selected from baseline model
      start.time <- Sys.time()
      log.modeltrain2 <- train(ADDEPEV2 ~ MENTHLTH+X_MENT14D+DECIDE +POORHLTH+ PHYSHLTH+ GENHLTH+
                            X_PHYS14D+EMPLOY1+DIFFALON+X_LMTACT1+X_LMTWRK1+HAVARTH3+INCOME2+
                            X_SMOKER3+X_ECIGSTS+CHCCOPD1+ECIGARET+MEDCOST+ MARITAL+X_BMI5+
                            SMOKE100+HIVTST6+SEX+BLIND+RENTHOM1+CADULT+FC60_+MAXVO2_+X_BMI5CAT+
                            X_PACAT1+X_AGEG5YR+HTIN4+EXRACT11+EXRACT21+FRUTDA2_+EDUCA+X_STATE+
                            BPHIGH4+HIVRISK5+greenIntake+X_AGE_G+X_AGE65YR+X_RACE+ X_IMPRACE+
                            POTATOE1+X_MRACE1+DEAF+X_PRACE1+PERSDOC2+PREDIAB1+X_RACEGR3+
                            FRENCHF1+PNEUVAC3+ DRNKANY5+ DROCDY3_+X_DUALUSE, data = bothChi,trControl = train_control, method = "glm",
                            family=binomial()) 
      
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      time.taken
      
     
    #TEST baseline model
      start.time <- Sys.time()
      predict.model2<- predict(log.modeltrain2,testChi)
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      time.taken
      
      
      confMatrix26 <- confusionMatrix(predict.model2, testChi$ADDEPEV2, positive = levels(testChi$ADDEPEV2)[1])
      
    #Evaluation
      
      # Precision: tp/(tp+fp):
      confMatrix26.1 <- as.table(confMatrix26) #convert 
      precisionLR1<- confMatrix26.1[1,1]/sum(confMatrix26.1[1,1:2])
      
      
      # Recall: tp/(tp + fn):
      recallLR1 <- confMatrix26.1[1,1]/sum(confMatrix26.1[1:2,1])
      
      # F-Score: 2 * precision * recall /(precision + recall):
      2 * precisionLR1 * recallLR1 / (precisionLR1 + recallLR1)
      
      
     ##ROC curve and AUC analysis
      library(ROCR)
      testChi$predict.model2 <- predict(log.modeltrain2,testChi, type = "prob")[,2]
      
      library(pROC)
      rocLR1 <- roc(testChi$ADDEPEV2, testChi$predict.model2, levels = levels(bothChi$ADDEPEV2))
      
    plot(rocLR1, col = 2, lty = 2, add=TRUE)
      
      predictionsLR1 <- prediction(testChi$predict.model2,testChi$ADDEPEV2)
      aucLR1 <- performance(predictionsLR1, "auc")
      unlist(slot(aucLR1, "y.values")) #view auc value
    
   #Misclassification error
      
      1- sum(diag(confMatrix26.1)/sum(confMatrix26.1))
      
    
      
    #*******Remove predict.model column from test dataset
      testChi<-subset(testChi,select = -c(predict.model2))
      
      #Conclusion: Fairly low misclassification rate of 22%. Low positive class predictive value of below 50% signifies lack of true positives in this model.
    