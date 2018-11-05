#Further selection of variables by treating multicollinearity in the data 
#I have copied an excellent function from this site. 
#This function calculates the VIFs for all the independent variables, and if they have values bigger than the choosed limit, 
#the function removes the biggest value and calculate again. 
#It repeates this operation until all the variables have the accepted VIF.


vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}
library(VIF)
library(car)
x = vif_func(credit,thresh = 2,trace = T) #Setting threshold VIF 2 for removing multicollinearity
x #We can see the variables "X30.DPD.12"   "X60.DPD.6"   "X30.DPD.6"   "X90.DPD.12"  "X60.DPD.12" were all multicollinear
#so dropping them and building the models again 

credit = credit[,x] 
credit = credit[,-c(2,3,4,5,7,8,9)]
dim(credit)
names(credit)
#So the important variables are from t-test and chi-sq test HL, Autoloan, Inc, Mon_Cur_Res, Mon_Cur_Com, 
#X90.DPD.6, X60.DPD.6, X30.DPD.6, X90.DPD.12, X60.DPD.12, X30.DPD.12, 
#Avg.CC.12, Trades.6, Trades.12, PL.Tr.6, PL.Tr.12,
#Inq.6, Inq.12, BAL, and Tot.Tr.


#Further variable selection using WOE binning of numerical variables











#woe binning on numerical variables
names(credit)
library(woeBinning)
?woe.binning
x = woe.binning(credit, 'Defaulter',credit,
                min.perc.total=0.005, min.perc.class=0.01,
                stop.limit=0.1, event.class=1)
wb = createWorkbook()
y = woe.binning.table(x)
y
str(credit)
#woe of X90DPD6 for 1,2,3 are similar so combining them as one level
credit$X90DPD_6 = as.factor(credit$X90DPD_6)
str(credit)
levels(credit$X90DPD_6) = c(0,123,123,123,123)
#based on woe Inq_6 is grouped together for 4 groups and we can find the IV value still significant
credit$Inq_6 = as.factor(credit$Inq_6)

levels(credit$Inq_6) = c(0,1,1,3,4,1,1,1,1,0,0)
#Based on woe income is divided into two groups one less than 32 and greater than 32
inc = credit$Inc
inc = ifelse(inc<32, "1","2")
inc = as.factor(inc)
str(inc)
credit$Inc = inc
str(credit)
mon = credit1$Mon_Cur_Res
str(mon)
mon = ifelse(mon <=6, 1, mon)
mon = ifelse((mon>1 & mon<=29),2,mon)
mon = ifelse(mon>2, 3,mon)
mon
mon = as.factor(mon)
str(mon)
table(mon)
credit$Mon_Cur_Res = mon
str(credit)
y
avg = credit2$Average_Credit_12
avg = as.factor(avg)
str(avg)
avg1 = as.numeric(avg)
avg1 = ifelse(avg1 <= 14,1,2)
credit$Average_Credit_12 = as.factor(avg1)

str(credit)
tr = credit1$Trade_6
tr = as.factor(tr)
str(tr)
table(tr)

tr = ifelse(tr <=1, 1, 2)
credit$Trade_6 = tr
str(credit)


mo = credit$Mon_Cur_Com
mo = ifelse(mo<=3, 1, ifelse(mo>3 & mo<=21,2,ifelse(mo>21 & mo <=50,3, ifelse(mo >50 & mo <=62, 4,5))))
mo = as.factor(mo)
str(mo)
credit$Mon_Cur_Com = mo
str(credit)
y #from the IV values we can see that Autoloan, HL and Mon_Cur_comp are having values less than 0.03 after binning the variables
#so dropping those variables and building the final model
str(credit2)
credit$Average_Credit_12 = credit2$Average_Credit_12
credit_fin = credit[,-c(10,9,4)]
names(credit_fin)
dim(credit_fin)
head(credit3)
write.csv(credit3, "credit_final.csv")
getwd()

l = woe.binning(credit, 'Defaulter', credit)
k
l
woe.binning.plot(l)
woe.binning.table(l)
names(validation)
# Draw a random, stratified sample including p percent of the data
library(caret)
train1.index <- createDataPartition(credit_fin$Defaulter, p = .8, list = FALSE)
train1 <- credit_fin[ train1.index,]
test  <- credit_fin[-train1.index,]
tr2.index = createDataPartition(train1$Defaulter,p=0.25,list = F)
validation = train1[tr2.index,]
train = train1[-tr2.index,]
dim(train)
dim(validation)
dim(test)
train4 = as.data.frame.model.matrix(train)
train4
names(train4) = names(train)
logistic <- function(train,valid)
{
  eval_mat = matrix()
  #Building logisitic regression model on the  data  
  
  
  init_mod = glm(Defaulter ~ ., family = binomial, data = train)
  print(summary(init_mod))
  
  
  options(warn=-1) 
  pred = predict(init_mod, newdata = valid, type = 'response')
  
  #threshold = 0.16
  pred1 = (ifelse(pred > 0.10, 1, 0))
  #print(pred1)
  #Checking the sensitivity, precision and confusion matrix
  #pred1 = (pred1)
  #Confusion matrix
  #print(table(validation$Defaulter, pred1))
  cm = (caret::confusionMatrix(as.factor(pred1),valid$Defaulter,positive = '1'))
  print(cm)
  pred1 = factor(pred1)
  #Sensitivity
  recall = cm[[4]][1] 
  
  #Specificity
  specifity = cm[[4]][2]
  
  #Precision
  precision = cm[[4]][5]
  
  #F1 score
  F1 <- ((2 * precision * recall) / (precision + recall))
  
  #Youden's Index (Sensitivity + Specificity - 1)
  youdensIndex(valid$target, pred1)
  
  #Mis-classification Error
  misClassError(valid$target, pred)
  library(ROCR)
  ROCpred <- ROCR::prediction(pred,valid$Defaulter)
  ROCperf <- ROCR::performance(ROCpred,"tpr","fpr")
  plot(ROCperf)
  plot(ROCperf, colorize=T, 
       print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
  perf_AUC=ROCR::performance(ROCpred,"auc") #Calculate the AUC value
  AUC=perf_AUC@y.values[[1]]
  #auc = (auroc)
  options(warn=1)
  eval_mat = c(recall,specifity,precision,F1,AUC)
  names(eval_mat) = c("recall","specifity","precision","F1","auc")
  #print(eval_mat)
  
  return(eval_mat)
}
credit$Defaulter = as.factor(credit$Defaulter)
x =logistic(train = train4,valid = validation)*100
x
x =logistic(train = train,valid = test)*100
x
pred = predict(init_mod, newdata = validation, type = 'response')
x
length(pred1)
str(pred1)
View(pred1)
target = Defaulter
print(confusionMatrix(as.factor(pred1),validation$Defaulter))
pred1 = factor(pred1)
str(validation$Defaulter)
#==============================================================================
#Decision tree function
#==============================================================================



decision <- function(train,valid)
{
  eval_mat = matrix()
  #Building logisitic regression model on the  data  
  
  
  library(rpart)
  # Feature Scaling
  classifier = rpart(formula = Defaulter ~ .,
                     data = train,method = 'class')
  
  # Predicting the Test set results
  pred = predict(classifier, newdata =valid, type = 'class')
  
  #Confusion matrix 
  cm = caret::confusionMatrix(pred,valid$Defaulter,positive = '1')
  print(cm)
  #tpr, tn
  
  #Sensitivity
  recall = cm[[4]][1] 
  
  #Specificity
  specifity = cm[[4]][2]
  
  #Precision
  precision = cm[[4]][5]
  
  #F1 score
  F1 <- ((2 * precision * recall) / (precision + recall))
  
  #Youden's Index (Sensitivity + Specificity - 1)
  #youdensIndex(validation$Defaulter, pred1)
  
  
  #options(warn=1)
  
  #print(eval_mat)
  #plot(classifier)
  #text(classifier)
  library(ROCR)
  ROCpred <- ROCR::prediction(as.numeric(pred), valid$Defaulter)
  
  perf_AUC=ROCR::performance(ROCpred,"auc") #Calculate the AUC value
  AUC=perf_AUC@y.values[[1]]
  
  perf_ROC=ROCR::performance(ROCpred,"tpr","fpr") #plot the actual ROC curve
  plot(perf_ROC, main="ROC plot")
  text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))
  auc = AUC
  
  
  eval_mat = c(recall,specifity,precision,F1,auc)
  names(eval_mat) = c("recall","specificity","precision","F1","AUC")
  return(eval_mat)
}

y =decision(train = train,valid = validation) *100
y

y =decision(train = train,valid = test) *100
y
names(test)
head(train)
names(valid)
?randomForest



#=======================================================================
#RANDOM FOREST
#=======================================================================
randomfor <- function(validation,train)
{
  eval_mat = matrix()
  #Building logisitic regression model on the  data  
  
  
  
  set.seed(123)
  library(randomForest)
  classifier = randomForest(Defaulter~.,data = train,mtry=2, ntree=200,na.action = na.exclude)
  
  # Predicting the Test set results
  pred = predict(classifier, newdata = validation[-1])
  #print(pred)
  #Confusion matrix 
  cm = caret::confusionMatrix(pred,validation$Defaulter,positive = '1')
  print(cm)
  #tpr, tn
  
  #Sensitivity
  recall = cm[[4]][1] 
  
  #Specificity
  specifity = cm[[4]][2]
  
  #Precision
  precision = cm[[4]][5]
  
  #F1 score
  F1 <- ((2 * precision * recall) / (precision + recall))
  
  #Youden's Index (Sensitivity + Specificity - 1)
  #youdensIndex(validation$Defaulter, pred1)
  
  library(ROCR)
  pre2 = predict(classifier,type="prob",newdata=validation[-1])[,2]
  #predictions=as.vector(classifier$votes[,2])
  pred=ROCR::prediction(pre2,validation$Defaulter)
  
  perf_AUC=ROCR::performance(pred,"auc") #Calculate the AUC value
  AUC=perf_AUC@y.values[[1]]
  
  #perf_ROC=ROCR::performance(pred,"tpr","fpr") #plot the actual ROC curve
  #plot(perf_ROC, main="ROC plot")
  #text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))
  #options(warn=1)
  eval_mat = c(recall,specifity,precision,F1, AUC)
  names(eval_mat) = c("recall","specificity","precision","F1","AUC")
  print(AUC)
  return(eval_mat)
}

q =randomfor(validation=validation,train = train)
q*100

q =randomfor(validation=test,train = train)
q*100



classifier = randomForest(Defaulter~.,data = train)

# Predicting the Test set results
pred = predict(classifier, newdata = X_val)

#Confusion matrix 
cm = caret::confusionMatrix(pred,validation$Defaulter,positive = '1')
print(cm)
#tpr, tn

#Sensitivity
recall = cm[[4]][1] 

#Specificity
specifity = cm[[4]][2]

#Precision
precision = cm[[4]][5]

#F1 score
F1 <- ((2 * precision * recall) / (precision + recall))

#Youden's Index (Sensitivity + Specificity - 1)
#youdensIndex(validation$Defaulter, pred1)

library(ROCR)
predictions=as.vector(classifier$votes[,2])
pred=ROCR::prediction(predictions,validation$Defaulter)

perf_AUC=ROCR::performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]

perf_ROC=ROCR::performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
dim(validation)
#==================================================================================
#Naive Bayes function
#==================================================================================
naivebay <- function(validation,train)
{
  eval_mat = matrix()
  #Building logisitic regression model on the  data  
  
  
  
  set.seed(123)
  library(e1071)
  #library(klaR)
  classifier = naiveBayes(Defaulter~., data = train)
  
  pred = predict(classifier, newdata = validation[-1])
  #Confusion matrix 
  cm = caret::confusionMatrix(as.factor(pred),(validation$Defaulter),positive = '1')
  print(cm)
  #tpr, tn
  
  #Sensitivity
  recall = cm[[4]][1] 
  
  #Specificity
  specifity = cm[[4]][2]
  
  #Precision
  precision = cm[[4]][5]
  
  #F1 score
  F1 <- ((2 * precision * recall) / (precision + recall))
  
  #Youden's Index (Sensitivity + Specificity - 1)
  #youdensIndex(validation$Defaulter, pred1)
  
  library(ROCR)
  #predictions= pred$posterior[,1]
  pred=ROCR::prediction(as.numeric(pred),as.numeric(validation$Defaulter))
  
  perf_AUC=ROCR::performance(pred,"auc") #Calculate the AUC value
  AUC=perf_AUC@y.values[[1]]
  
  perf_ROC=ROCR::performance(pred,"tpr","fpr") #plot the actual ROC curve
  plot(perf_ROC, main="ROC plot")
  text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))
  #options(warn=1)
  eval_mat = c(recall,specifity,precision,F1,AUC)
  names(eval_mat) = c("recall","specificity","precision","F1","AUC")
  
  return(eval_mat)
}

z =naivebay(validation,train = train)
z*100

z =naivebay(validation = test,train = train)
z*100


#====================================================
#Over Sampling 
#====================================================
library(ROSE)  #Randomly Over sampling examples
table(train$Defaulter)
dim(train)
#Oversampling
?ovun.sample
over = ovun.sample(Defaulter~., data = train, method = "over", p = 0.2)$data
table(over$Defaulter)

#====================================================
#Under Sampling 
#====================================================
library(ROSE)  #Randomly Over sampling examples
table(train$Defaulter)

#Undersampling
under = ovun.sample(Defaulter~., data = train, method = "under", p = 0.2)$data
table(under$Defaulter)

#====================================================
#Both Sampling 
#====================================================
library(ROSE)  #Randomly Over sampling examples
table(train$Defaulter)

#Undersampling
both = ovun.sample(Defaulter~., data = train, method = "both", p =0.4)$data
table(both$Defaulter)
?ovun.sample()

#=======
#SMOTE
#=======

library(DMwR)
smote_cr = SMOTE(Defaulter~., data = train, perc.over = 450,perc.under = 150 )
table(smote_cr$Defaulter)
table(train$Defaulter)
head(smote_cr)
#=======================================================================================================================================
#Tomek balancing on the imbalanced train data and trying different models to check for improvement in the model performance
#=======================================================================================================================================
#train = train[-3]
names(train)
library(unbalanced)
tomek_bal = ubTomek(X = train[,-1], Y = train[,1])

head(train)
?ubTomek

newData<-cbind(tomek_bal$X, tomek_bal$Y)
View(newData)
table(train$Defaulter)
table(newData$`tomek_bal$Y`)

tomek_cr = newData
head(tomek_cr)
dim(tomek_cr)
colnames(tomek_cr)[21] <- "Defaulter"  
names(tomek_cr)
#===============================================================================================================
#ENN #Edited Nearest Neighbour
#======================================
en = ubENN(X = train[,-1], Y = train[,1],k = 3)
names(train)
str(train)
train$Defaulter = as.numeric(train$Defaulter)
train$HL = as.numeric(train$HL)
train$AutoLoan = as.numeric(train$AutoLoan)
head(en)
enn_cr = cbind(en$X, en$Y)
?ubENN
table(train$Defaulter)
table(en$Y)
colnames(enn_cr)[21] <- "Defaulter" 
?ubENN
#==================================================================
#CNN Condensed Nearest neighbour
#=========================================
?ubCNN
cn = ubCNN(X = train[,-1], Y = train[,1],k = 2)
cnn_cr<-cbind(cn$X, cn$Y)
colnames(cnn_cr)[21] <- "Defaulter" 
table(cnn$`cn$Y`)

#==================================
#SMOTE + TOMEK
#==================================
head(smote_cr)
smotetomek = ubBalance(X = smote_cr[,-1], Y = smote_cr[,1], type = 'ubTomek')
table(smotetomek$Y)
smotetom_cr = cbind(smotetomek$X,smotetomek$Y)
colnames(smotetom_cr)[21] <- "Defaulter" 
#=============================
#SMOTE + ENN
#=============================
smoteenn = ubBalance(X = smote_cr[,-1], Y = smote_cr[,1], type = 'ubENN')
table(smoteenn$Y)
smoteenn_cr = cbind(smoteenn$X,smoteenn$Y)
colnames(smoteenn_cr)[21] <- "Defaulter" 
#==================================
#All models on underbalanced data
#==================================
#logistic regression output with threshold at 0.10

x =logistic(train = under,valid = validation)*100
x
names(under)
names(test)
x1 =logistic(train = under,valid = test)*100
x1
#Decsion tree output 
y = decision(train = under,valid = validation)*100
y

y1 = decision(train = under,valid = test)*100
y1
#Random forest output
z = randomfor(validation = validation,train = under)
z*100
z1 = randomfor(validation = test,train = under)
z1*100
#Naive bayes output
w = naivebay(validation = validation,train = under)
w*100

undebalanced = c(x,y,z,wc)
undebalanced
#==================================
#All models on overbalanced data
#==================================
#logistic regression output with threshold at 0.10
x1 =logistic(train = over,valid = test)*100
x1
#Decsion tree output 
y1 = decision(train = over,valid = test)*100
y1
table(over$Defaulter)
table(under$Defaulter)
#Random forest output
z = randomfor(validation = test,train = over)
z*100

#Naive bayes output
w = naivebay(validation = test,train = over)
w*100



#========================================================
#All models on both over sampling and under sampling data
#==========================================================
#logistic regression output with threshold at 0.10
x1= logistic(valid = test,train = both)
x1*100
#Decsion tree output 
y = decision(valid = validation,train = both)
y*100

#Random forest output
z = randomfor(validation = validation,train = both)
z*100

#Naive bayes output
w = naivebay(validation = validation,train = both)
w*100







#============================
#All models on SMOTE data 
#============================
#logistic regression output with threshold at 0.10
x1= logistic(valid= test,train = smote_cr)
x1*100
#Decsion tree output 
y = decision(valid = validation,train = smote_cr)
y*100

#Random forest output
z = randomfor(validation = validation,train = smote_cr)
z*100

#Naive bayes output
w = naivebay(validation = validation,train = smote_cr)
w*100

#================================
#All models on Tomek data 
#================================
#logistic regression output with threshold at 0.10
x1= logistic(valid = test,train = tomek_cr)
print('The logistic regression output on Tomek balanced data')
x1*100
#Decsion tree output 
y = decision(valid= validation,train = tomek_cr)
y*100

#Random forest output
z = randomfor(validation = validation,train = tomek_cr)
z = z*100
print('The random forest output on Tomek balanced data')

z
#Naive bayes output
w = naivebay(validation = validation,train = tomek_cr)
wc= w*100
print('The Naive Bayes output on Tomek balanced data')
wc

#==================================
#All models on ENN Balanced data
#==================================
#logistic regression output with threshold at 0.10
x= logistic(valid= validation,train = enn_cr)
print('The logistic regression output on ENN balanced data')
x*100
#Decsion tree output 
y = decision(valid = validation,train = enn_cr)
print('The Decision tree output on ENN balanced data')

y*100

#Random forest output
z = randomfor(validation = validation,train = enn_cr)
z = z*100
print('The random forest output on ENN balanced data')
z
#Naive bayes output
w = naivebay(validation = validation,train = enn_cr)
wc= w*100
print('The Naive Bayes output on ENN balanced data')
wc
#==================================
#All models on CNN Balanced data
#==================================
#logistic regression output with threshold at 0.10
x= logistic(valid= validation
            ,train = cnn_cr)
print('The logistic regression output on CNN balanced data')
x*100
#Decsion tree output 
y = decision(valid = validation,train = cnn_cr)
print('The Decision tree output on CNN balanced data')

y*100

#Random forest output
z = randomfor(validation = validation,train = cnn_cr)
z = z*100
print('The random forest output on CNN balanced data')
z
#Naive bayes output
w = naivebay(validation = validation,train = cnn_cr)
wc= w*100
print('The Naive Bayes output on CNN balanced data')
wc


#==================================
#All models on SMOTE + TOMEK Balanced data
#==================================
#logistic regression output with threshold at 0.10
x= logistic(valid = validation,train = smotetom_cr)
print('The logistic regression output on SMOTE + TOMEK Balanced data')
x*100
#Decsion tree output 
y = decision(valid = validation,train = smotetom_cr)
print('The Decision tree output on SMOTE + TOMEK Balanced data')

y*100

#Random forest output
z = randomfor(validation = validation,train = smotetom_cr)
z = z*100
print('The random forest output on SMOTE + TOMEK Balanced data')
z
#Naive bayes output
w = naivebay(validation = validation,train = smotetom_cr)
wc= w*100
print('The Naive Bayes output on SMOTE + TOMEK Balanced data')
wc

#==================================
#All models on SMOTE + ENN Balanced data
#==================================
#logistic regression output with threshold at 0.10
table(smoteenn_cr$Defaulter)
x= logistic(valid= validation,train = smoteenn_cr)
print('The logistic regression output on SMOTE + ENN Balanced data')
x*100
#Decsion tree output 
y = decision(valid = validation,train = smoteenn_cr)
print('The Decision tree output on SMOTE + ENN Balanced data')

y*100

#Random forest output
z = randomfor(validation = validation,train = smoteenn_cr)
z = z*100
print('The random forest output on SMOTE + ENN Balanced data')
z
#Naive bayes output
w = naivebay(validation = validation,train = smoteenn_cr)
wc= w*100
print('The Naive Bayes output on SMOTE + ENN Balanced data')
wc


#woe binning on numerical variables
names(credit)
library(woeBinning)
?woe.binning
x = woe.binning(credit, 'Defaulter',credit,
                min.perc.total=0.005, min.perc.class=0.01,
                stop.limit=0.1, event.class=1)
wb = createWorkbook()
y = woe.binning.table(x)
y
str(credit)
#woe of X90DPD6 for 1,2,3 are similar so combining them as one level
credit$X90DPD_6 = as.factor(credit$X90DPD_6)
str(credit)
levels(credit$X90DPD_6) = c(0,123,123,123,123)
#based on woe Inq_6 is grouped together for 4 groups and we can find the IV value still significant
credit$Inq_6 = as.factor(credit$Inq_6)
levels(credit$Inq_6) = c(0,1,1,3,4,1,1,1,1,0,0)
#Based on woe income is divided into two groups one less than 32 and greater than 32
inc = credit$Inc
inc = ifelse(inc<32, "1","2")
inc = as.factor(inc)
str(inc)
credit$Inc = inc
str(credit)
mon = credit$Mon_Cur_Res
str(mon)
mon = ifelse(mon <=6, 1, mon)
mon = ifelse( (mon>1) & (mon<=29),2,3)
mon
mon = ifelse(mon <=6, 1, ifelse((mon >6) & (mon <=29),2,3))
mon
mon = as.factor(mon)
str(mon)
table(mon)
credit$Mon_Cur_Res = mon
str(credit)
y
avg = credit$Average_Credit_12
avg = as.factor(avg)
str(avg)
avg = ifelse(avg <= 14,1,2)
credit$Average_Credit_12 = avg

str(credit)
tr = credit$Trade_6
tr = as.factor(tr)
str(tr)
table(tr)

tr = ifelse(tr <=1, 1, 2)
credit$Trade_6 = tr
str(credit)


mo = credit$Mon_Cur_Com
mo = ifelse(mo<=3, 1, ifelse(mo>3 & mo<=21,2,ifelse(mo>21 & mo <=50,3, ifelse(mo >50 & mo <=62, 4,5))))
mo = as.factor(mo)
str(mo)
credit$Mon_Cur_Com = mo
str(credit)
y #from the IV values we can see that Autoloan, HL and Mon_Cur_comp are having values less than 0.03 after binning the variables
#so dropping those variables and building the final model

credit_fin = credit[,-c(10,9,4)]
names(credit_fin)
dim(credit_fin)
head(credit3)
write.csv(credit3, "credit_final.csv")
getwd()
