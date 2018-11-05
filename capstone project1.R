#1. DATA (CREEDIT Bureau data)
#=============================================================================================

credit = read.csv("C:\\Users\\HP\\Desktop\\Capp\\Credit Bureau data.csv")
View(credit)



#2. Get a basic understanding of the dataset
#=============================================================================================

dim(credit) #Checking the dimensions of the data
str(credit)

table(credit$Defaulter) # Checking for imbalance in the data of defaulters
prop.table(table(credit$Defaulter))*100 
barplot(table(credit$Defaulter)*100 , xlab = 'Credit card Non Defaulters and Defaulters', ylab = 'Count')
#Total missing values in the dataset
sum(is.na(credit))
x = dim(credit[1][1])
x[1]
sum(is.na(credit))/x[1]*100 #total percent of records having missing values

#Missing values count in each variable

na_count <-sapply(credit, function(y) sum(length(which(is.na(y)))))
na_count



#3. EXPLORING THE DATA VISUALLY
#=============================================================================================

#A. UNIVARIATE ANALYSIS

#Histogram or barplots for numerical variables
for(i in 1:ncol(credit))
{
  if(is.numeric(credit[,i]))
  {
    if(length(unique(credit[,i])) > 10)
    {
      hist(credit[,i], main = names(credit)[i], xlab = names(credit)[i])
    }
    
    else if(length(unique(credit[,i])) < 10)
    {
      barplot(table(credit[,i]), main=names(credit)[i], xlab = names(credit)[i])
    }
  }
}

#Bad rate for all the variables and storing them in a separate excel file

#Changing the variable names 
names(credit)
names(credit)[names(credit) == 'Avg.CC.12'] <- 'Average_Credit_12'
names(credit)[names(credit) == 'Trades.6'] <- 'Trade_6'
names(credit)[names(credit) == 'Res.Ty'] <- 'Res_Ty'
names(credit)[names(credit) == 'PL.Tr.6'] <- 'PLTr_6'
names(credit)[names(credit) == 'PL.Tr.12'] <- 'PLTr_12'
names(credit)[names(credit) == 'Inq.6'] <- 'Inq_6'
names(credit)[names(credit) == 'Inq.12'] <- 'Inq_12'
names(credit)[names(credit) == 'Tot.Tr'] <- 'Tot_Tr'
names(credit)[names(credit) == 'X90.DPD.6'] <- 'X90DPD_6'
names(credit)[names(credit) == 'X90.DPD.12'] <- 'X90DPD_12'
names(credit)[names(credit) == 'X60.DPD.6'] <- 'X60DPD_6'
names(credit)[names(credit) == 'X60.DPD.12'] <- 'X60DPD_12'
names(credit)[names(credit) == 'X30.DPD.6'] <- 'X30DPD_6'
names(credit)[names(credit) == 'X30.DPD.12'] <- 'X30DPD_12'






library(sqldf)
library(xlsx)
wb = createWorkbook()

for(i in 2:ncol(credit))
{
  if(is.factor(credit[,i]))
  {
    
    varname = names(credit)[i]
    
    sheet = createSheet(wb, varname)
    
    string = paste("SELECT", varname, ", round(avg(Defaulter)*100,2) AS 'Bad Rate', (100 - round(avg(Defaulter)*100,2)) AS 'Good Rate' FROM credit GROUP BY", varname)
    
    addDataFrame(sqldf(string), sheet = sheet, startColumn = 2, row.names = F)
    
  }
}

saveWorkbook(wb, "Bad Rate.xlsx")

getwd()

credit = credit[,-2] #Dropping id


#3. EXPLORING THE DATA VISUALLY
#=============================================================================================

#A. UNIVARIATE ANALYSIS

#Histogram or barplots for numerical variables
for(i in 1:ncol(credit))
{
  if(is.numeric(credit[,i]))
  {
    if(length(unique(credit[,i])) > 10)
    {
      hist(credit[,i], main = names(credit)[i], xlab = names(credit)[i])
    }
    
    else if(length(unique(credit[,i])) < 10)
    {
      barplot(table(credit[,i]), main=names(credit)[i], xlab = names(credit)[i])
    }
  }
}


#Barplots for categorical varibales
for(i in 1:ncol(credit))
{
  if(is.factor(credit[,i]))
  {
    #barplot(table(credit[,i]), main=names(credit)[i], xlab = names(credit)[i])
    print(names(credit)[i])
  }
}
cat = c("Gender","Marital","Edu","Prof","Res_Ty","AutoLoan","HL")

cat
#B. BIVARIATE ANALYSIS

#Side-by-side Boxplots for numerical variables
for(i in 2:ncol(credit))
{
  if(is.numeric(credit[,i]))
  {
    if(length(unique(credit[,i])) > 10)
    {
      boxplot(credit[,i] ~ credit$Defaulter, main = names(credit)[i], ylab = names(credit)[i])
      print(names(credit)[i])
    }
    
    else if(length(unique(credit[,i])) < 10)
    {
      barplot(table(credit[,i], credit$Defaulter), main=names(credit)[i], 
              xlab = names(credit)[i], beside = T, legend = rownames(table(credit[,i])))
      print(names(credit)[i])
    }
  }
}

num = c("Age","Dep","Inc","Mon_Cur_Res","Mon_Cur_Com","X90DPD_6","X60DPD_6","X30DPD_6","X90DPD_12","X60DPD_12",
        "Average_Credit_12","Trade_6","Trades.12","PLTr_6","PLTr_12","Inq_6","Inq_12","HL","Bal","Tot_Tr","AutoLoan")

#Treating the anomalies and missing values 



require(sqldf)
#checking for missing values count across target variable distribution of 1s and 0s
sqldf("SELECT Average_Credit_12,Defaulter,count(Defaulter) as Count
      FROM credit
      WHERE Average_Credit_12 is NULL
      GROUP BY (Defaulter) ")

sqldf("SELECT (HL),Defaulter,count(Defaulter) as Count
      FROM credit
      WHERE HL is null GROUP BY (Defaulter) ")

sqldf("SELECT Bal,Defaulter,count(Defaulter) as Count
      FROM credit
      WHERE Bal is null
      GROUP BY (Defaulter) ")

sqldf("SELECT Trade_6,Defaulter,count(Defaulter) as Count
      FROM credit
      WHERE Trade_6,Defaulter is NULL
      GROUP BY (Defaulter) ")

#Dropping the missing value records in which response variable takes the value 0(majority class)

#For variables Balance and HL
ind <- which(with( credit, credit$Defaulter==0 & is.na(credit$Bal)))
length(ind)
credit = credit[-ind,]
dim(credit)

#Variable Average_credit_12
ind2 <- which(with( credit, credit$Defaulter==0 & is.na(credit$Average_Credit_12)))
length(ind2)
credit = credit[-ind2,]
na_count <-sapply(credit, function(y) sum(length(which(is.na(y)))))
na_count

#Dropping all records for which response variable is missing 
credit = credit[-which(is.na(credit$Defaulter)),]
dim(credit)

# Imputing the missing values in the variables 


head(credit)
dim(credit)
str(credit)
library(Hmisc)
credit$Bal[is.na(credit$Bal)] <- median(credit$Bal, na.rm = T) # replace with mean
credit$HL[is.na(credit$HL)] <- median(credit$HL, na.rm = T) # replace with mean
# median
credit$Average_Credit_12[is.na(credit$Average_Credit_12)] <- median(credit$Average_Credit_12, na.rm = T) # replace with mean




#Checking the missing values in dataframe 

na_count <-sapply(credit, function(y) sum(length(which(is.na(y)))))
na_count

getwd()


#Checking for anomalies in all the categorical variables

#Calculating the Bad Rate for all the categorical variables

for(i in 1:ncol(credit))
{
  if(is.factor(credit[,i]))
  {
    varname = names(credit)[i]
    
    string = paste("SELECT", varname, ", count(Defaulter) as Count FROM credit GROUP BY", varname)
    
    #print(sqldf(string))
    
    a = sqldf(string)
    print(a)
    
  }
}
dim(credit)

#Dropping the anomalies in the categorical variables which have 0 as their value 

variables = c('Gender','Marital','Edu','Professional','Res_Ty')

  
ind3 <- which(with( credit, credit$Gender==0))
length(ind3)
credit = credit[-ind3,]
is.vector(ind3)
ind4 <- which(with( credit, credit$Marital==0))
length(ind4)
credit = credit[-ind4,]
ind5 <- which(with( credit, credit$Edu==0))
length(ind5)
credit = credit[-ind5,]
ind6 <- which(with( credit, credit$Res_Ty==0))
length(ind6)
credit = credit[-ind6,]
ind7 <- which(with( credit, credit$Prof==0))
length(ind7)
credit = credit[-ind7,]
dim(credit)

#4. RE-GROUPING THE LEVELS OF THE CATEGORICAL VARIABLES
#=============================================================================================


library(xlsx)
library(InformationValue)

num

#Calculate the WOE table for the variable history

WOETable(credit$Average_Credit_12, credit$Defaulter)

WOETable(credit$Gender, credit$Defaulter)


#Exporting the WOE Table for every categorical variables in excel workbook
wb = createWorkbook()
str(credit)
for(i in 1:ncol(credit))
{
  if(is.factor(credit[,i]))
  {
    
    varname = names(credit)[i]
    
    sheet = createSheet(wb, varname)
    
    woe = WOETable(credit[,varname], credit$Defaulter)
    
    addDataFrame(woe, sheet = sheet, startColumn = 2, row.names = F)
    
  }
}

saveWorkbook(wb, "WOE Table 2.xlsx")

#Information value to figure the important variables 

credit$Defaulter = as.numeric(as.character(credit$Defaulter))
IV <- create_infotables(data=credit,
                        
                        y="Defaulter")
names(credit)
IV$Summary

grid.table((IV$Summary), rows=NULL)
#WOE for numerical variables
library(woeBinning)
x = woe.binning(credit, 'Defaulter',c(names(num)),
            min.perc.total=0.05, min.perc.class=0.01,
            stop.limit=0.1, event.class=1)
wb = createWorkbook()
x
saveWorkbook(wb2, "WOE Table for numerical 2.xlsx")


k = woe.binning(credit, 'Defaulter', credit)
k
woe.binning.plot(k)
woe.binning.table(x)

getwd()
library(Information)
dim(train)
dim(validation)
?create_infotables
str(train)
names(train)
train$Defaulter = as.numeric(train$Defaulter)
IV <- create_infotables(data=train,
                        y="Defaulter")
names(train)
create_infotables(data = train,valid = validation, y = 'Defaulter')
#UNDERSTANDING VARIABLE IMPORTANCE
#=============================================================================================
library(InformationValue)


#Using IV to understand the imporance of the categorical variables
infovalue = list()
for(i in 1:ncol(credit))
{
  if(is.factor(credit[,i]))
  {
    varname = names(credit)[i]
    
    #print(varname)
    IV = (IV(X=factor(credit[,varname]), Y=credit$Defaulter))
    tmp = list(IV)
    infovalue[varname] = tmp
    #print(infovalue)
    
  }
 print(IV) 
}
#------------------------------------------------------------------------------
#chisq-test for categorical variables
#-------------------------------------------------------------------------------

ChisqTest <- function(data,var,responseIndex)
{
  o = data.frame()
  x=1
  for (i in var) {
    if (length(unique(data[,i]))>1) {
      o[x,1] = variable.names(data[i])
      o[x,2] = chisq.test(data[,i],data[,responseIndex])$p.value
      x=x+1
    }
  }
  return(o)
}

ChisqTest(credit,cat,1)

#HL and #Auto loan are important categorical variables as their p-value

cat


#Using t-test to understand the imporance of the numerical variables

tTest <- function(data,var,responseIndex)
{
  o = data.frame()
  x=1
  for (i in var) {
    if (length(unique(data[,i]))>1) {
      o[x,1] = variable.names(data[i])
      o[x,2] = t.test(data[,i]~data[,responseIndex])$p.value
      x=x+1
    }
  }
  return(o)
}

tTest(credit,num,1)

#Except Age and Dept all other numerical variables are important as p-value is less than 0.05. 

# So the important variables are from t-test and chi-sq test HL, Autoloan, Inc, Mon_Cur_Res, Mon_Cur_Com, 
#X90.DPD.6, X60.DPD.6, X30.DPD.6, X90.DPD.12, X60.DPD.12, X30.DPD.12, 
#Avg.CC.12, Trades.6, Trades.12, PL.Tr.6, PL.Tr.12,
#Inq.6, Inq.12, BAL, and Tot.Tr.

names(credit)
credit = credit[,-c(2,3,4,5,7,8,9)]
View(credit)
head(credit)
dim(credit)
credit$Defaulter = factor(credit$Defaulter)
str(credit1)
credit1 = credit


# Draw a random, stratified sample including p percent of the data
library(caret)
train1.index <- createDataPartition(credit$Defaulter, p = .8, list = FALSE)
train1 <- credit[ train1.index,]
test  <- credit[-train1.index,]
tr2.index = createDataPartition(train1$Defaulter,p=0.25,list = F)
validation = train1[tr2.index,]
train = train1[-tr2.index,]
dim(train)
dim(validation)
dim(test)


X_train = (model.matrix(Defaulter~ ., train)[,-1])
View(X_train)
X_val = (model.matrix(Defaulter ~ ., validation)[,-1])
y_val = validation$Defaulter
X_test = (model.matrix(Defaulter ~ ., test)[,-1])
y_train = (train$Defaulter)
y_test = test$Defaulter
str(credit)
str(y_val)

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

x =logistic(train = train,valid = validation)*100
x

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



#=======================================================================
#RANDOM FOREST
#=======================================================================
randomfor <- function(validation,train)
{
  eval_mat = matrix()
  #Building logisitic regression model on the  data  
  
  
  
  set.seed(123)
  library(randomForest)
  classifier = randomForest(Defaulter~.,data = train,mtry=2, ntree=200)
  
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


#====================================================
#Over Sampling 
#====================================================
library(ROSE)  #Randomly Over sampling examples
table(train$Defaulter)
dim(train)
#Oversampling
?ovun.sample
over = ovun.sample(Defaulter~., data = train, method = "over", p = 0.4)$data
table(over$Defaulter)
barplot(table(over$Defaulter))
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
?SMOTE
smote_cr = SMOTE(Defaulter~., data = train, perc.over = 450,perc.under = 150)
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
table(tomek_cr$Defaulter)
#===============================================================================================================
#ENN #Edited Nearest Neighbour
#======================================
en = ubENN(X = train[,-1], Y = train[,1],k = 2)
head(en)
enn_cr = cbind(en$X, en$Y)
?ubENN
table(train$Defaulter)
table(en$Y)
colnames(enn_cr)[21] <- "Defaulter" 
?ubENN
table(enn_cr$Defaulter)
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
#Decsion tree output 
y = decision(train = under,valid = validation)*100
y
#Random forest output
z = randomfor(validation = validation,train = under)
z*100
#Naive bayes output
w = naivebay(validation = validation,train = under)
w*100

undebalanced = c(x,y,z,wc)
undebalanced
#==================================
#All models on overbalanced data
#==================================
#logistic regression output with threshold at 0.10
x =logistic(train = over,valid = validation)*100
x
#Decsion tree output 
y = decision(train = over,valid = validation)*100
y
table(over$Defaulter)
table(under$Defaulter)
#Random forest output
z = randomfor(validation = validation,train = over)
z*100

#Naive bayes output
w = naivebay(validation = validation,train = over)
w*100



#========================================================
#All models on both over sampling and under sampling data
#==========================================================
#logistic regression output with threshold at 0.10
x= logistic(valid = validation,train = both)
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
x= logistic(valid= validation,train = smote_cr)
x*100
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
x= logistic(valid = validation,train = tomek_cr)
print('The logistic regression output on Tomek balanced data')
x*100
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




#Random forest model on SMOTE + ENN gives the best F1 score and AUC score among all the 40 models computed



#Further selection of variables by treating multicollinearity in the data 
#I have developed a function taking help from github for vif. 
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
x = vif_func(credit1,thresh = 2,trace = T) #Setting threshold VIF 2 for removing multicollinearity
x #We can see the variables "X30.DPD.12"   "X60.DPD.6"   "X30.DPD.6"   "X90.DPD.12"  "X60.DPD.12" were all multicollinear
#so dropping them and building the models again 
dim(train)
names(credit1)
names(x)
credit = credit[,x] 
credit1 = credit1[,x]
dim(credit1)
credit2 = credit
train = train[,x]
validation = validation[,x]
over = over[,x]
under = under[,x]
both = both[,x]
smote_cr = smote_cr[,x]
tomek_cr = tomek_cr[,x]
enn_cr = enn_cr [,x]
smoteenn_cr = smoteenn_cr[,x]
smotetom_cr = smotetom_cr[,x]

credit = credit1
str(credit)
names(validation)
names(test)






# # Draw a random, stratified sample including p percent of the data
# library(caret)
# train1.index <- createDataPartition(credit$Defaulter, p = .8, list = FALSE)
# train1 <- credit[ train1.index,]
# test  <- credit[-train1.index,]
# tr2.index = createDataPartition(train1$Defaulter,p=0.25,list = F)
# validation = train1[tr2.index,]
# train = train1[-tr2.index,]
# dim(train)
# dim(validation)
# dim(test)






