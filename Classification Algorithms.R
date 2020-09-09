if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} 
pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071") 
library(pROC)
library(dplyr)

#Importing Data
data1<-read.csv("C:\\Users\\urvipalvankar\\Urvi\\Master of Management Analytics\\867 - Predictive Modelling\\Assignment 3\\MMA867 A3 -- credit data.csv",na.strings=c(""," ","NA"), header=TRUE,stringsAsFactors = TRUE)
data2<-read.csv("C:\\Users\\urvipalvankar\\Urvi\\Master of Management Analytics\\867 - Predictive Modelling\\Assignment 3\\MMA867 A3 -- new applications.csv",na.strings=c(""," ","NA"), header=TRUE,stringsAsFactors = TRUE)

str(data1)
str(data2)

data2_default<-select(data2,-c(X))
data2_default$default_0=0

credit_data<-rbind(data1,data2_default)

####################### Combining Categories ########################

credit_data$PAY_1 <- ifelse(credit_data$PAY_1 >= 4 , 4 , credit_data$PAY_1)
credit_data$PAY_2 <- ifelse(credit_data$PAY_2 >= 4 , 4 , credit_data$PAY_2)
credit_data$PAY_3 <- ifelse(credit_data$PAY_3 >= 4 , 4 , credit_data$PAY_3)
credit_data$PAY_4 <- ifelse(credit_data$PAY_4 >= 4 , 4 , credit_data$PAY_4)
credit_data$PAY_5 <- ifelse(credit_data$PAY_5 >= 4 , 4 , credit_data$PAY_5)
credit_data$PAY_6 <- ifelse(credit_data$PAY_6 >= 4 , 4 , credit_data$PAY_6)

####################### Feature Engineering #########################

#feature 1 - Feature for -1 paying class ad 0 paying class
credit_data$bene <- as.factor(ifelse(credit_data$PAY_1 == -1 & credit_data$PAY_2 == -1 & credit_data$PAY_3 == -1 & credit_data$PAY_4 == -1 & credit_data$PAY_5 == -1 & credit_data$PAY_6 == -1,1,0))
credit_data$rev <- as.factor(ifelse(credit_data$PAY_1 == 0 & credit_data$PAY_2 == 0 & credit_data$PAY_3 == 0 & credit_data$PAY_4 == 0 & credit_data$PAY_5 == 0 & credit_data$PAY_6 == 0,1,0))

#feature 2 - variable to see who is defaulting more than 3 payments
pay_1_var<-ifelse(credit_data$PAY_1>0,1,0)
pay_2_var<-ifelse(credit_data$PAY_2>0,1,0)
pay_3_var<-ifelse(credit_data$PAY_3>0,1,0)                 
pay_4_var<-ifelse(credit_data$PAY_4>0,1,0)
pay_5_var<-ifelse(credit_data$PAY_5>0,1,0)
pay_6_var<-ifelse(credit_data$PAY_6>0,1,0)

total<-pay_1_var+pay_2_var+pay_3_var+pay_4_var+pay_5_var+pay_6_var
credit_data$def<-as.factor(ifelse(total>3,1,0))

#Feature 3 = Filtering clients whose bill amounts are less than payment status
credit_data$completepay2 <- ifelse(credit_data$BILL_AMT2 <= credit_data$PAY_1, 1, 0)
credit_data$completepay3 <- ifelse(credit_data$BILL_AMT3 <= credit_data$PAY_2, 1, 0)
credit_data$completepay4 <- ifelse(credit_data$BILL_AMT4 <= credit_data$PAY_3, 1, 0)
credit_data$completepay5 <- ifelse(credit_data$BILL_AMT5 <= credit_data$PAY_4, 1, 0)
credit_data$completepay6 <- ifelse(credit_data$BILL_AMT6 <= credit_data$PAY_5, 1, 0)

credit_data$pay_credit_data <- credit_data$completepay2 + credit_data$completepay3 + credit_data$completepay4 + credit_data$completepay5 + credit_data$completepay6

#Feature 4 - Marriage
credit_data$MARRIAGE <- ifelse(credit_data$MARRIAGE == 0, 3 , credit_data$MARRIAGE)

#Feature 5 - Education
credit_data$EDUCATION <- ifelse(credit_data$EDUCATION == 0 | credit_data$EDUCATION == 6 , 5 , credit_data$EDUCATION)

#Feature 6 - Age Group
#credit_data$agegroup1 <- as.factor(ifelse(credit_data$AGE <= 24, 1, 0))
#credit_data$agegroup2 <- as.factor(ifelse(credit_data$AGE >= 25 & credit_data$AGE <=34 , 1, 0))
#credit_data$agegroup3 <- as.factor(ifelse(credit_data$AGE >= 35 & credit_data$AGE <=44 , 1, 0))
#credit_data$agegroup4 <- as.factor(ifelse(credit_data$AGE >= 45 & credit_data$AGE <=54 , 1, 0))
#credit_data$agegroup5 <- as.factor(ifelse(credit_data$AGE >= 55, 1, 0))

#Feature 7 - pay amount/bill amount
credit_data$PAY_RATIO_APR<-ifelse(is.nan(credit_data$PAY_AMT1/credit_data$BILL_AMT1),0,
                           ifelse(is.infinite(credit_data$PAY_AMT1/credit_data$BILL_AMT1),0,round(credit_data$PAY_AMT1/credit_data$BILL_AMT1,2)))

credit_data$PAY_RATIO_MAY<-ifelse(is.nan(credit_data$PAY_AMT2/credit_data$BILL_AMT2),0,
                           ifelse(is.infinite(credit_data$PAY_AMT2/credit_data$BILL_AMT2),0,round(credit_data$PAY_AMT2/credit_data$BILL_AMT2,2)))

credit_data$PAY_RATIO_JUNE<-ifelse(is.nan(credit_data$PAY_AMT3/credit_data$BILL_AMT3),0,
                            ifelse(is.infinite(credit_data$PAY_AMT3/credit_data$BILL_AMT3),0,round(credit_data$PAY_AMT3/credit_data$BILL_AMT3,2)))

credit_data$PAY_RATIO_JULY<-ifelse(is.nan(credit_data$PAY_AMT4/credit_data$BILL_AMT4),0,
                            ifelse(is.infinite(credit_data$PAY_AMT4/credit_data$BILL_AMT4),0,round(credit_data$PAY_AMT4/credit_data$BILL_AMT4,2)))

credit_data$PAY_RATIO_AUG<-ifelse(is.nan(credit_data$PAY_AMT5/credit_data$BILL_AMT5),0,
                           ifelse(is.infinite(credit_data$PAY_AMT5/credit_data$BILL_AMT5),0,round(credit_data$PAY_AMT5/credit_data$BILL_AMT5,2)))

credit_data$PAY_RATIO_SEPT<-ifelse(is.nan(credit_data$PAY_AMT6/credit_data$BILL_AMT6),0,
                            ifelse(is.infinite(credit_data$PAY_AMT6/credit_data$BILL_AMT6),0,round(credit_data$PAY_AMT6/credit_data$BILL_AMT6,2)))


#feature 8 - If bill amount is not 0 and if pay type is -2
credit_data$nopay2 <- as.factor(ifelse(credit_data$BILL_AMT2 != 0 & credit_data$PAY_1 == -2, 1, 0))
credit_data$nopay3 <- as.factor(ifelse(credit_data$BILL_AMT3 != 0 & credit_data$PAY_2 == -2, 1, 0))
credit_data$nopay4 <- as.factor(ifelse(credit_data$BILL_AMT4 != 0 & credit_data$PAY_3 == -2, 1, 0))
credit_data$nopay5 <- as.factor(ifelse(credit_data$BILL_AMT5 != 0 & credit_data$PAY_4 == -2, 1, 0))
credit_data$nopay6 <- as.factor(ifelse(credit_data$BILL_AMT6 != 0 & credit_data$PAY_5 == -2, 1, 0))

#feature 9 - looking at only those customers who payment status is >=5
credit_data$PAY_1_5 <- as.factor(ifelse((credit_data$PAY_1) >= 2 , 1 , 0))
credit_data$PAY_2_5 <- as.factor(ifelse((credit_data$PAY_2) >= 2 , 1 , 0))
credit_data$PAY_3_5 <- as.factor(ifelse((credit_data$PAY_3) >= 2 , 1 , 0))
credit_data$PAY_4_5 <- as.factor(ifelse((credit_data$PAY_4) >= 2 , 1 , 0))
credit_data$PAY_5_5<- as.factor(ifelse((credit_data$PAY_5) >= 2, 1 , 0))
credit_data$PAY_6_5<- as.factor(ifelse((credit_data$PAY_6) >= 2 , 1 , 0))

#feature 12 - summing the rows of bill amounts to show 2,3,4,5,6 month bill amounts
head(credit_data[,24])

credit_data$TwoMonth_BillAmount<-rowSums(credit_data[,13:14])
credit_data$ThreeMonth_BillAmount<-rowSums(credit_data[,13:15])
credit_data$FourMonth_BillAmount<-rowSums(credit_data[,13:16])
credit_data$FiveMonth_BillAmount<-rowSums(credit_data[,13:17])
credit_data$SixMonth_BillAmount<-rowSums(credit_data[,13:18])

#feature 13 - summing the columns of payment amounts to show 2,3,4,5,6 month bill amounts
credit_data$TwoMonth_PaymentAmount<-rowSums(credit_data[,19:20])
credit_data$ThreeMonth_PaymentAmount<-rowSums(credit_data[,19:21])
credit_data$FourMonth_PaymentAmount<-rowSums(credit_data[,19:22])
credit_data$FiveMonth_PaymentAmount<-rowSums(credit_data[,19:23])
credit_data$SixMonth_PaymentAmount<-rowSums(credit_data[,19:24])

#feature 14 - Bill amount ratios
credit_data$OneMonth_BillAmount_Ratio<-ifelse(is.nan(credit_data$BILL_AMT1/credit_data$SixMonth_BillAmount),0,
                                       ifelse(is.infinite(credit_data$BILL_AMT1/credit_data$SixMonth_BillAmount),0,round(credit_data$BILL_AMT1/credit_data$SixMonth_BillAmount,2)))

credit_data$TwoMonth_BillAmount_Ratio<-ifelse(is.nan(credit_data$TwoMonth_BillAmount/credit_data$SixMonth_BillAmount),0,
                                       ifelse(is.infinite(credit_data$TwoMonth_BillAmount/credit_data$SixMonth_BillAmount),0,round(credit_data$TwoMonth_BillAmount/credit_data$SixMonth_BillAmount,2)))

credit_data$ThreeMonth_BillAmount_Ratio<-ifelse(is.nan(credit_data$ThreeMonth_BillAmount/credit_data$SixMonth_BillAmount),0,
                                         ifelse(is.infinite(credit_data$ThreeMonth_BillAmount/credit_data$SixMonth_BillAmount),0,round(credit_data$ThreeMonth_BillAmount/credit_data$SixMonth_BillAmount,2)))

credit_data$FourMonth_BillAmount_Ratio<-ifelse(is.nan(credit_data$FourMonth_BillAmount/credit_data$SixMonth_BillAmount),0,
                                        ifelse(is.infinite(credit_data$FourMonth_BillAmount/credit_data$SixMonth_BillAmount),0,round(credit_data$FourMonth_BillAmount/credit_data$SixMonth_BillAmount,2)))

credit_data$FiveMonth_BillAmount_Ratio<-ifelse(is.nan(credit_data$FiveMonth_BillAmount/credit_data$SixMonth_BillAmount),0,
                                        ifelse(is.infinite(credit_data$FiveMonth_BillAmount/credit_data$SixMonth_BillAmount),0,round(credit_data$FiveMonth_BillAmount/credit_data$SixMonth_BillAmount,2)))


#feature 15 - payment ratios
credit_data$OneMonth_PaymentAmount_Ratio<-ifelse(is.nan(credit_data$PAY_AMT1/credit_data$SixMonth_PaymentAmount),0,
                                          ifelse(is.infinite(credit_data$PAY_AMT1/credit_data$SixMonth_PaymentAmount),0,round(credit_data$PAY_AMT1/credit_data$SixMonth_PaymentAmount,2)))

credit_data$TwoMonth_PaymentAmount_Ratio<-ifelse(is.nan(credit_data$TwoMonth_PaymentAmount/credit_data$SixMonth_PaymentAmount),0,
                                          ifelse(is.infinite(credit_data$TwoMonth_PaymentAmount/credit_data$SixMonth_PaymentAmount),0,round(credit_data$TwoMonth_PaymentAmount/credit_data$SixMonth_PaymentAmount,2)))

credit_data$ThreeMonth_PaymentAmount_Ratio<-ifelse(is.nan(credit_data$ThreeMonth_PaymentAmount/credit_data$SixMonth_PaymentAmount),0,
                                            ifelse(is.infinite(credit_data$ThreeMonth_PaymentAmount/credit_data$SixMonth_PaymentAmount),0,round(credit_data$ThreeMonth_PaymentAmount/credit_data$SixMonth_PaymentAmount,2)))

credit_data$FourMonth_PaymentAmount_Ratio<-ifelse(is.nan(credit_data$FourMonth_PaymentAmount/credit_data$SixMonth_PaymentAmount),0,
                                           ifelse(is.infinite(credit_data$FourMonth_PaymentAmount/credit_data$SixMonth_PaymentAmount),0,round(credit_data$FourMonth_PaymentAmount/credit_data$SixMonth_PaymentAmount,2)))

credit_data$FiveMonth_PaymentAmount_Ratio<-ifelse(is.nan(credit_data$FiveMonth_PaymentAmount/credit_data$SixMonth_PaymentAmount),0,
                                           ifelse(is.infinite(credit_data$FiveMonth_PaymentAmount/credit_data$SixMonth_PaymentAmount),0,round(credit_data$FiveMonth_PaymentAmount/credit_data$SixMonth_PaymentAmount,2)))


#Feature 16 - Payment Amount v/s six month bill amount ratios
credit_data$OneMonth_PaymentandBill_Ratio<-ifelse(is.nan(credit_data$PAY_AMT1/credit_data$SixMonth_BillAmount),0,
                                           ifelse(is.infinite(credit_data$PAY_AMT1/credit_data$SixMonth_BillAmount),0,round(credit_data$PAY_AMT1/credit_data$SixMonth_BillAmount,2)))

credit_data$TwoMonth_PaymentandBill_Ratio<-ifelse(is.nan(credit_data$TwoMonth_PaymentAmount/credit_data$SixMonth_BillAmount),0,
                                           ifelse(is.infinite(credit_data$TwoMonth_PaymentAmount/credit_data$SixMonth_BillAmount),0,round(credit_data$TwoMonth_PaymentAmount/credit_data$SixMonth_BillAmount,2)))

credit_data$ThreeMonth_PaymentandBill_Ratio<-ifelse(is.nan(credit_data$ThreeMonth_PaymentAmount/credit_data$SixMonth_BillAmount),0,
                                             ifelse(is.infinite(credit_data$ThreeMonth_PaymentAmount/credit_data$SixMonth_BillAmount),0,round(credit_data$ThreeMonth_PaymentAmount/credit_data$SixMonth_BillAmount,2)))

credit_data$FourMonth_PaymentandBill_Ratio<-ifelse(is.nan(credit_data$FourMonth_PaymentAmount/credit_data$SixMonth_BillAmount),0,
                                            ifelse(is.infinite(credit_data$FourMonth_PaymentAmount/credit_data$SixMonth_BillAmount),0,round(credit_data$FourMonth_PaymentAmount/credit_data$SixMonth_BillAmount,2)))

credit_data$FiveMonth_PaymentandBill_Ratio<-ifelse(is.nan(credit_data$FiveMonth_PaymentAmount/credit_data$SixMonth_BillAmount),0,
                                            ifelse(is.infinite(credit_data$FiveMonth_PaymentAmount/credit_data$SixMonth_BillAmount),0,round(credit_data$FiveMonth_PaymentAmount/credit_data$SixMonth_BillAmount,2)))

credit_data$SixMonth_PaymentandBill_Ratio<-ifelse(is.nan(credit_data$SixMonth_PaymentAmount/credit_data$SixMonth_BillAmount),0,
                                           ifelse(is.infinite(credit_data$SixMonth_PaymentAmount/credit_data$SixMonth_BillAmount),0,round(credit_data$SixMonth_PaymentAmount/credit_data$SixMonth_BillAmount,2)))

#Feature 17 - pay status added up
credit_data$PAY_TwoMonth<-rowSums(credit_data[,7:8])
credit_data$PAY_ThreeMonth<-rowSums(credit_data[,7:9])
credit_data$PAY_FourMonth<-rowSums(credit_data[,7:10])
credit_data$PAY_FiveMonth<-rowSums(credit_data[,7:11])
credit_data$PAY_SixMonth<-rowSums(credit_data[,7:12])


#Feature 19 - counting number of 0's or positives
var_1<-credit_data%>%
  select(13:24)

credit_data$Zero_Values<-apply(var_1,1,function(x)sum(x == 0))

Var_2<-credit_data%>%
  select(7:12)

credit_data$Positive_Values<-apply(Var_2,1,function(x)sum(x>=0))

credit_data$Zero_Values<-as.factor(credit_data$Zero_Values)
credit_data$Positive_Values<-as.factor(credit_data$Positive_Values)

#feature 20- bill amount to limit balance ratios
#credit_data$bill1<-credit_data$BILL_AMT1/credit_data$LIMIT_BAL
#credit_data$bill2<-credit_data$BILL_AMT2/credit_data$LIMIT_BAL
#credit_data$bill3<-credit_data$BILL_AMT3/credit_data$LIMIT_BAL
#credit_data$bill4<-credit_data$BILL_AMT4/credit_data$LIMIT_BAL
#credit_data$bill5<-credit_data$BILL_AMT5/credit_data$LIMIT_BAL
#credit_data$bill6<-credit_data$BILL_AMT6/credit_data$LIMIT_BAL

#credit_data$bill1[is.na(credit_data$bill1)]<-0
#credit_data$bill2[is.na(credit_data$bill2)]<-0
#credit_data$bill3[is.na(credit_data$bill3)]<-0
#credit_data$bill4[is.na(credit_data$bill4)]<-0
#credit_data$bill5[is.na(credit_data$bill5)]<-0
#credit_data$bill6[is.na(credit_data$bill6)]<-0

#Feature 21 - payment amount to limit balance ratios
#credit_data$pay_limi1<-ifelse(is.nan(credit_data$PAY_AMT1/credit_data$LIMIT_BAL),0,
                                                  #ifelse(is.infinite(credit_data$PAY_AMT1/credit_data$LIMIT_BAL),0,round(credit_data$PAY_AMT1/credit_data$LIMIT_BAL,2)))
#credit_data$pay_limi2<-ifelse(is.nan(credit_data$PAY_AMT2/credit_data$LIMIT_BAL),0,
                              #ifelse(is.infinite(credit_data$PAY_AMT2/credit_data$LIMIT_BAL),0,round(credit_data$PAY_AMT2/credit_data$LIMIT_BAL,2)))
#credit_data$pay_limi3<-ifelse(is.nan(credit_data$PAY_AMT3/credit_data$LIMIT_BAL),0,
                              #ifelse(is.infinite(credit_data$PAY_AMT3/credit_data$LIMIT_BAL),0,round(credit_data$PAY_AMT3/credit_data$LIMIT_BAL,2)))
#credit_data$pay_limi4<-ifelse(is.nan(credit_data$PAY_AMT4/credit_data$LIMIT_BAL),0,
                              #ifelse(is.infinite(credit_data$PAY_AMT4/credit_data$LIMIT_BAL),0,round(credit_data$PAY_AMT4/credit_data$LIMIT_BAL,2)))
#credit_data$pay_limi5<-ifelse(is.nan(credit_data$PAY_AMT5/credit_data$LIMIT_BAL),0,
                              #ifelse(is.infinite(credit_data$PAY_AMT5/credit_data$LIMIT_BAL),0,round(credit_data$PAY_AMT5/credit_data$LIMIT_BAL,2)))
#credit_data$pay_limi6<-ifelse(is.nan(credit_data$PAY_AMT6/credit_data$LIMIT_BAL),0,
                              #ifelse(is.infinite(credit_data$PAY_AMT6/credit_data$LIMIT_BAL),0,round(credit_data$PAY_AMT6/credit_data$LIMIT_BAL,2)))



#Age
credit_data$AGE<-ifelse(credit_data$AGE>=18 & credit_data$AGE<=25,"18-25",
                       ifelse(credit_data$AGE>=26 & credit_data$AGE <=35,"26-35",
                              ifelse(credit_data$AGE>=36 & credit_data$AGE<=45,"36-45",
                                     ifelse(credit_data$AGE>=46 & credit_data$AGE <=55,"46-55",
                                            ifelse(credit_data$AGE>=56 & credit_data$AGE<=65,"56-65",
                                                   ifelse(credit_data$AGE>65,"65+","NA"))))))

################ data formatting ##################################

#Converting the variables that need to be converted into factors
credit_data$SEX<-as.factor(credit_data$SEX)
credit_data$EDUCATION<-as.factor(credit_data$EDUCATION)
credit_data$MARRIAGE<-as.factor(credit_data$MARRIAGE)
credit_data$PAY_1<-as.factor(credit_data$PAY_1)
credit_data$PAY_2<-as.factor(credit_data$PAY_2)
credit_data$PAY_3<-as.factor(credit_data$PAY_3)
credit_data$PAY_4<-as.factor(credit_data$PAY_4)
credit_data$PAY_5<-as.factor(credit_data$PAY_5)
credit_data$PAY_6<-as.factor(credit_data$PAY_6)
credit_data$default_0<-as.factor(credit_data$default_0)
credit_data$AGE<-as.factor(credit_data$AGE)

str(credit_data)


####################### Train and Test Data Sets ####################

##bringing the data set back to orginal training and prediction set
str(credit_data)

head(credit_data[,25])
#credit_data<-credit_data%>%
 # select(1:12,25:90)

credit_data<-credit_data[,-1]
credit_data_revamp<-credit_data[1:24000,]
application_data<-credit_data[24001:25000,]

#splitting dataset into train and test
inTrain <- createDataPartition(y = credit_data_revamp$default_0,
                               p = 16799/24000, list = FALSE)
training <- credit_data_revamp[ inTrain,]
testing <- credit_data_revamp[ -inTrain,]


str(training)

#######################Logistic Model###########################
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071") #Check, and if needed install the necessary packages

model_logistic<-glm(default_0~., data=training, family="binomial"(link="logit"))
summary(model_logistic) 


## Stepwise regressions to select only variables that are important
model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1)
summary(model_logistic_stepwiseAIC) 

par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC) #Error plots: similar nature to lm plots
par(mfrow=c(1,1))

###Finding predicitons: probabilities and classification
logistic_probabilities<-predict(model_logistic,newdata=testing,type="response")
logistic_classification<-rep("1",7200)
logistic_classification[logistic_probabilities<0.21]="0" #Predict classification using 0.6073 threshold. Why 0.6073 - that's the average probability of being retained in the data. An alternative code: logistic_classification <- as.integer(logistic_probabilities > mean(testing$Retained.in.2012. == "1"))
logistic_classification<-as.factor(logistic_classification)

###Confusion matrix  
confusionMatrix(logistic_classification,testing$default_0,positive = "1") #Display confusion matrix

####ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, testing$default_0)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr")
plot(logistic_ROC) 



####AUC (area under curve)
auc.tmp <- performance(logistic_ROC_prediction,"auc") 
logistic_auc_testing <- as.numeric(auc.tmp@y.values) 
logistic_auc_testing 

 #### Lift chart
plotLift(logistic_probabilities, testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

################################ CART #############################
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not
pacman::p_load("caret","partykit","ROCR","lift","rpart","e1071")

###
##ctree model
###

str(training)
#Run ctree on training data

ctree_tree<-ctree(default_0~.,data=training) #Run ctree on training data
ctree_probabilities<-predict(ctree_tree,newdata=testing,type="prob")
ctree_classification<-rep("1",7200)
ctree_classification[ctree_probabilities[,2]<0.21]="0" 
ctree_classification<-as.factor(ctree_classification)

###Confusion matrix  
confusionMatrix(ctree_classification,testing$default_0,positive = "1")

####ROC Curve
ctree_probabilities_testing <-predict(ctree_tree,newdata=testing,type = "prob") 
ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], testing$default_0) 
ctree_ROC_testing <- performance(ctree_pred_testing,"tpr","fpr") #Create ROC curve data
plot(ctree_ROC_testing) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(ctree_pred_testing,"auc") #Create AUC data
ctree_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
ctree_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(ctree_probabilities[,2],  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

#################################Rpart################################

CART_cp = rpart.control(cp = 0.0005) #set cp to a small number to "grow" a large tree

rpart_tree<-rpart(default_0~.,data=training, method="class", control=CART_cp) #"Grow" a tree on training data

plotcp(rpart_tree) # Use printcp(rpart_tree) to print the values. As a rule of thumb pick up the largest cp which does not give a substantial drop in error
prunned_rpart_tree<-prune(rpart_tree, cp=0.0012) #Prun the tree. Play with cp to see how the resultant tree changes
plot(as.party(prunned_rpart_tree), type = "extended",gp = gpar(fontsize = 7)) #Plotting the tree (adjust fontsize if needed)

# Understand the relationship between the cross-validated error, size of the tree and cp.

rpart_prediction_class<-predict(prunned_rpart_tree,newdata=testing, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class,testing$default_0,positive = "1") #Display confusion matrix

rpart_probabilities_testing <-predict(prunned_rpart_tree,newdata=testing,type = "prob") #Predict probabilities
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], testing$default_0) #Calculate errors
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing,"auc") #Create AUC data
rpart_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing #Display AUC value

plotLift(rpart_prediction_class,  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

##############################random forest############################
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","randomForest") #Check, and if needed install the necessary packages

model_forest <- randomForest(default_0~ ., data=training, 
                             type="classification",
                             importance=TRUE,
                             ntree = 50,           # hyperparameter: number of trees in the forest
                             mtry = 10,             # hyperparameter: number of random columns to grow each tree
                             nodesize = 10,         # hyperparameter: min number of datapoints on the leaf of each tree
                             maxnodes = 10,         # hyperparameter: maximum number of leafs of a tree
                             cutoff = c(0.5, 0.5)   # hyperparameter: how the voting works; (0.5, 0.5) means majority vote
) 

plot(model_forest)  # plots error as a function of number of trees in the forest; use print(model_forest) to print the values on the plot

varImpPlot(model_forest) # plots variable importances; use importance(model_forest) to print the values

###Finding predicitons: probabilities and classification
forest_probabilities<-predict(model_forest,newdata=testing,type="prob") #Predict probabilities -- an array with 2 columns: for not retained (class 0) and for retained (class 1)
forest_classification<-rep("1",7200)
forest_classification[forest_probabilities[,2]<0.5]="0" #Predict classification using 0.5 threshold. Why 0.5 and not 0.6073? Use the same as in cutoff above
forest_classification<-as.factor(forest_classification)

confusionMatrix(forest_classification,testing$default_0, positive="1") #Display confusion matrix. Note, confusion matrix actually displays a better accuracy with threshold of 50%

#There is also a "shortcut" forest_prediction<-predict(model_forest,newdata=testing, type="response") 
#But it by default uses threshold of 50%: actually works better (more accuracy) on this data


####ROC Curve
forest_ROC_prediction <- prediction(forest_probabilities[,2], testing$default_0) #Calculate errors
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(forest_ROC) #Plot ROC curve

####AUC (area under curve)
AUC.tmp <- performance(forest_ROC_prediction,"auc") #Create AUC data
forest_AUC <- as.numeric(AUC.tmp@y.values) #Calculate AUC
forest_AUC #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(forest_probabilities[,2],  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_forest <- performance(forest_ROC_prediction,"lift","rpp")
plot(Lift_forest)

###############################xgboost###########################
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","xgboost") #Check, and if needed install the necessary packages

STCdata_A_matrix <- model.matrix(default_0~., data = credit_data_revamp)

x_train <- STCdata_A_matrix[ inTrain,]
x_test <- STCdata_A_matrix[ -inTrain,]

y_train <-training$default_0
y_test <-testing$default_0

model_XGboost<-xgboost(data = data.matrix(x_train), 
                       label = as.numeric(as.character(y_train)), 
                       eta = 0.1,       # hyperparameter: learning rate 
                       max_depth = 20,  # hyperparameter: size of a tree in each boosting iteration
                       nround=50,       # hyperparameter: number of boosting iterations  
                       objective = "binary:logistic"
)

XGboost_prediction<-predict(model_XGboost,newdata=x_test, type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction>0.221,1,0)),y_test,positive="1") #Display confusion matrix

####ROC Curve
XGboost_ROC_prediction <- prediction(XGboost_prediction, y_test) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_ROC_prediction,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(XGboost_prediction, y_test, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_XGboost <- performance(XGboost_ROC_prediction,"lift","rpp")
plot(Lift_XGboost)

