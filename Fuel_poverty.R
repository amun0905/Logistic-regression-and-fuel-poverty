fp<-read.csv("FuelPoverty_2019.csv",header = TRUE,sep=",",fileEncoding="UTF-8-BOM")

#Logistic regression is a method for fitting a regression curve, y = f(x), when y is a categorical variable. 
#The typical use of this model is predicting y given a set of predictors x. 
#The predictors can be continuous, categorical or a mix of both.
View(fp)
library(dplyr) 
library(ggplot2)
library(caTools)
library(caret)
#install.packages("ROSE") 
#package for random over sampling
library(ROSE)
library(pROC)

fp<-rename(fp, c("In_fuel_Poverty"="fpLILEEflg","hh_income"="fpfullinc","fuel_costs"="fuelexpn", 
                 "Under_occupied"="Unoc","Region"="gorehs", "Tenure"="tenure4x","Head_Working_Status"="emphrp3x",
                 "Head_Ethnic_Origin"="ethhrp2x","Age_of_youngest"="Ageyng","Age_of_oldest"="Ageold"))

#here our binary response variable (In Fuel Poverty) is a dummy 0/1 variable - 0 for not fuel poor, 1 for fuel poor

str(fp)#In_fuel_Poverty should be a factor and not 'numeric'
fp$In_fuel_Poverty<-as.factor(fp$In_fuel_Poverty)

#Lets explore our sample using graphs
#Region
fp$Region <- recode_factor(fp$Region, "1" = "North East","2"="North West", "4"="Yorkshire and the Humber", "5"="East Midlands",
                           "6"="West Midlands", "7"="East", "8"="London","9"="South East","10"="South West")
p1<-ggplot(fp, aes(x=Region))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p1

#Tenure
fp$Tenure <- recode_factor(fp$Tenure, "1" = "owner occupied","2"="private rented", "3"="local authority", "4"="housing association")

p2<-ggplot(fp, aes(x=Tenure))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p2

#Dwelling type
fp$DWtype <- recode_factor(fp$DWtype, "1" = "end terrace","2"="mid terrace", "3"="semi detached", "4"="detached",
                           "5"="purpose built","6"="converted and non residential")

p3<-ggplot(fp, aes(x=DWtype))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p3#there is a category for converted and non residential. I don't want these in my sample, as we are only
#interested in residential properties

#Delete these from your dataframe!
fp<-subset(fp,!fp$DWtype=="converted and non residential")

#Floor area - will not use it here, but this is how you would recode it:
#fp$FloorArea <- recode_factor(fp$FloorArea, "1" = "less than 50","2"="50 to 69", "3"="70 to 89", "4"="90 to 109",
                             # "5"="110 sqm or more","-8"="unknown")

#create your own plots for Mainfueltype, hhcompx, hhsize, FloorArea

#Working status
fp$Head_Working_Status<- recode_factor(fp$Head_Working_Status, "1" = "working","2"="unemployed", "3"="inactive")
p4<-ggplot(fp, aes(x=Head_Working_Status))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p4

#Ethnic origin
fp$Head_Ethnic_Origin<- recode_factor(fp$Head_Ethnic_Origin, "1" = "white","2"="ethnic minority")

p5<-ggplot(fp, aes(x=Head_Ethnic_Origin))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p5

#EPC
fp$EPC<- recode_factor(fp$EPC, "1" = "A/B/C","2"="D", "3"="E","4"="F","5"="G")

p6<-ggplot(fp, aes(x=EPC))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p6

fp$hhcompx<- recode_factor(fp$hhcompx, "1" = "couple, no child(ren) under 60","2"="couple, no child(ren) 60 or over","3"="couple with child(ren)",
                           "4"="lone parent with child(ren)","5"="other multi-person households","6"="one person under 60","7"="one person aged 60 or over")
fp$fpvuln<- recode_factor(fp$fpvuln, "1" = "Vulnerable","0"="Not vulnerable")
#A household is vulnerable if it contains at least one member who is 65 or older, younger than 5 
#or living with a long-term health condition 

#lets fit our fist model

set.seed(101) #we use set.seed so that the next time we run the analysis, we will get
# the same randomisation
#create dataframe which contains response and features to be used
fp_log <- fp[,c("In_fuel_Poverty","fpvuln", "Region", "Tenure", "hhcompx", "Head_Working_Status", "Head_Ethnic_Origin")] 
sample = sample.split(fp_log$In_fuel_Poverty, SplitRatio = .70)#a caTools function

train = subset(fp_log, sample == TRUE)
test  = subset(fp_log, sample == FALSE)
str(train)
mylog1<- glm(In_fuel_Poverty ~  fpvuln+ Region+ Tenure+ hhcompx+ Head_Working_Status+ Head_Ethnic_Origin, data = train, family = "binomial") 
summary(mylog1)
library(blorr) #load it to be able to use a Mc Fadden R2: a pseudo-R2 using maximum likelihood
#other R2 measures use OLS
#McFadden's R2 indicates a very good fit when it ranges 0.2-0.4 and best used
#to compare fit among different models

blr_rsq_mcfadden(mylog1)#apparently a quite poor model

#now we need to predict the test set results
pred_logistic<-predict(mylog1, type='response', newdata=test)#set type 'response' for probabilities
pred_logistic #produces probabilities! we need to choose a threshold.
y_pred_logistic<-ifelse(pred_logistic>0.5,1,0)
confusionMatrix(as.factor(y_pred_logistic),test[,1],positive = "1")
#Accuracy : 0.85, Sensitivity : 0.04, Specificity : 0.99
#Accuracy: N correct pred/tot N pred
#Sensitivity: (True Positive)/(True Positive + False Negative)
#Specificity:(True Negative)/(True Negative + False Positive)

#what is happening here?
#model is unable to predict the minority class. 
accuracy.meas(test$In_fuel_Poverty, y_pred_logistic)#from ROSE
#Precision: 0.6, Recall (Sensitivity): 0.04, F: 0.04!! - what a poor model! 
#Precision: (True Positive)/(True Positive + False Positive)
#F: (2 * Precision * Recall) / (Precision + Recall)


#add the column of predictions to our test set:
log_output <- cbind(test, y_pred_logistic)

#ROC Curve using pROC package
#The ROC plots the true positive rate (TPR) against the false positive rate (FPR) 
#at various threshold settings. The AUC is the area under the ROC curve. 
#A model with good predictive ability should have an AUC closer to 1 than to 0.5.
test_roc_log = roc(test$In_fuel_Poverty ~ y_pred_logistic, plot = TRUE, print.auc = TRUE)#AUC:0.52 very poor


#lets observe the 2 classes for fuel poverty in our train set - are they balanced?
table(train$In_fuel_Poverty)#zero classes are 6 times the number of the 1 classes
#this can cause many problems - we need more entries for our minority class (1)
# lets oversample - create more data for the minority class
# we could use SMOTE but be careful - this requires all features to be numerical (uses a k-nn algo)
# instead, we will use random oversampling - we will increase the sample size of the minority class in the train data

train_balanced_over <- ovun.sample(In_fuel_Poverty ~ ., data = train, method = "over",N = 13000)$data
table(train_balanced_over$In_fuel_Poverty)#minority class sample now increased to 6160, totalling 13000 sample size

#lets re run the model on more balanced data:
mylog2<- glm(In_fuel_Poverty ~  fpvuln+ Region+ Tenure+ hhcompx+ Head_Working_Status+ Head_Ethnic_Origin, data = train_balanced_over, family = "binomial") 
summary(mylog2)#how do we interpret these results?

#Lets interpret a few significant variables:
#fpvuln: Being a non-vulnerable household decreases the log odds of being in fuel poverty (versus not being)
#A household is now counted as vulnerable in these statistics if it contains at least one household member who 
#is 65 or older, younger than 5 or living with a long-term health condition affecting mobility, breathing, heart or mental health condition. 

#Region: Living in West Midlands increases the log odds of being in fuel poverty, compared to
#living in the North East. 
#Living in London and the SE decreases the log odds of being in fuel poverty, compared to
#living in the North East.

#Tenure: Living in private rented, local authority or housing association homes increases the log odds of being in fuel poverty,
#compared to living in an owner occupied dwelling. So the ODDS increase by (e^Estimate)

#house composition: Over 60s Couples without children have increased log odds of being in fuel poverty,
#compared to Under 60s Couples without children. Same applies for most remaining categories.

#Working status: Houses where the head is unemployed or inactive, have increased log odds of being fuel poor,
#compared to houses where the head is working

#Ethnic group: Houses where the head belongs to an ethnic minority have increased log odds of being
#fuel poor compared to houses where the head is white
blr_rsq_mcfadden(mylog2)#apparently a poor model

#predict the test set results
pred_logistic<-predict(mylog2, type='response', newdata=test)#set type 'response' for probabilities
pred_logistic #produces probabilities! we need to choose a threshold.
y_pred_logistic<-ifelse(pred_logistic>0.5,1,0)
confusionMatrix(as.factor(y_pred_logistic),test[,1],positive = "1")
#Accuracy : 0.7, Sensitivity : 0.6, Specificity : 0.71

#ROC Curve using pROC package
test_roc_log = roc(test$In_fuel_Poverty ~ y_pred_logistic, plot = TRUE, print.auc = TRUE)#AUC:0.67 
#still poor, but better than with mylog1 model



