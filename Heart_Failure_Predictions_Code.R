#Read CSV file and call 
Heart = read.csv("heart_failure_clinical_records_dataset.csv",na.strings ='?')
#fix(Heart)
Heart<-na.omit(Heart)
#dim()
attach(Heart)
dim(Heart)
View(Heart)

#Summary Statistics-(variable names, boxplots, histograms, mean, SD)
names(Heart)

#Mean and SD
round(sapply(Heart[,1:13],mean),5)
round(sapply(Heart[,1:13],sd),5)
#Min, Max, Median, and Mean
for(i in c(2,4,6,10,11,13)){
  Heart[,i]<-as.factor(Heart[,i])
}
summary(Heart)

#Boxplot of significant variables
par(mfrow=c(2,5))
boxplot(age~DEATH_EVENT, main="Boxplot of age")
boxplot(ejection_fraction~DEATH_EVENT, main="Boxplot of ejection_fraction")
boxplot(serum_creatinine~DEATH_EVENT, main="Boxplot of serum_creatinine",ylim=c(0,5))
boxplot(serum_sodium~DEATH_EVENT, main="Boxplot of serum_sodium")
boxplot(time~DEATH_EVENT, main="Boxplot of time")

#histogram for boolean and numeric variables
par(mfrow=c(2,6))
counts <- table(Heart$anaemia)
barplot(counts, main="Histogram of anemia",
        xlab="anemia",beside = TRUE)
counts <- table(Heart$diabetes)
barplot(counts, main="Histogram of diabetes",
        xlab="diabetes",beside = TRUE)
counts <- table(Heart$high_blood_pressure)
barplot(counts, main="Histogram of high blood pressure",
        xlab="high blood pressure",beside = TRUE)
counts <- table(Heart$sex)
barplot(counts, main="Histogram of sex",
        xlab="sex",beside = TRUE)
counts <- table(Heart$smoking)
barplot(counts, main="Histogram of smoking",
        xlab="smoking",beside = TRUE)
counts <- table(Heart$DEATH_EVENT)
barplot(counts, main="Histogram of death event",
        xlab="death event",beside = TRUE)
par(mfrow=c(2,4))
hist(age)
hist(creatinine_phosphokinase)
hist(ejection_fraction)
hist(platelets)
hist(serum_creatinine)
hist(serum_sodium)
hist(time)


#set the training and test dataset 
set.seed(1)
train = sample(1:nrow(Heart), nrow(Heart)/2)
Heart.test=Heart[-train,]
#Convert boolean 0 and 1 to numerical
Heart$sex = as.factor(Heart$sex)
Heart$diabetes = as.factor(Heart$diabetes)
Heart$anaemia = as.factor(Heart$anaemia)
Heart$high_blood_pressure = as.factor(Heart$high_blood_pressure)
Heart$smoking = as.factor(Heart$smoking)
Heart$DEATH_EVENT = as.factor(Heart$DEATH_EVENT)
str(Heart)


# run Logistic Regression model
set.seed(1)
library(boot)
glm.fits=glm(DEATH_EVENT~.,data=Heart,family = binomial, subset= train)
summary(glm.fits)
#checking model accuracy 
glm.probs=predict(glm.fits,Heart.test, type="response")
#0 is no death, 1 is death
#there are 299 observations of heart, 150 in test data
glm.pred=rep(0,150)
glm.pred[glm.probs>.5]=1
# compare the predicted value of DV with the true value of DV in the test dataset
DEATH_EVENT.test= DEATH_EVENT[-train]
table(glm.pred,DEATH_EVENT.test)
# calculate the accuracy rate 
mean(glm.pred==DEATH_EVENT.test)
#accuracy rate is 88%
# calculate the test error
mean(glm.pred!=DEATH_EVENT.test)
#test error rate is 12%

#Backward selection method (threshold: p<0.10)
#Backward selection method 1: Deleted smoking
glm.fits1=glm(DEATH_EVENT~.-smoking,data=Heart,family = binomial, subset=train)
summary(glm.fits1)
#Backward selection method 2: Deleted platelets
glm.fits1=glm(DEATH_EVENT~age+anaemia+creatinine_phosphokinase+diabetes+ejection_fraction+high_blood_pressure+serum_creatinine+serum_sodium+sex+time,data=Heart,family = binomial,subset=train)
summary(glm.fits1)
#Backward selection method 3: Deleted anaemia
glm.fits1=glm(DEATH_EVENT~age+creatinine_phosphokinase+diabetes+ejection_fraction+high_blood_pressure+serum_creatinine+serum_sodium+sex+time,data=Heart,family = binomial,subset=train)
summary(glm.fits1)
#Backward selection method 4: Deleted diabetes
glm.fits1=glm(DEATH_EVENT~age+creatinine_phosphokinase+ejection_fraction+high_blood_pressure+serum_creatinine+serum_sodium+sex+time,data=Heart,family = binomial,subset=train)
summary(glm.fits1)
#Backward selection method 5: Deleted sex
glm.fits1=glm(DEATH_EVENT~age+creatinine_phosphokinase+ejection_fraction+high_blood_pressure+serum_creatinine+serum_sodium+time,data=Heart,family = binomial,subset=train)
summary(glm.fits1)
#Backward selection method 6: Deleted high_blood_pressure
glm.fits1=glm(DEATH_EVENT~age+creatinine_phosphokinase+ejection_fraction+serum_creatinine+serum_sodium+time,data=Heart,family = binomial,subset=train)
summary(glm.fits1)
#Backward selection method 7: Deleted creatinine_phosphokinase 
glm.fits1=glm(DEATH_EVENT~age+ejection_fraction+serum_creatinine+serum_sodium+time,data=Heart,family = binomial,subset=train)
summary(glm.fits1)


#checking model accuracy of smaller model (only significant variables) 
glm.probs1=predict(glm.fits1,Heart.test, type="response")
glm.pred1=rep(0,150)
glm.pred1[glm.probs1>.5]=1
# compare the predicted value of DV with the true value of DV in the test dataset
DEATH_EVENT.test= DEATH_EVENT[-train]
table(glm.pred1,DEATH_EVENT.test)
# calculate the accuracy rate 
mean(glm.pred1==DEATH_EVENT.test)
# accuracy rate is 86.6%.
# calculate the test error
mean(glm.pred1!=DEATH_EVENT.test)
# test error rate is 13.3%.


#Check for Collinearity
install.packages("car")
library(car)
glm.fits1=glm(DEATH_EVENT~ age+ejection_fraction+serum_creatinine+serum_sodium+time,data=Heart,family = binomial, subset=train)
vif(glm.fits1)
#There is no evidence of problematic amounts of co-linearity because VIF values less than 5 or 10. 


#RANDOM FOREST 
install.packages(c("gbm","randomForest"))
library(randomForest)
set.seed(1)
rf.Heart=randomForest(DEATH_EVENT~., data = Heart, subset = train, mtry=sqrt(12),importance=TRUE)
rf.Heart

# accuracy rate is 80.5%.
((29 + 91)/ (91+19+10+29))
# precision rate is 60.4%.
((29)/(29+19))
# recall rate is 74.3%.
((29)/(29+10))

importance(rf.Heart)
varImpPlot(rf.Heart)
#time has the most variable importance.


