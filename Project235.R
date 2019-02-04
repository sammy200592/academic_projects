obs_data<- read.csv("C:/Users/SaMMy/Downloads/Obesity in America_2015.csv")
head(obs_data)
table_age<- subset(obs_data,obs_data$Category=="Age")
plot(table_age$X..Obese~table_age$Sub.Category)
levels(table_age$Sub.Category)
x<-c("Male","Female")
table_gender<- subset(obs_data,obs_data$Category=="Gender")
plot(table_gender$X..Obese~table_gender$Sub.Category)

table_income<- subset(obs_data,obs_data$Category=="Income")
plot(table_income$X..Obese~table_income$Sub.Category)

head(table_gender)
?subset
obs1<-read.csv("C:/Users/SaMMy/Desktop/original_weight_data.csv")
head(obs1)
names(obs1)
obs1<-read.csv("C:/Users/SaMMy/Documents/Projectfile.csv")
head(obs1)
foo <- obs1$Age.years.[, colSums(is.na(obs1$Age.years.)) == 0]
class(obs1$Age.years.)
levels(obs1$Age.years.)
plot(obs1$Data_Value~obs1$Age.years.)
?plot
plot(obs1$Data_Value~ obs1$Age.years.)
summary(obs1)
class(obs1)
sapply(obs1, class)
head(obs1)
dim(obs1)
summary(obs1)

summary(obs1$Data_Value)

names(obs1)
plot(obs1$Data_Value~obs1$Age.years.,xlab="Age",ylab = "obesity value", main="Age vs Obesity")


plot(obs1$Data_Value~obs1$Gender,main="Gender vs Obesity", xlab="gender",ylab = "obesity value")

plot(obs1$Data_Value~obs1$Income, main="Income vs Obesity",xlab="income",ylab = "obesity value")

plot(obs1$Data_Value~obs1$Education,main="Education vs Obesity",xlab="Education",ylab = "Obesity value")
names(obs1)0
lm1<-lm(Data_Value~Education,data=obs1)
summary(lm1)
lm2<-lm(Data_Value~Age.years.,data=obs1)
summary(lm2)
lm3<-lm(Data_Value~Gender,data=obs1)
summary(lm3)
lm4<-lm(Data_Value~Income,data = obs1)
summary(lm4)
compmdl<-lm(Data_Value~Age.years.+Education+Gender+Income,data = obs1)
summary(compmdl)
newmdl<-lm(Data_Value~)
head(obs1)
obs1$Gender=="Male"
class(obs1$Income)
levels(obs1$Education)
levels(obs1$Gender)!="Male"
x<-lm(Data_Value ~ factor(Income, exclude=c("$35,000 - $49,999","$50,000 - $74,999","Data not reported")) + factor(Gender,exclude = "Female")+factor(Age.years.,exclude = "35 - 44")+factor(Education,exclude="Some college or technical school"),data = obs1)
plot(x)
?par()
?mar
par(mfrow=c(2,2))
summary(x)
install.packages("lattice")
library("DAAG")
mdlcv<-CVlm(obs1, formula(Data_Value ~ factor(Income, exclude=c("$35,000 - $49,999","$50,000 - $74,999","Data not reported")) +factor(Gender,exclude = "Female")+factor(Age.years.,exclude = "35 - 44")+factor(Education,exclude="Some college or technical school"), 10)
plot(x$fitted.values,x$residuals,main = "Residual plot",xlab="Residual",ylab = "Fitted values")
summary(obs1)    
training.rows <- sample(1:nrow(obs1), size =3600 )
obs.train <- obs1[training.rows, ]
obs.test <- obs1[-training.rows, ]
obs.test
full.model <- lm(Data_Value ~  factor(Income, exclude=c("$35,000 - $49,999","$50,000 - $74,999","Data not reported")) + factor(Gender,exclude = "Female")+factor(Age.years.,exclude = "35 - 44")+factor(Education,exclude="Some college or technical school"),data = obs.train)
names(obs.train)
dim(obs1)
summary(full.model) 

step.model <- step(full.model)

summary(step.model)
###
## Making predictions on our testing set
full.predictions <- predict(full.model, newdata = obs.test)
sqrt(mean((full.predictions - obs.test$Data_Value)^2))
l1pred<-predict(lasso.)
## Using the model constructed stepwise
step.predictions <- predict(step.model, newdata = obs.test)
sqrt(mean((step.predictions - obs.test$Data_Value)^2)) ## .858                        
cor(obs1)            
?cor
cook<-cooks.distance(x)
cook
plot(cook, ylab = "Cook's Distance", main="Cooks Distance")
