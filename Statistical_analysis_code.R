#Author: Rubén Sánchez Fernández

#This is the code for the statistical analysis project.

##--------------------------------------------------------------##

#load libraries
library(Hmisc)

##--------------------------------------------------------------##

#DATA OVERVIEW

#import dataset
getHdata(acath)

#dimension dataset
dim(acath)

#names variables
names(acath)

#summary dataset
summary(acath)

#internal structure 
str(acath)


##-------------------------------------------------------------##

# QUESTIONS

#histogram plot of symptoms duration
hist(acath$cad.dur, col="darkseagreen1", main="Distribution of heart symptoms \n in the coronary artery disease", xlab="Time", ylab="Frequency")


#How old is the older patient?
print(paste0("The older patient is ", max(acath$age), " years old"))

#And the youngest?
print(paste0("and the youngest patient is ", min(acath$age), " years old"))

#How many patients have high level of cholesterol? (more than 200)
nrow(subset(acath, acath$choleste>200))

##--------------------------------------------------------------##

# PROBABILITY

#Probability to find more than 60 patients evaluating 100 tests?
est<-subset(acath,acath$sigdz==1) #patients with the disease
p<-nrow(est)/nrow(acath) #probability = patients with the disease/total patients
p60<-1-pbinom(60,100,p,lower.tail=T) #dist binomial
print(paste0("The probability to find more than 60 patients with the disease after evaluating 100 tests is ", p60))


#Probability of not having the disease if someone has a narrow artery?
sya<-subset(acath,acath$tvdlm==0 & acath$sigdz==1)
psya<-nrow(sya)/nrow(acath) #probability no disease and narrow artery
ps<-psya/p #conditional probability
print(paste0("Probability of not having the disease while having narrow coronary artery is ",ps))


#simulation
set.seed(12345)
t<-rbinom(10000,100,p)
str(t)
#plots
par(mfrow=c(1,2))
hist(t,col="lightblue")
plot(density(t))
mean(t)
sd(t)


##--------------------------------------------------------------------------##

#REGRESSION ANALYSIS

#scatterplots all variables
pairs(acath)

#linear model symptoms duration ~ age
RegModel <- lm(cad.dur~age, data=acath)
#summary model
summary(RegModel)

#correlation coefficient symptoms duration ~ age
cor(acath[,c("age","cad.dur")])