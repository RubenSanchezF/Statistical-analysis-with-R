#Author: Rubén Sánchez Fernández

#This is raw code, if you want it commented download the .Rmd version




library(Hmisc)
getHdata(acath)

dim(acath)

names(acath)

summary(acath)

str(acath)

hist(acath$cad.dur, col="darkseagreen1", main="Distribution of heart symptoms \n in the coronary artery disease", xlab="Time", ylab="Frequency")

print(paste0("The older patient is ", max(acath$age), " years old"))

print(paste0("and the youngest patient is ", min(acath$age), " years old"))

nrow(subset(acath, acath$choleste>200))

est<-subset(acath,acath$sigdz==1) #patients with the disease
p<-nrow(est)/nrow(acath) #probability = patients with the disease/total patients

p60<-1-pbinom(60,100,p,lower.tail=T) #dist binomial
print(paste0("The probability to find more than 60 patients with the disease after evaluating 100 tests is ", p60))

sya<-subset(acath,acath$tvdlm==0 & acath$sigdz==1)
psya<-nrow(sya)/nrow(acath) #probability no disease and narrow artery
ps<-psya/p #conditional probability
print(paste0("Probability of not having the disease while having narrow coronary artery is ",ps))


set.seed(12345)
t<-rbinom(10000,100,p)
str(t)
#plots
par(mfrow=c(1,2))
hist(t,col="lightblue")
plot(density(t))
mean(t)
sd(t)

pairs(acath)

RegModel <- lm(cad.dur~age, data=acath)
summary(RegModel)

cor(acath[,c("age","cad.dur")])