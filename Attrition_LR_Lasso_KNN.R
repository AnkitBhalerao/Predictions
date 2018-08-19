

#--------------------------

library(MASS)
library(ISLR)
library(data.table)
library(glmnet)

empdata<-read.csv("C:\\Users\\Documents\\Attrition\\Attrition Case Study.csv")
head(empdata)

#convert empdata to empdata1(Data Frame)
empdata1=data.frame(empdata)

#Remove rows with NA
empdata1=na.omit(empdata1)
str(empdata1)
x=model.matrix(MonthlyIncome~Age+JobSatisfaction+PercentSalaryHike+WorkLifeBalance+JobSatisfaction*PercentSalaryHike,
               data=empdata1)
head(x)

x=x[,-1] #Remove Intercept
head(x)

y=(empdata1$MonthlyIncome) # A vector of salary
head(y)

grid=10^seq(10,-2,length=100)
grid
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]

rbind(ridge.mod$lambda[c(1,30,40,50,60,70,80,90,100)],
      coef(ridge.mod)[2:6,c(1,30,40,50,60,70,80,90,100)])

paste(sqrt(sum(coef(ridge.mod)[-1,1]^2)),
      sqrt(sum(coef(ridge.mod)[-1,50]^2)),
      sqrt(sum(coef(ridge.mod)[-1,60]^2)),
      sqrt(sum(coef(ridge.mod)[-1,100]^2)))

predict(ridge.mod,s=50,type="coefficients")[1:20]
sqrt(sum(predict(ridge.mod,s=50,type="coefficients")[1:20])^2)

#----------split the sample in test and training data

set.seed(1) #random sample generation is set to start at 1 every time
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
y.test

#fit model on training set

ridge.mod=glmnet(x[train,],y[train],alpha=0, lambda=grid, thresh=1e-12)
#prediction using s=4
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
ridge.pred
mean((y.test-ridge.pred)^2)

#S=1e10
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((y.test-ridge.pred)^2)
ridge.pred=predict.glmnet(ridge.mod, s=0, newx=x[test,],
                          x=x[train,],y=y[train],exact=TRUE)

mean((y.test-ridge.pred)^2)

# Cross validation, nfolds = no of folds, default=10
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min  # Best value of Lambda
bestlam

# Predicting using best value of Lambda on test observation
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
ridge.pred
mean((y.test-ridge.pred)^2)

# Training using full data set and predicting coeff using best lambda
out=glmnet(x,y,alpha = 0)
predict(out,type="coefficient",s=bestlam)[2:6,]
# plotting Ridge regression coefficients
plot(out)


#---------------------- LASSO----------

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)

plot(lasso.mod)

set.seed(1)

cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)

bestlam=cv.out$lambda.min
bestlam

lasso.pred = predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coeff=predict(out,type="coefficients",s=bestlam)[2:6,]
lasso.coeff

lasso.coeff[lasso.coeff!=0]


#---------------CLassfication - Logistc Reg-------------

library(ISLR)
library(data.table)
library(boot)

MonthlyIncome1<-log(empdata1$MonthlyIncome)
empdata1=data.frame(empdata1,MonthlyIncome1)
mean(MonthlyIncome1)
HighMOnthSal=ifelse(empdata1$MonthlyIncome1<8.55,"No","Yes")

empdata1=data.frame(empdata1,HighMOnthSal)

head(empdata1)
str(empdata1)
dim(empdata1)

table(HighMOnthSal)

log.fit = glm(HighMOnthSal~Age+JobSatisfaction+PercentSalaryHike+WorkLifeBalance+
          JobSatisfaction*PercentSalaryHike,data=empdata1,family=binomial)

summary(log.fit)
coef(log.fit)
summary(log.fit)$coef

#----The model with lowest AIC is the best model----

log.probs = predict(log.fit, type="response")

log.pred = rep("No",1470)
log.pred[log.probs>0.5] = "Yes"
log.pred

#----------------------KNN--------------------------
library(class)

set.seed(1)
train=sample(1:nrow(empdata1),4*nrow(empdata1)/5)
test=(-train)

x.train<-cbind(empdata1$MonthlyIncome[train],empdata1$PercentSalaryHike[train],
               empdata1$WorkLifeBalance[train],empdata1$JobSatisfaction[train])
x.test<-cbind(empdata1$MonthlyIncome[test],empdata1$PercentSalaryHike[test],
              empdata1$WorkLifeBalance[test],empdata1$JobSatisfaction[test])
y.train.cat<-empdata1$HighMOnthSal[train]
y.test.cat<-empdata1$HighMOnthSal[test]
y.test.cat
y.train.reg<-empdata1$HighMOnthSal[train]
y.test.reg<-empdata1$HighMOnthSal[test]

#K=4

knn.pred.cat = knn(x.train, x.test, y.train.cat,k=4)
pred.tab<-table(knn.pred.cat, y.test.cat)
pred.tab
sum(pred.tab[c(1,4)])/sum(pred.tab[1:4])
1-sum(pred.tab[c(1,4)])/sum(pred.tab[1:4])
