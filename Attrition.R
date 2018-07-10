library(data.table)
#C:\Users\akbhalerao\Documents\Attrition CSTD

empdata<-read.csv("C:\\Users\\akbhalerao\\Documents\\Attrition CSTD\\Attrition Case Study.csv")
head(empdata)

#convert empdata to empdata1(Data Frame)

empdata1=data.frame(empdata)

#explore MonthlyIncome column
mean(empdata1$MonthlyIncome, na.rm = TRUE)
sd(empdata1$MonthlyIncome, na.rm = TRUE)

#Remove rows with NA
table(is.na(empdata1$MonthlyIncome))
empdata1=na.omit(empdata1)
hist(empdata1$MonthlyIncome)
#---------------------------------------------
#-----ANOVA

head(empdata1)

str(empdata1)

table(empdata1$JobSatisfaction)
plot(density(empdata1$MonthlyIncome))

boxplot(empdata1$MonthlyIncome~empdata1$JobSatisfaction)

empdata1.aov<-aov(MonthlyIncome~JobSatisfaction,data=empdata1)
summary(empdata1.aov)

#----------Q->Q-------------
#Histogram
hist(empdata1$MonthlyIncome)

#scatter plot q->q
plot(empdata1$Age,empdata1$MonthlyIncome)
#correlation coeficient
cor(empdata1$MonthlyIncome,empdata1$Age)
#Linear regression line t test for slope
empdata1lm1<-lm(MonthlyIncome~Age,data=empdata1)
summary(empdata1lm1)
abline(empdata1lm1)
#stratfied scatter plot
plot(empdata1$Age,empdata1$MonthlyIncome, col=(empdata1$JobSatisfaction),pch=35)
#matrix plot
pairs(empdata1[,c(1,2,3,4,6)])
#--------------p value and confidence interval

empdata1lm1$coefficients
confint(empdata1lm1)
empdata1lm1$fitted.values
empdata1lm1$residuals

RSS1 = sum (empdata1lm1$residuals^2)
RSS1

summary(empdata1lm1)$sigma #RSE

summary(empdata1lm1)$r.squared

summary(empdata1lm1)$adj.r.squared

summary(empdata1lm1)

plot(empdata1lm1,1)

TSS = sum((empdata1$MonthlyIncome-mean(empdata1$MonthlyIncome))^2)
TSS

R2<-((TSS-RSS1)/TSS) #or *100 for percentage
R2

#Multiple Linear Regression

#----Qualitative Predictions-----

empdata1lm2<-lm(MonthlyIncome~Age,data=empdata1)
RSS3=sum(empdata1lm2$residuals^2)
RSS3

summary(empdata1lm2)
plot(empdata1lm2,which=1)
empdata1lm2$fitted.values

empdata1lm3<-lm(MonthlyIncome~Age+JobSatisfaction,data=empdata1)
RSS2=sum(empdata1lm3$residuals^2)
RSS2

summary(empdata1lm3)
plot(empdata1lm3,1)

#--------Quantitative+Categorical Predictors

empdata1lm4<-lm(MonthlyIncome~Age+JobSatisfaction+PercentSalaryHike,data=empdata1)
RSS2=sum(empdata1lm4$residuals^2)
RSS2

summary(empdata1lm4)
plot(empdata1lm4,1)

#--------------------------Interaction effect------------

empdata1lm5<-lm(MonthlyIncome~Age+JobSatisfaction+PercentSalaryHike+WorkLifeBalance+JobSatisfaction*PercentSalaryHike,data=empdata1)
RSS2=sum(empdata1lm5$residuals^2)
RSS2

summary(empdata1lm5)
plot(empdata1lm5,1)

#-------------------Polynomial Regression----

empdata1lm6<-lm(MonthlyIncome~PerformanceRating,data=empdata1)
empdata1lm7<-lm(MonthlyIncome~Age+I(JobSatisfaction^2),data=empdata1)
empdata1lm8<-lm(MonthlyIncome~poly(JobSatisfaction,3),data=empdata1)

plot(empdata1$JobSatisfaction,empdata1$MonthlyIncome)
abline(empdata1lm6)

lines(sort(empdata1$JobSatisfaction), fitted(empdata1lm7)[order(empdata1$JobSatisfaction)])
lines(sort(empdata1$JobSatisfaction), fitted(empdata1lm8)[order(empdata1$JobSatisfaction)])

RSS6=sum(empdata1lm6$residuals^2)
RSS7=sum(empdata1lm7$residuals^2)
RSS8=sum(empdata1lm8$residuals^2)

paste("RSS6=",round(RSS6,2),",RSS7=",round(RSS7,2),",RSS8=",round(RSS8,2))

summary(empdata1lm6)
summary(empdata1lm7)
summary(empdata1lm8)

plot(empdata1lm6,1)
plot(empdata1lm7,1)
plot(empdata1lm8,1)

empdata1lm9<-lm(MonthlyIncome~Age+JobSatisfaction+PercentSalaryHike+WorkLifeBalance+Age*PercentSalaryHike+(JobSatisfaction^2),data=empdata1)
RSS9=sum(empdata1lm9$residuals^2)
RSS9
summary(empdata1lm9)
plot(empdata1lm9,1) 

Reg_result=data.frame(Model_no = c(1,2,3,4,5,6,7,8,9),
                      model = c("MonthlyIncome","MonthlyIncome+Age","JobSatisfaction","MonthlyIncome+Age+JobSatisfaction","MonthlyIncome+Age+JobSatisfaction+MonthlyIncome*Age",
                                "Age","Age^2","poly(Age,3)","MonthlyIncome+Age+JobSatisfaction+MonthlyIncome*Age+I(Age^2)"),
                      RSS=c(RSS1,RSS2,RSS3,RSS4,RSS5,RSS6,RSS7,RSS8,RSS9),
                      RSE=c(summary(empdata1lm1)$sigma,summary(empdata1lm2)$sigma,summary(empdata1lm3)$sigma,
                            summary(empdata1lm4)$sigma,summary(empdata1lm5)$sigma,summary(empdata1lm6)$sigma,
                            summary(empdata1lm7)$sigma,summary(empdata1lm8)$sigma,summary(empdata1lm9)$sigma),
                      R.sq=c(summary(empdata1lm1)$r.squared,summary(empdata1lm2)$r.squared,summary(empdata1lm3)$r.squared,
                             summary(empdata1lm4)$r.squared,summary(empdata1lm5)$r.squared,summary(empdata1lm6)$r.squared,
                             summary(empdata1lm7)$r.squared,summary(empdata1lm8)$r.squared,summary(empdata1lm9)$r.squared),
                      R.sq.adj=c(summary(empdata1lm1)$adj.r.squared,summary(empdata1lm2)$adj.r.squared,summary(empdata1lm3)$adj.r.squared,
                                 summary(empdata1lm4)$adj.r.squared,summary(empdata1lm5)$adj.r.squared,summary(empdata1lm6)$adj.r.squared,
                                 summary(empdata1lm7)$adj.r.squared,summary(empdata1lm8)$adj.r.squared,summary(empdata1lm9)$adj.r.squared)
)

Reg_result

anova(empdata1lm2,empdata1lm3,empdata1lm4,empdata1lm5,empdata1lm6,empdata1lm7,empdata1lm9)

#pred = predict(empdata1lm7,newdata=data.frame(Age=c(25:30,30:40,40:50,50:60),MonthlyIncome=c(500:2500,2500:10000,10000:20000,20000:50000),JobSatisfaction=c(1,2,3,4)))
pred=predict(empdata1lm7)
pred
