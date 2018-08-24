##---------------------------------------------------------------
# Artificial Neural Networks (ANN)
##---------------------------------------------------------------
library(neuralnet)

empdata<-read.csv("C:\\Users\\Documents\\Attrition Case Study.csv")
head(empdata)

set.seed(2)
empdata1=data.frame(empdata)

head(empdata1)
names(empdata1)
dim(empdata1)
str(empdata1)

sample.size <- 500
sample.size1 <- 970

# get a training sample from empdata of sample.size
empdatatrain <- empdata1[sample(1:nrow(empdata1), sample.size),] 
nnet_empdatatrain <- empdatatrain

empdatatest <- empdata1[sample(1:nrow(empdata1), sample.size1),] 
nnet_empdatatest <- empdatatest

# Convert categorical output to two binary variales

View(head(nnet_empdatatrain))
View(head(nnet_empdatatest))

# Run neuralnet function to train the neural networks using backpropagation
# hidden = vetor of integers specifying the number of hidden neurons in each layer

allDataTrain <- model.matrix(~Attrition+Age                     
                        +BusinessTravel          
                        +DailyRate               
                        +Department              
                        +DistanceFromHome        
                        +Education               
                        +EducationField          
                        +EmployeeCount           
                        +EmployeeNumber          
                        +EnvironmentSatisfaction 
                        +HourlyRate              
                        +JobInvolvement          
                        +JobLevel                
                        +JobRole                 
                        +JobSatisfaction         
                        +MaritalStatus           
                        +MonthlyIncome           
                        +MonthlyRate             
                        +NumCompaniesWorked      
                        +PercentSalaryHike       
                        +PerformanceRating       
                        +RelationshipSatisfaction
                        +StandardHours           
                        +StockOptionLevel        
                        +TotalWorkingYears       
                        +TrainingTimesLastYear   
                        +WorkLifeBalance         
                        +YearsAtCompany          
                        +YearsInCurrentRole      
                        +YearsSinceLastPromotion 
                        +YearsWithCurrManager,
                        data=nnet_empdatatrain)
head(allDataTrain)

nn <- neuralnet(Attrition~ Age                     
                 +BusinessTravelTravel_Frequently
                 +BusinessTravelTravel_Rarely
                 +DailyRate               
                 +DepartmentSales
                 +DistanceFromHome        
                 +Education    
                 +EducationFieldMarketing
                 +EducationFieldMedical
                 +EducationFieldOther
                 +EmployeeCount           
                 +EmployeeNumber          
                 +EnvironmentSatisfaction 
                 +HourlyRate              
                 +JobInvolvement          
                 +JobLevel                
                 +JobRoleManager
                 +JobSatisfaction         
                 +MaritalStatusMarried  
                 +MaritalStatusSingle
                 +MonthlyIncome           
                 +MonthlyRate             
                 +NumCompaniesWorked      
                 +PercentSalaryHike       
                 +PerformanceRating       
                 +RelationshipSatisfaction
                 +StandardHours           
                 +StockOptionLevel        
                 +TotalWorkingYears       
                 +TrainingTimesLastYear   
                 +WorkLifeBalance         
                 +YearsAtCompany          
                 +YearsInCurrentRole      
                 +YearsSinceLastPromotion 
                 +YearsWithCurrManager,
                 data=allDataTrain, hidden=c(4,3), 
                 algorithm = 'backprop', learningrate = 0.15,
                 linear.output = F)
plot(nn)

#help(neuralnet)

# stepmax = the maximum steps for the training of neural network
# rep = No of repetitions for the neural network training
# as the algorithm tends to get stuck at local minima
# algorithm = "backprop"
# learning rate = rate at which algorithm learns
# threshold = threshold of the partial derivative of the error function
# linear.output = (T/F) Whether activation function is to be applied 

nn$act.fct # activation function
nn$err.fct # error function
nn$net.result # a list containing the overall result for every rep
nn$weights # fitted weights
nn$result.matrix # matrix of results (reached threshold, needed steps, weights)
nn$startweights # list of startweights

neuralnet::compute(nn, allDataTrain[,1:35], rep=1)
mypredict <- neuralnet::compute(nn, allDataTrain[,1:35],rep=1)$net.result

# Put multiple binary output to categorical output
# function to get the max id from array for each column
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(mypredict, 1, maxidx)
prediction <- c('0', '1')[idx]
table(pred=prediction, true=empdatatrain$Attrition)

####################  TEST Data  ##########################

allData <- model.matrix(~Attrition+Age                     
                        +BusinessTravel          
                        +DailyRate               
                        +Department              
                        +DistanceFromHome        
                        +Education               
                        +EducationField          
                        +EmployeeCount           
                        +EmployeeNumber          
                        +EnvironmentSatisfaction 
                        +HourlyRate              
                        +JobInvolvement          
                        +JobLevel                
                        +JobRole                 
                        +JobSatisfaction         
                        +MaritalStatus           
                        +MonthlyIncome           
                        +MonthlyRate             
                        +NumCompaniesWorked      
                        +PercentSalaryHike       
                        +PerformanceRating       
                        +RelationshipSatisfaction
                        +StandardHours           
                        +StockOptionLevel        
                        +TotalWorkingYears       
                        +TrainingTimesLastYear   
                        +WorkLifeBalance         
                        +YearsAtCompany          
                        +YearsInCurrentRole      
                        +YearsSinceLastPromotion 
                        +YearsWithCurrManager,
                        data=nnet_empdatatest)
head(allData)

nn1 <- neuralnet(Attrition~ Age                     
                 +BusinessTravelTravel_Frequently
                 +BusinessTravelTravel_Rarely
                 +DailyRate               
                 +DepartmentSales
                 +DistanceFromHome        
                 +Education    
                 +EducationFieldMarketing
                 +EducationFieldMedical
                 +EducationFieldOther
                 +EmployeeCount           
                 +EmployeeNumber          
                 +EnvironmentSatisfaction 
                 +HourlyRate              
                 +JobInvolvement          
                 +JobLevel                
                 +JobRoleManager
                 +JobSatisfaction         
                 +MaritalStatusMarried  
                 +MaritalStatusSingle
                 +MonthlyIncome           
                 +MonthlyRate             
                 +NumCompaniesWorked      
                 +PercentSalaryHike       
                 +PerformanceRating       
                 +RelationshipSatisfaction
                 +StandardHours           
                 +StockOptionLevel        
                 +TotalWorkingYears       
                 +TrainingTimesLastYear   
                 +WorkLifeBalance         
                 +YearsAtCompany          
                 +YearsInCurrentRole      
                 +YearsSinceLastPromotion 
                 +YearsWithCurrManager,
                data=allData, hidden=c(4,3), 
                algorithm = 'backprop', learningrate = 0.15,
                linear.output = F)
plot(nn1)

nn1$act.fct # activation function
nn1$err.fct # error function
nn1$net.result # a list containing the overall result for every rep
nn1$weights # fitted weights
nn1$result.matrix # matrix of results (reached threshold, needed steps, weights)
nn1$startweights # list of startweights

#----Predict--------------------------------------------
# compute function computes the output of all neurons for given x 
# for a trained neural network, uses object of class nn and rep no
# outputs for each neuron and a matrix containing overall results
# rep indicates which repetition to use

neuralnet::compute(nn1, allData[,1:35], rep=1)
mypredict <- neuralnet::compute(nn1, allData[,1:35],rep=1)$net.result
mypredict
# Put multiple binary output to categorical output
# function to get the max id from array for each column
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(mypredict, 1, maxidx)
prediction <- c('0', '1')[idx]
table(pred=prediction, true=empdatatest$Attrition)
