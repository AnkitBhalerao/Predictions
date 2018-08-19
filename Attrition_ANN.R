##---------------------------------------------------------------
# Artificial Neural Networks (ANN)
##---------------------------------------------------------------

library(neuralnet)
library(dplyr)
##--------Multilayer nural network
##--------THREE CATEGORY ANN, Two hidden layer (3,2)-------------

empdata<-read.csv("C:\\Users\\Documents\\Attrition\\Attrition Case Study.csv")
head(empdata)

set.seed(101)
empdata1=data.frame(empdata)
head(empdata1)

glimpse(empdata1)
for(i in 1:35)
{
  if(class(empdata1[,i])=="character")
  {
    empdata1[,i]=as.factor(empdata1[,i])
  }
}

dim(empdata1)
sample.size <- 999

# get a training sample from empdata of sample.size
empdatatrain <- empdata1[sample(1:nrow(empdata1), sample.size),] 
nnet_empdatatrain <- empdatatrain

# Convert categorical output to three binary variales
nnet_empdatatrain <- cbind(nnet_empdatatrain, empdatatrain$Attrition == 0)
nnet_empdatatrain <- cbind(nnet_empdatatrain, empdatatrain$Attrition == 1)

head(nnet_empdatatrain)
names(nnet_empdatatrain)[4] <- 0
names(nnet_empdatatrain)[5] <- 1
head(nnet_empdatatrain)

# Run neuralnet function to train the neural networks using backpropagation
# hidden = vetor of integers specifying the number of hidden neurons in each layer

nn <- neuralnet(Attrition ~ MonthlyIncome+JobSatisfaction+PercentSalaryHike+WorkLifeBalance,
                data=nnet_empdatatrain, hidden=c(4,3), 
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

nn <- neuralnet(Attrition ~ MonthlyIncome+JobSatisfaction+PercentSalaryHike+WorkLifeBalance,
                data=nnet_empdatatrain, hidden=c(4,3),rep = 5, 
                algorithm = 'backprop', learningrate = 0.05,
                threshold = 0.01, linear.output = F)
plot(nn)

nn$act.fct # activation function
nn$err.fct # error function
nn$net.result # a list containing the overall result for every rep
nn$weights # fitted weights
nn$result.matrix # matrix of results (reached threshold, needed steps, weights)
nn$startweights # list of startweights

#----Predict--------------------------------------------
# compute function computes the output of all neurons for given x 
# for a trained neural network, uses object of class nn and rep no
# outputs for each neuron and a matrix containing overall results
# rep indicates which repetition to use

empdata <- model.matrix(~ Attrition+MonthlyIncome+JobSatisfaction+PercentSalaryHike
                        +WorkLifeBalance,data=nnet_empdatatrain)
head(empdata)
neuralnet::compute(nn, empdata[,2:5], rep=1)
mypredict <- neuralnet::compute(nn, empdata[,2:5],rep=1)$net.result

# Put multiple binary output to categorical output
# function to get the max id from array for each column
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(mypredict, 1, maxidx)
prediction <- c('0', '1')[idx]
table(pred=prediction, true=empdata$Attrition)
