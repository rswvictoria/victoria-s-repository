#R file
#This homework project is about monitoring price changes and profit maximization while considering competing products.

#question 1
#a)
file.choose()
soupData <- read.csv('/Users/victoria/Downloads/soupData.csv')

#b)
install.packages('nnet')
library('nnet')
model1 = lm(units~pricePerUnit*factor(flavID),data=soupData)
model2 = nnet(units~pricePerUnit*factor(flavID),data=soupData,rang=0,size=3,linout=TRUE)
model3 = lm(units~pricePerUnit*factor(flavID) + otherPrice,data=soupData)


#question 2
#a)
#model 1
objFunc1 = function(x){
  data1  = data.frame(flavID=1,pricePerUnit=x,otherPrice=0.69)
  data2  = data.frame(flavID=2,pricePerUnit=0.69,otherPrice=x)
  demand1 = predict(model1,data1)
  demand2 = predict(model1,data2)
  profit= demand1*(x-0.3)+demand2*(0.69-0.3)
  return(profit)
}

testX = seq(0,2,0.01)
objectiveValues = rep(NA,length(testX))
objectiveValues = sapply(testX,objFunc1)
testX[objectiveValues==max(objectiveValues)]
#In model 1,the optimal price for flavor 1 is 0.68.

#model 2
objFunc2 = function(x){
  data1  = data.frame(flavID=1,pricePerUnit=x,otherPrice=0.69)
  data2  = data.frame(flavID=2,pricePerUnit=0.69,otherPrice=x)
  demand1 = predict(model2,data1)
  demand2 = predict(model2,data2)
  profit= demand1*(x-0.3)+demand2*(0.69-0.3)
  return(profit)
}

testX = seq(0,2,0.01)
objectiveValues = rep(NA,length(testX))
objectiveValues = sapply(testX,objFunc2)
testX[objectiveValues==max(objectiveValues)]
#In model 2,the optimal price for flavor 1 is 0.49.

#model 3
objFunc3 = function(x){
  data1  = data.frame(flavID=1,pricePerUnit=x,otherPrice=0.69)
  data2  = data.frame(flavID=2,pricePerUnit=0.69,otherPrice=x)
  demand1 = predict(model3,data1)
  demand2 = predict(model3,data2)
  profit= demand1*(x-0.3)+demand2*(0.69-0.3)
  return(profit)
}

testX = seq(0,2,0.01)
objectiveValues = rep(NA,length(testX))
objectiveValues = sapply(testX,objFunc3)
testX[objectiveValues==max(objectiveValues)]
#In model 3, the optimal price for flavor 1 is 0.68.

#b)
#model 1
objFunc1 = function(x){
  data1  = data.frame(flavID=1,pricePerUnit=x,otherPrice=0.89)
  data2  = data.frame(flavID=2,pricePerUnit=0.89,otherPrice=x)
  demand1 = predict(model1,data1)
  demand2 = predict(model1,data2)
  profit= demand1*(x-0.3)+demand2*(0.89-0.3)
  return(profit)
}

testX = seq(0,2,0.01)
objectiveValues = rep(NA,length(testX))
objectiveValues = sapply(testX,objFunc1)
testX[objectiveValues==max(objectiveValues)]
#In model 1,the optimal price for flavor 1 is 0.68.

#model 2
objFunc2 = function(x){
  data1  = data.frame(flavID=1,pricePerUnit=x,otherPrice=0.89)
  data2  = data.frame(flavID=2,pricePerUnit=0.89,otherPrice=x)
  demand1 = predict(model2,data1)
  demand2 = predict(model2,data2)
  profit= demand1*(x-0.3)+demand2*(0.89-0.3)
  return(profit)
}

testX = seq(0,2,0.01)
objectiveValues = rep(NA,length(testX))
objectiveValues = sapply(testX,objFunc2)
testX[objectiveValues==max(objectiveValues)]
#In model 2,the optimal price for flavor 1 is 0.49.

#model 3
objFunc3 = function(x){
  data1  = data.frame(flavID=1,pricePerUnit=x,otherPrice=0.89)
  data2  = data.frame(flavID=2,pricePerUnit=0.89,otherPrice=x)
  demand1 = predict(model3,data1)
  demand2 = predict(model3,data2)
  profit= demand1*(x-0.3)+demand2*(0.89-0.3)
  return(profit)
}

testX = seq(0,2,0.01)
objectiveValues = rep(NA,length(testX))
objectiveValues = sapply(testX,objFunc3)
testX[objectiveValues==max(objectiveValues)]
#In model 3, the optimal price for flavor 1 is 0.76.

#c)
#two-dimensional grid search
Objfunc_full_grid = function(X,Y){
  data1=data.frame(flavID=1,pricePerUnit=X,otherPrice=Y)
  data2=data.frame(flavID=2,pricePerUnit=Y,otherPrice=X)
  demand1 = predict(model3,data1)
  demand2 = predict(model3,data2)
  profit= demand1*(X-0.3)+demand2*(Y-0.3)
  return(profit)
}
testX = seq(0,2,.01)
testY = seq(0,2,.01)
fullgrid = expand.grid(testX,testY)
objectiveValues = rep(NA,nrow(fullgrid))
for(i in 1:nrow(fullgrid)){
  objectiveValues[i] = Objfunc_full_grid(fullgrid[i,1],fullgrid[i,2])
}
fullgrid[objectiveValues==max(objectiveValues),]
#Using two-dimensional grid search, the optimal price for flavor 1 is 0.67, and for flavor 2 is 0.65.

#optim
Objfunc_optim = function(x){
  v1=x[1]
  v2=x[2]
  data1=data.frame(flavID=1,pricePerUnit=v1,otherPrice=v2)
  data2=data.frame(flavID=2,pricePerUnit=v2,otherPrice=v1)
  demand1 = predict(model3,data1)
  demand2 = predict(model3,data2)
  profit= -(demand1*(v1-0.3)+demand2*(v2-0.3))
  return(profit)
}
bestPar = optim(c(0,0),Objfunc_optim)$par
bestPar
#Using optim, the optimal price for flavor 1 is 0.6657, and for flavor 2 is 0.6508.

#d)
Objfunc_model = function(y){
  profit = function(x) {
  data1  = data.frame(flavID=1,pricePerUnit=x,otherPrice=0.69)
  data2  = data.frame(flavID=2,pricePerUnit=0.69,otherPrice=x)
  demand1 = predict(y,data1)
  demand2 = predict(y,data2)
  profit= demand1*(x-0.3)+demand2*(0.69-0.3)
  return(profit) 
  }
  testX = seq(0,2,0.01)
  objectiveValues = rep(NA,length(testX))
  objectiveValues = sapply(testX,profit)
  best_price = testX[objectiveValues==max(objectiveValues)]
  return(best_price)
}
#try model
Objfunc_model(model1)

#Question 3
#a)bootstrapping
#model 1
nSamples = nrow(soupData)
bsprice1 = rep(NA,1000)
for(i in 1:1000){
  bsSample = sample(nSamples,replace = TRUE)
  bsData = soupData[bsSample,]
  bsModel = lm(units~pricePerUnit*factor(flavID),data = bsData)
  bsprice1[i]= Objfunc_model(bsModel)
}

#model 2
nSamples = nrow(soupData)
bsprice2 = rep(NA,1000)
for(i in 1:1000){
  bsSample = sample(nSamples,replace = TRUE)
  bsData = soupData[bsSample,]
  bsModel = nnet(units~pricePerUnit*factor(flavID),data=bsData,rang=0,size=3,linout=TRUE)
  bsprice2[i]= Objfunc_model(bsModel)
}

#model 3
nSamples = nrow(soupData)
bsprice3 = rep(NA,1000)
for(i in 1:1000){
  bsSample = sample(nSamples,replace = TRUE)
  bsData = soupData[bsSample,]
  bsModel = lm(units~pricePerUnit*factor(flavID) + otherPrice,data=bsData)
  bsprice3[i]= Objfunc_model(bsModel)
}
sd(bsprice3)

#b)
sd(bsprice1)
#For model 1, the standard error of optimal prices is 0.02644502.
sd(bsprice2)
#For model 2, the standard error of optimal prices is 0.6609209.
sd(bsprice3)
#For model 3, the standard error of optimal prices is 0.02293608.

#c)
#model 1
nSamples = nrow(soupData)
bsprice11 = rep(NA,2000)
for(i in 1:2000){
  bsSample = sample(nSamples,replace = TRUE)
  bsData = soupData[bsSample,]
  bsModel = lm(units~pricePerUnit*factor(flavID),data = bsData)
  bsprice11[i]= Objfunc_model(bsModel)
}
sd(bsprice11)
#For model 1, the standard error of optimal prices is 0.02545653.

#model 2
nSamples = nrow(soupData)
bsprice22 = rep(NA,2000)
for(i in 1:2000){
  bsSample = sample(nSamples,replace = TRUE)
  bsData = soupData[bsSample,]
  bsModel = nnet(units~pricePerUnit*factor(flavID),data=bsData,rang=0,size=3,linout=TRUE)
  bsprice22[i]= Objfunc_model(bsModel)
}
sd(bsprice22)
#For model 2, the standard error of optimal prices is 0.6750104.

#model 3
nSamples = nrow(soupData)
bsprice33 = rep(NA,2000)
for(i in 1:2000){
  bsSample = sample(nSamples,replace = TRUE)
  bsData = soupData[bsSample,]
  bsModel = lm(units~pricePerUnit*factor(flavID) + otherPrice,data=bsData)
  bsprice33[i]= Objfunc_model(bsModel)
}
sd(bsprice33)
#For model 3, the standard error of optimal prices is 0.02213966.

#d)
hist(bsprice1,main="Bootstrap Distribution Of Optimal Price",xlab="Optimal Price")
hist(bsprice2,main="Bootstrap Distribution Of Optimal Price",xlab="Optimal Price")
hist(bsprice3,main="Bootstrap Distribution Of Optimal Price",xlab="Optimal Price")

#Question 4
nSamples = nrow(soupData)
bsprice4 = rep(NA,1000)
for(i in 1:1000){
  bsSample =  sample(1:nSamples,4500,replace = TRUE)
  bsData =  soupData[bsSample,]
  bsModel = lm(units~pricePerUnit*(factor(flavID)) + otherPrice,data=bsData)
  bsprice4[i] = Objfunc_model(bsModel)
}
sd(bsprice4)
#With sample number=4500, the standard error is 0.008581914.

#Question 6
#a)
model4 = lm(units~pricePerUnit*factor(flavID)+ pricePerUnit*poly(weekInYearNum,4),data=soupData)
summary(model4)

#b)
data1  = data.frame(flavID=1,pricePerUnit=0.69,otherPrice=0.69,weekInYearNum=1)
data2  = data.frame(flavID=2,pricePerUnit=0.69,otherPrice=0.69,weekInYearNum=1)
demand1     = predict(model4,data1)
demand2     = predict(model4,data2)

#c)
Objfunc_optim2 = function(x){
  data1 = data.frame(flavID=1,pricePerUnit=x,otherPrice=0.69,weekInYearNum=1)
  data2 = data.frame(flavID=2,pricePerUnit=0.69,otherPrice=x,weekInYearNum=1)
  demand1 = predict(model4,data1)
  demand2 = predict(model4,data2)
  profit= -(demand1*(x-0.3)+demand2*(0.69-0.3))
  return(profit)
}
bestPar = optim(0,Objfunc_optim2)$par
bestPar
#The optimal price for flavID = 1 when weekInYearNum=1, assuming the price of flavID=2 is 0.69 and marginal cost = .3.
#is 0.6294434.

#d)
plotprices = data.frame(weeknumber = unique(soupData$weekInYearNum),Price = rep(NA,length(unique(soupData$weekInYearNum))))
Objfunc_week = function(x,y){
  data1 = data.frame(flavID=1,pricePerUnit=x,otherPrice=0.69,weekInYearNum=y)
  data2 = data.frame(flavID=2,pricePerUnit=0.69,otherPrice=x,weekInYearNum=y)
  demand1 = predict(model4,data1)
  demand2 = predict(model4,data2)
  profit= -(demand1*(x-0.3)+demand2*(0.69-0.3))
  return(profit)
}

for(i in 1:52){plotprices$Price[i] = optim(0,Objfunc_week,y=i,method='BFGS')$par}
plot(plotprices$Weeknumber,plotprices$Price)



