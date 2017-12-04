#Marketing project 2
#Shiwei Ran

#Part A
#Q2
demo = read.csv("/Users/victoria/Downloads/HW2 - Demo Data.csv")
purchase = read.csv("/Users/victoria/Downloads/HW2 - Purchase Dataframe.csv")

purchase$isPurchase [purchase$isPurchase == 'TRUE'] = 1
purchase$isPurchase [purchase$isPurchase == 'False'] = 0
purchase = purchase[,-1]
purchase = purchase[c(3,4,1,2,5)]

linear = lm(isPurchase~price,data=purchase)
summary(linear)

logit = glm(isPurchase~price,data=purchase,family=binomial(link=logit))
summary(logit)

probit = glm(isPurchase~price,data=purchase,family=binomial(link=probit))
summary(probit)

pricegrid = seq(-5,5,0.01)

linearpredict = rep(NA,length(pricegrid))
for(i in 1:length(pricegrid)){
  linearpredict[i] = predict(linear,data.frame(price = pricegrid[i]),type="response")
}

logitpredict = rep(NA,length(pricegrid))
for(i in 1:length(pricegrid)){
  logitpredict[i] = predict(logit,data.frame(price = pricegrid[i]),type="response")
}

probitpredict = rep(NA,length(pricegrid))
for(i in 1:length(pricegrid)){
  probitpredict[i] = predict(probit,data.frame(price = pricegrid[i]),type="response")
}

merge = data.frame(pricegrid,linearpredict,logitpredict,probitpredict)

require(reshape2)
alter_merge = melt(merge,id="pricegrid")

library(ggplot2)
ggplot(data = alter_merge, aes(x=pricegrid, y=value)) + geom_line(aes(colour=variable))

#Q4
pricegrid1=c(1,1.1,3,3.1,5,5.1)
linearpredict1 = predict(linear,data.frame(price=pricegrid1),type="response")
logitpredict1 = predict(logit,data.frame(price=pricegrid1),type="response")
probitpredict1 = predict(probit,data.frame(price=pricegrid1),type="response")

merge1 = data.frame(pricegrid1,linearpredict1,logitpredict1,probitpredict1)

#Q5
objfunc_linear = function(price){
  profit = -1*(predict(linear,data.frame(price),type="response")*(price-1))
  return(profit)	
}
optim(0,objfunc_linear,method="BFGS")

objfunc_logit = function(price){
  profit = -1*(predict(logit,data.frame(price),type="response")*(price-1))
  return(profit)	
}
optim(0,objfunc_logit,method="BFGS")

objfunc_probit = function(price){
  profit = -1*(predict(probit,data.frame(price),type="response")*(price-1))
  return(profit)	
}
optim(0,objfunc_probit,method="BFGS")

#Q6
alter_objfunc_logit = function(price){
  profit = -100*(predict(logit,data.frame(price),type="response")*(price-1))
  return(profit)	
}
optim(5,alter_objfunc_logit,method="BFGS")

#Part B
#Q1
install.packages('plm')
require('plm')
withinmodel = plm(demand~price,data=purchase,model="within")
randommodel = plm(demand~price,data=purchase,model="random")
differmodel = plm(demand~price,data=purchase,model="fd")
poolingmodel = plm(demand~price,data=purchase,model="pooling")

#Q2
altwithinmodel = lm(demand~price+factor(panelID-1),data=purchase)
altpoolingmodel = lm(demand~price,data=purchase)

AIC(altwithinmodel)
AIC(altpoolingmodel)

#Q3
purchase_subset = purchase[purchase$demand >0, ]
subset_withinmodel = plm(demand~price,data=purchase_subset,model="within")

#Q5
total_sales=aggregate(demand ~ panelID, purchase, FUN=sum)
cor(altwithinmodel$coefficients[-1],total_sales$demand)

#Part C
#Q1
merge2 = merge(purchase,demo,by='panelID', all=TRUE)
hetero_model1=lm(demand~factor(incomeCategory-1)*price, data = merge2)
hetero_model2=lm(demand~factor(homeType-1)*price, data = merge2)
hetero_model3=lm(demand~factor(familySize-1)*price, data = merge2)

#Q2
median = median(total_sales$demand)
below=purchase[total_sales$demand < median, ]
above=purchase[total_sales$demand > median, ]
lmbelow=lm(demand~price,data=below)
lmabove=lm(demand~price,data=above)

#Q3
hetero_model4=lm(demand~(panelID-1)*price, data = merge2)
objfunc = function(price){
  panel1   = data.frame(price = price, panelID = 1)
  profit = -1*(predict(hetero_model4,panel1,type="response")*(price-1))
  return(profit)	
}
optim(0,objfunc_probit,method="BFGS")






