#part 1

#a)load the dataset
consumerdata = read.csv(file.choose())
itemdata = read.csv(file.choose())

#b)change the name of each column in each dataset
#change names in consumer data
colnames(consumerdata)[1]<- "consumer_id"
colnames(consumerdata)[4]<- "week_number"
colnames(consumerdata)[5]<- "store_identifier"
colnames(consumerdata)[6]<- "panel_number"
colnames(consumerdata)[7]<- "flavor_id"

#change names in item data
colnames(itemdata)[1]<- "index"
colnames(itemdata)[2]<- "volume_per_unit"
colnames(itemdata)[3]<- "chicken"
colnames(itemdata)[4]<- "private_label"
colnames(itemdata)[6]<- "tomato"
colnames(itemdata)[7]<- "flavor_id"

#c)merge consumerdata and itemdata
data <- merge(consumerdata,itemdata,by="flavor_id")

#d)create two new variables
#create a new variable that contains the price per unit purchased
data[14]<-(data[4]/data[3])
colnames(data)[14]<- "price_per_unit_purchased"

#create a new variable that contains the total volume of soup purchased
data[15]<-(data[3]*data[9])
colnames(data)[15]<- "total_volume_purchased"

#e)Present some relevant descriptive statistics of the data
#market shares for each store
store_marketshare<-aggregate(units ~ store_identifier, data = data, sum)
store_marketshare[3]<-store_marketshare[2]/sum(store_marketshare["units"])
colnames(store_marketshare)[3]<-"store_marketshare"
store_marketshare[3]<-round(store_marketshare[3],digits=5)

#market shares for each of the top ten flavors
flavor_marketshare<-aggregate(units ~ flavor, data = data, sum)
flavor_marketshare<-flavor_marketshare[order(-flavor_marketshare$units),]
flavor_marketshare<-flavor_marketshare[1:10,]
flavor_marketshare[3]<-flavor_marketshare[2]/sum(flavor_marketshare["units"])
colnames(flavor_marketshare)[3]<-"flavor_marketshare"
flavor_marketshare[3]<-round(flavor_marketshare[3],digits=5)

#market shares for private label
private_label_marketshare<-aggregate(units ~ private_label, data = data, sum)
private_label_marketshare[3]<-private_label_marketshare[2]/sum(private_label_marketshare["units"])
colnames(private_label_marketshare)[3]<-"private_label_marketshare"
private_label_marketshare[3]<-round(private_label_marketshare[3],digits=5)

#means and standard deviations
sapply(data, function(x) list(means=mean(x,na.rm=TRUE), sds=sd(x,na.rm=TRUE)))


#part 2
#(a)


#(b)
for (i in 1:length(data)){
  result<-lm(data$units[i]~data$price_per_unit_purchased[i])
}
result

