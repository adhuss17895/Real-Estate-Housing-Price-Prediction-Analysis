setwd("C:/Users/ramva/OneDrive/Desktop/Adithya/Data Science/R_Prog/Project1-RealEstate")

library(dplyr)
library(tidyr)
train = read.csv("housing_train.csv")
test = read.csv("housing_test.csv")

table(train$Suburb > 100)

#Adding Price column to make both tables equal
test$Price = 0

train$data = "train"
test$data = "test"

#Combining both data sets
re_all = rbind(train,test)

CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

re_all = re_all %>% fill(YearBuilt)

re_all = re_all %>% 
  select(-Postcode,-Address,-SellerG,)

lapply(re_all,function(x) sum(is.na(x)))  

re_all$Bathroom[is.na(re_all$Bathroom)]=round(mean(re_all$Bathroom,na.rm=T),0)
apply(re_all,2,function(x) sum(is.na(x)))

#Bedroom2
re_all$Bedroom2[is.na(re_all$Bedroom2)]=round(mean(re_all$Bedroom2,na.rm=T),0)
apply(re_all,2,function(x) sum(is.na(x)))

#Car
re_all$Car[is.na(re_all$Car)]=round(mean(re_all$Car,na.rm=T),0)
apply(re_all,2,function(x) sum(is.na(x)))

#LandSize
re_all$Landsize[is.na(re_all$Landsize)]=round(mean(re_all$Landsize,na.rm=T),0)
apply(re_all,2,function(x) sum(is.na(x)))

#BuildingArea
re_all$BuildingArea[is.na(re_all$BuildingArea)]=round(mean(re_all$BuildingArea,na.rm=T),0)
apply(re_all,2,function(x) sum(is.na(x)))


for_dummy_vars = c('Method','Type','Suburb','CouncilArea')
for(var in for_dummy_vars){
  re_all=CreateDummies(re_all,var,50)
}

train=re_all %>% filter(data=='train') %>% select(-data)
test=re_all %>% filter(data=='test') %>% select(-data,-Price)

set.seed(2)
s = sample(1*nrow(train),0.7*nrow(train))
train1 = train[s,]
train2 = train[-s,]

fit = lm(Price~. ,data=train)
summary(fit)

library(car)
vif(fit)
sort(vif(fit),decreasing = T)[1:3]

fit = lm(Price~.-CouncilArea_ ,data = train)
vif(fit)
sort(vif(fit),decreasing = T)[1:3]

fit = lm(Price~.-CouncilArea_ -Distance,data = train)
vif(fit)
sort(vif(fit),decreasing = T)[1:3]


train$Price = as.factor(train$Price)


library(randomForest)
  rf.model=randomForest(Price~.,data=train,ntree=50)
score = predict(rf.model,newdata = test,type="prob")[,2]

errors = train2$Price - score
rmse = errors**2 %>% mean() %>% sqrt()  #tentative perfomance of the model
rmse

write.csv(score,"Adhitya_Ramesh_P1_Part2_Initial.csv",row.names = F)
