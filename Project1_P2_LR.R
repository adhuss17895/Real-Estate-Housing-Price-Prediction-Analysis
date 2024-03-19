#Project Real Estate
#Given the train and test date we need to predict values for the given response variable Price
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

#Checking for NA in the dataset
lapply(re_all,function(x) sum(is.na(x)))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       lapply(re_all,function(x) sum(is.na(x)))


#Combing categories based on mean
#Bathroom
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

glimpse(re_all)

#Filling up NA values for YearBuilt as mean aggregate cannot be found with year
re_all = re_all %>% fill(YearBuilt)

#Address can be eliminated as every address is unique

re_all = re_all %>% 
  select(-Address)
glimpse(re_all)

#Function to create Dummies
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var]) #Display table format of categoical variable
  t=t[t>freq_cutoff]  #Assign a frequncy cut-off based on the no of observations and eliminate others
  t=sort(t) # sort the remainig data in ascending order 
  categories=names(t)[-1] #Create dummy variables for n-1 of remaining observations
  
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    #Cleaning of unwanted things in categorical names
    
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

#sort(table(re_all$Suburb),decreasing=TRUE)

for(cat in c("Suburb","Method","SellerG","CouncilArea")){
    re_all = CreateDummies(re_all,cat,150)
  }
glimpse(re_all)

re_all = re_all %>% 
  mutate(
    Type_h = as.numeric(Type=='h'),
    Type_u = as.numeric(Type=='u')
  ) %>% 
  select(-Type)

glimpse(re_all)

train = re_all %>% 
  filter(data=='train') %>% 
  select(-data) 

test = re_all %>% 
  filter(data=='test') %>% 
  select(-data,-Price)

set.seed(2)
s = sample(1*nrow(train),0.7*nrow(train))
train1 = train[s,]
train2 = train[-s,]

#Working on ld_train 1
# Step 1: Check all are numeric values

glimpse(train1)

# 2: Using lm function do a linear model of the data excluding ID

fit = lm(Price~. ,data=train1)
summary(fit)

# 3: Find if Variance Inflation factor is high or variant for all variables 
# Install library car

library(car)
vif(fit)
sort(vif(fit),decreasing = T)[1:3]


#4: Finding p-values with cut off of 0.05
summary(fit)
fit = step(fit)
summary(fit)

formula(fit)

fit = lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
           Landsize + BuildingArea + YearBuilt + Suburb_Essendon + Suburb_SouthYarra + 
           Suburb_Preston + Suburb_Richmond + Suburb_Reservoir + 
           Method_SP + Method_S + SellerG_Miles  + SellerG_RT + 
           SellerG_Fletchers + SellerG_Biggin +  SellerG_Marshall + 
           SellerG_hockingstuart + SellerG_Jellis + 
           CouncilArea_HobsonsBay + CouncilArea_Bayside + CouncilArea_Banyule + 
           CouncilArea_PortPhillip + CouncilArea_Stonnington + 
           CouncilArea_Darebin + CouncilArea_Moreland + CouncilArea_Boroondara + 
           Type_h + Type_u, data=train1)
summary(fit)

library(ggplot2)
train1 %>% 
  mutate(pred_Price = predict(fit,newdata=train1)) %>% 
  ggplot(aes(x=Price,y=pred_Price))+geom_point(alpha=0.6)

plot(fit,which = 1)
plot(fit,which = 2)
plot(fit,which = 3)
plot(fit,which = 4)  #Cook's Distance

#Removing outliers using Cooks Distance
cooks_fitted = cooks.distance(fit)
influencing_values = cooks_fitted[(cooks_fitted > (3 * mean(cooks_fitted, na.rm = TRUE)))]
influencing_values

val.pred = predict(fit,newdata = train2)
errors = train2$Price - val.pred
rmse = errors**2 %>% mean() %>% sqrt()  #tentative perfomance of the model
rmse

#rmse = 413382.1

#Final Model

fit.final = lm(Price~. ,data=train)
vif(fit.final)
sort(vif(fit.final),decreasing=T)[1:3]

step(fit.final)
summary(fit.final)
formula(fit.final)

fit.final = lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
                 Landsize + BuildingArea + YearBuilt + Suburb_Essendon +
                 Suburb_SouthYarra + Suburb_StKilda + Suburb_Preston + Suburb_Richmond + 
                 Suburb_Reservoir +
                 Method_S + SellerG_Miles + 
                 SellerG_RT + 
                 SellerG_Biggin + SellerG_Ray +  SellerG_Marshall + 
                 SellerG_hockingstuart + SellerG_Jellis + 
                 CouncilArea_HobsonsBay + 
                 CouncilArea_Bayside + CouncilArea_Banyule + 
                 CouncilArea_PortPhillip +
                 CouncilArea_Stonnington + CouncilArea_Darebin + 
                 CouncilArea_Moreland + CouncilArea_Boroondara + 
                 CouncilArea_ + Type_h + Type_u, data = train)
summary(fit.final)


pred.Price = predict(fit.final, newdata = test)
write.csv(pred.Price,"Adhitya_Ramesh_P1_Part2.csv",row.names = F)