
getwd()

setwd("C:/project/manufacturing")
#
mf_train=read.csv("product_train .csv",stringsAsFactors = F)
mf_test=read.csv("product_test.csv",stringsAsFactors = F)

library(dplyr)
glimpse(mf_train)
#1.----------#####

mf_test$went_on_backorder=NA


mf_train$data="train"

mf_test$data="test"

#2.-----------------------##

CreateDummies=function(data,var,freq_cutoff=0){
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
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

################

mf_all=rbind(mf_train,mf_test)

glimpse(mf_all)

table(mf_all$potential_issue)
table(mf_all$deck_risk)
table(mf_all$oe_constraint)
table(mf_all$ppap_risk)
table(mf_all$stop_auto_buy)
table(mf_all$rev_stop)
table(mf_all$went_on_backorder)


lapply(mf_all[,names(mf_all)], class) # for checking class

names(mf_all)

cat_col=c("potential_issue","deck_risk","oe_constraint","ppap_risk","stop_auto_buy","rev_stop","went_on_backorder")
class(cat_col)

mf_all$potential_issue=ifelse(mf_all$potential_issue=="Yes",1,0)  
  
for (cat in cat_col) {
  mf_all[,cat]=ifelse((mf_all[,cat]=="Yes"),1,0)
}


library(dplyr)
glimpse(mf_all)

#some col as more zeros so we eliminating the following col.

# potential_issue
# oe_constraint
# rev_stop


mf_all=mf_all %>% 
  select(-potential_issue,-oe_constraint,-rev_stop)

glimpse(mf_all)
prop.table(table(mf_all$pieces_past_due)) #### drop
#here zero is 99% so drop this col

names(mf_all)
table(mf_all$national_inv) #keep
table(mf_all$lead_time) #keep
table(mf_all$in_transit_qty)#keep
table(mf_all$forecast_3_month)#keep
table(mf_all$forecast_6_month) #keep
table(mf_all$forecast_9_month)#keep
table(mf_all$sales_1_month)#keep
table(mf_all$sales_3_month)#keep
table(mf_all$sales_6_month)#keep
table(mf_all$sales_9_month)#keep
table(mf_all$min_bank)#keep
prop.table(table(mf_all$pieces_past_due))#drop 
table(mf_all$perf_6_month_avg)#keep
table(mf_all$perf_12_month_avg)#keep
prop.table(table(mf_all$local_bo_qty))#drop
table(mf_all$deck_risk)#keep
table(mf_all$ppap_risk)#keep
table(mf_all$stop_auto_buy)#keep

mf_all=mf_all %>% 
  select(-pieces_past_due,-local_bo_qty)
glimpse(mf_all)
 
#--------------no NA"s

lapply(mf_all,function(x) sum(is.na(x)))
#---------------------
 
#data prep completed

#separate the train and test
mf_train=mf_all %>% filter(data=="train") %>% select(-data)
mf_test=mf_all %>% filter(data=="test") %>% select(-data)

#---modelling starts here---

set.seed(2)
s=sample(1:nrow(mf_train),0.8*nrow(mf_train))
mf_train1=mf_train[s,]
mf_train2=mf_train[-s,]


#-----------


library(car)
names(mf_train1)
for_vif=lm(went_on_backorder~.-sku,data=mf_train1)

sort(vif(for_vif),decreasing=T)[1:3]

#dropped one by one

for_vif=lm(went_on_backorder~.-sku -forecast_6_month -sales_6_month 
           -sales_9_month -forecast_9_month -sales_1_month,data=mf_train1)

sort(vif(for_vif),decreasing=T)[1:3]

#--------

log_fit=glm(went_on_backorder~.-sku -forecast_6_month -sales_6_month 
            -sales_9_month -forecast_9_month -sales_1_month,
            data=mf_train1,family = binomial("logit"),maxit=100)

log_fit=step(log_fit)

formula(log_fit)

#-----

log_fit=glm(went_on_backorder ~ national_inv + lead_time + in_transit_qty + 
              forecast_3_month + perf_6_month_avg + deck_risk
              ,data=mf_train1,family = binomial("logit"),maxit=100)
summary(log_fit)

#--------------------
#### performance of score model on validation data
library(pROC)

val.score=predict(log_fit,newdata = mf_train2,type='response')

auc(roc(mf_train2$went_on_backorder,val.score))

#------- auc is 66% lets move preparing for whole model


for_vif=lm(went_on_backorder~.-sku -forecast_6_month -sales_6_month
           -sales_9_month -forecast_9_month -sales_1_month 
           ,data=mf_train)

sort(vif(for_vif),decreasing = T)[1:3]

log.fit.final=glm(went_on_backorder~.-sku -forecast_6_month -sales_6_month
                  -sales_9_month -forecast_9_month -sales_1_month 
                  ,data=mf_train,family = binomial("logit"),maxit=100)

log.fit.final=step(log.fit.final)
# this will again take a long time to run

summary(log.fit.final)

formula(log.fit.final)

log.fit.final=glm(went_on_backorder ~ national_inv + lead_time + in_transit_qty + 
                    min_bank + perf_12_month_avg + deck_risk ,
                  data=mf_train,family = binomial("logit"),maxit=100)

summary(log.fit.final)

# now if we needed to submit probability scores for the test data we can do at this point

test.prob.score= predict(log_fit,newdata = mf_test,type='response')
write.csv(test.prob.score,"Surendran_R_P3_part2.csv",row.names = F)



# however if we need to submit hard classes, we'll need to determine cutoff score

train.score=predict(log.fit.final,newdata = mf_train,type='response')
real=mf_train$went_on_backorder
cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]


#### visualise how these measures move across cutoffs
library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=Sp))+geom_line()

library(tidyr)

cutoff_long=cutoff_data %>% 
  gather(Measure,Value,Sn:M)

ggplot(cutoff_long,aes(x=cutoff,y=Value,color=Measure))+geom_line()


my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

# now that we have our cutoff we can convert score to hard classes

test.predicted=as.numeric(test.prob.score>my_cutoff)
write.csv(test.predicted,"Surendran_R_P3_part2.csv",row.names = F)


###-------------completed ----- 

result=read.csv("Surendran_R_P3_part2.csv",stringsAsFactors = F)

View(result)

colnames(result)="went_on_backorder"

result$went_on_backorder=ifelse(result$went_on_backorder== 1,"Yes","No")

View(result)

#-----final output based on request-------

write.csv(result,"Surendran_R_P3_part2.csv",row.names = F)
