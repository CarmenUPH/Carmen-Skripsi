rm(list =ls())     
library(tidyverse)
library(pracma)
library(plyr)
library(caret)
library(actuar)
library(truncnorm)
library(MASS)
library(CASdatasets)
library(copula)

set.seed(100)

#Data 
data(fremotor1freq0304a)
data(fremotor1sev0304a)
data(fremotor1prem0304a)

fremotor1freq_a=data.frame(fremotor1freq0304a)
fremotor1sev_a=data.frame(fremotor1sev0304a)
fremotor1prem_a=data.frame(fremotor1prem0304a)

n=nrow(fremotor1sev_a)

Sev_Windscreen=NULL
Sev_TPL=NULL
Sev_Damage=NULL
Sev_Theft=NULL
Sev_Other=NULL
Sev_Fire=NULL

for(i in 1:n ){
  Sev_Windscreen[i]=ifelse(fremotor1sev_a$Guarantee[i]=="Windscreen",fremotor1sev_a$Payment[i],0)
  Sev_TPL[i]=ifelse(fremotor1sev_a$Guarantee[i]=="TPL",fremotor1sev_a$Payment[i],0)
  Sev_Damage[i]=ifelse(fremotor1sev_a$Guarantee[i]=="Damage",fremotor1sev_a$Payment[i],0)
  Sev_Theft[i]=ifelse(fremotor1sev_a$Guarantee[i]=="Theft",fremotor1sev_a$Payment[i],0)
  Sev_Other[i]=ifelse(fremotor1sev_a$Guarantee[i]=="Other",fremotor1sev_a$Payment[i],0)
  Sev_Fire[i]=ifelse(fremotor1sev_a$Guarantee[i]=="Fire",fremotor1sev_a$Payment[i],0)
}

fremotor1sev_a$Sev_Windscreen=Sev_Windscreen
fremotor1sev_a$Sev_TPL=Sev_TPL
fremotor1sev_a$Sev_Damage=Sev_Damage
fremotor1sev_a$Sev_Theft=Sev_Theft
fremotor1sev_a$Sev_Other=Sev_Other
fremotor1sev_a$Sev_Fire=Sev_Fire
Year=year(fremotor1sev_a$OccurDate)
fremotor1sev_a$Year=Year

by=c("IDpol","Year")
data1=merge(fremotor1freq_a,fremotor1prem_a,by=by)
data1=merge(data1,fremotor1sev_a,by=by)

# Merapihkan data 
data01=subset(data1,select = c("BonusMalus","PayFreq","VehClass","VehPower", "VehGas"
                                ,"VehAge","Garage","Area", "Region", "Channel","DrivGender","DrivAge","Marketing","Windscreen"
                                ,"TPL","Damage","Other","Theft", "Fire","Sev_Windscreen","Sev_TPL","Sev_Damage"
                                ,"Sev_Other","Sev_Theft","Sev_Fire"))

#Data NA
cekdata <- data.frame(Jumlah_NA = sapply(data01, function(x) sum(is.na(x))))
cekdata
#Duplikat 
is_duplicated <- duplicated(data01)
duplicated_rows <- data01[is_duplicated, ]
print(duplicated_rows)
data01 <- data01[!duplicated(data01), ]
nrow(data01)
#data dengan severity sama dengan 0 dan jumlah klaim tidak nol 
cekdata_1=NULL
severity=NULL
claim=NULL
for(i in 1:nrow(data01)){
  severity[i]=sum(data01$Sev_Damage[i],data01$Sev_Fire[i],data01$Sev_Other[i],data01$Sev_Theft[i],data01$Sev_TPL[i],data01$Sev_Windscreen[i])
  claim[i]=sum(data01$Damage[i],data01$Fire[i],data01$Other[i],data01$Theft[i],data01$TPL[i],data01$Windscreen[i])
  cekdata_1[i]=ifelse(severity[i]==0 & claim[i]>0,"Yes","No")
  if(cekdata_1[i]=="Yes"){
  }
}
sum(cekdata_1=="Yes")

#data dengan severity sama dengan !=0 dan jumlah klaim nol 

cekdata_2=NULL
severity2=NULL
claim2=NULL
for(i in 1:nrow(data01)){
  severity2[i]=sum(data01$Sev_Damage[i],data01$Sev_Fire[i],data01$Sev_Other[i],data01$Sev_Theft[i],data01$Sev_TPL[i],data01$Sev_Windscreen[i])
  claim2[i]=sum(data01$Damage[i],data01$Fire[i],data01$Other[i],data01$Theft[i],data01$TPL[i],data01$Windscreen[i])
  cekdata_2[i]=ifelse(severity2[i]>0 & claim2[i]==0,"Yes","No")
  if(cekdata_2[i]=="Yes"){
    print(i)
  }
}

sum(cekdata_2=="Yes")

#Data yang sudah bersih 
data01=cbind(data01,cekdata_1)
data01=cbind(data01,cekdata_2)
data01=data01[data01$cekdata_1=="No",]
data01=data01[data01$cekdata_2=="No",]
sum(data01$cekdata_1=="Yes")
sum(data01$cekdata_2=="Yes")
data01=subset(data01,select = c("BonusMalus","PayFreq","VehClass","VehPower", "VehGas"
                               ,"VehAge","Garage","Area", "Region", "Channel","DrivGender","DrivAge","Marketing","Windscreen"
                               ,"TPL","Damage","Other","Theft", "Fire","Sev_Windscreen","Sev_TPL","Sev_Damage"
                               ,"Sev_Other","Sev_Theft","Sev_Fire"))

#Bonus-Malus 
data01$BonusMalus=ifelse(data01$BonusMalus<100,"bonus","malus")
data01$BonusMalus=as.factor(data01$BonusMalus)


#Menggambungkan variabel dengan jumlah paling sedikit 
# vehPower
levels(data01$VehPower)[levels(data01$VehPower)=='P2']='P2-P9' 
levels(data01$VehPower)[levels(data01$VehPower)=='P4']='P2-P9' 
levels(data01$VehPower)[levels(data01$VehPower)=='P5']='P2-P9' 
levels(data01$VehPower)[levels(data01$VehPower)=='P6']='P2-P9' 
levels(data01$VehPower)[levels(data01$VehPower)=='P7']='P2-P9' 
levels(data01$VehPower)[levels(data01$VehPower)=='P8']='P2-P9'
levels(data01$VehPower)[levels(data01$VehPower)=='P9']='P2-P9'

levels(data01$VehPower)[levels(data01$VehPower)=='P10']='P10+P11+P12'
levels(data01$VehPower)[levels(data01$VehPower)=='P11']='P10+P11+P12'
levels(data01$VehPower)[levels(data01$VehPower)=='P12']='P10+P11+P12'

levels(data01$VehPower)[levels(data01$VehPower)=='P13']='P13-P17'
levels(data01$VehPower)[levels(data01$VehPower)=='P14']='P13-P17'
levels(data01$VehPower)[levels(data01$VehPower)=='P15']='P13-P17'
levels(data01$VehPower)[levels(data01$VehPower)=='P16']='P13-P17'
levels(data01$VehPower)[levels(data01$VehPower)=='P17']='P13-P17' 

#Area
levels(data01$Area)[levels(data01$Area)=='A2']='A2+A3+A4' 
levels(data01$Area)[levels(data01$Area)=='A3']='A2+A3+A4' 
levels(data01$Area)[levels(data01$Area)=='A4']='A2+A3+A4'

levels(data01$Area)[levels(data01$Area)=='A5']='A5+A6' 
levels(data01$Area)[levels(data01$Area)=='A6']='A5+A6' 

levels(data01$Area)[levels(data01$Area)=='A7']='A7-A10+A12' 
levels(data01$Area)[levels(data01$Area)=='A8']='A7-A10+A12' 
levels(data01$Area)[levels(data01$Area)=='A9']='A7-A10+A12' 
levels(data01$Area)[levels(data01$Area)=='A12']='A7-A10+A12' 
levels(data01$Area)[levels(data01$Area)=='A10']='A7-A10+A12' 


#PayFreq
levels(data01$PayFreq)[levels(data01$PayFreq)=='Half-yearly']='bi+monthly+quarterly'
levels(data01$PayFreq)[levels(data01$PayFreq)=='Monthly']='bi+monthly+quarterly'
levels(data01$PayFreq)[levels(data01$PayFreq)=='Quarterly']='bi+monthly+quarterly'

#VehClass 
levels(data01$VehClass)[levels(data01$VehClass)=='Cheap']='other'
levels(data01$VehClass)[levels(data01$VehClass)=='Expensive']='other'
levels(data01$VehClass)[levels(data01$VehClass)=='More expensive']='other'
levels(data01$VehClass)[levels(data01$VehClass)=='Most expensive']='other'
levels(data01$VehClass)[levels(data01$VehClass)=='Medium']='other'
levels(data01$VehClass)[levels(data01$VehClass)=='Medium high']='other'
levels(data01$VehClass)[levels(data01$VehClass)=='Medium low']='other'
levels(data01$VehClass)[levels(data01$VehClass)=='Most expensive']='other'


#Region 
levels(data01$Region)[levels(data01$Region)=='Paris area']='Paris and South West'
levels(data01$Region)[levels(data01$Region)=='South West']='Paris and South West'

#Channel 
levels(data01$Channel)[levels(data01$Channel)=='B']='B+L'
levels(data01$Channel)[levels(data01$Channel)=='L']='B+L'

#Garage 
levels(data01$Garage)[levels(data01$Garage)=='Opened collective parking']='Opened & closed collective parking'
levels(data01$Garage)[levels(data01$Garage)=='Closed collective parking']='Opened & closed collective parking'
################################################################################

str(data01)
n <- nrow(data01)

Sev_Windscreen01=NULL
Sev_Damage01=NULL
Sev_Fire01=NULL
Sev_Other01=NULL
Sev_Theft01=NULL
Sev_TPL01=NULL

for (i in 1:n) {
  Sev_Windscreen01[i]= ifelse(data01$Sev_Windscreen[i] != 0 & data01$Windscreen[i] !=0 , data01$Sev_Windscreen[i] / data01$Windscreen[i],0)
  Sev_Damage01[i]= ifelse(data01$Sev_Damage[i] != 0 & data01$Damage[i] !=0 , data01$Sev_Damage[i] / data01$Damage[i],0)
  Sev_Fire01[i]= ifelse(data01$Sev_Fire[i] != 0 & data01$Fire[i] !=0 , data01$Sev_Fire[i] / data01$Fire[i],0)
  Sev_Other01[i]= ifelse(data01$Sev_Other[i] != 0 & data01$Other[i] !=0 , data01$Sev_Other[i] / data01$Other[i],0)
  Sev_Theft01[i]= ifelse(data01$Sev_Theft[i] != 0 & data01$Theft[i] !=0 , data01$Sev_Theft[i] / data01$Theft[i],0)
  Sev_TPL01[i]= ifelse(data01$Sev_TPL[i] != 0 & data01$TPL[i] !=0 , data01$Sev_TPL[i] / data01$TPL[i],0)
  }

data01$Sev_Windscreen=Sev_Windscreen01
data01$Sev_Damage=Sev_Damage01
data01$Sev_Fire=Sev_Fire01
data01$Sev_Other=Sev_Other01
data01$Sev_TPL=Sev_TPL01
data01$Sev_Theft=Sev_Theft01

summary(data01)
Sev_Total=NULL
Total=NULL
for (i in 1:nrow(data01)){
  Sev_Total[i]=data01$Sev_Damage[i]+data01$Sev_Fire[i]+data01$Sev_Theft[i]+data01$Sev_Other[i]+data01$Sev_TPL[i]+data01$Sev_Windscreen[i]
  Total[i]=data01$Damage[i]+data01$Fire[i]+data01$Theft[i]+data01$Other[i]+data01$TPL[i]+data01$Windscreen[i]
}
data01=cbind(data01,Sev_Total,Total)
data01=subset(data01,select = c("BonusMalus","PayFreq","VehClass","VehPower", "VehGas"
                                ,"VehAge","Garage","Area", "Region", "Channel","DrivGender","DrivAge","Marketing","Windscreen"
                                ,"TPL","Damage","Other","Theft", "Fire","Total","Sev_Windscreen","Sev_TPL","Sev_Damage"
                                ,"Sev_Other","Sev_Theft","Sev_Fire","Sev_Total"))

#Data_training dan data_testing dengan pembagian data 70% data training dan 30% 
#data testing berdasarkan sev_windscreen
dt<-  createDataPartition(data01$Sev_Windscreen, p = .7,list = FALSE)
data_training<- data01[dt,]
data_testing <- data01[-dt,]
summary(data_training)
#######################################################################################################
#Model severity untuk sev_windscreen 
attach(data_training)

data_training_sev_subset=subset(data_training,Sev_Windscreen>0)
data_testing_sev_subset=data_testing
summary(data_training_sev_subset)

# Mengubah data_kategorikal
data_VehClass_Windscreen=mapvalues(data_training_sev_subset$VehClass, from = c("Cheapest","Cheaper","other")
                         , to = c("1", "2","3"))
data_VehClass_Windscreen=data.frame(as.factor(data_VehClass_Windscreen))

frequency_table <- table(data_VehClass_Windscreen)

data_VehGas_Windscreen=mapvalues(data_training_sev_subset$VehGas, from = c("Diesel","Regular")
                                   , to = c("1", "2"))
data_VehGas_Windscreen=data.frame(as.factor(data_VehGas_Windscreen))
frequency_table_VehGas <- table(data_VehGas_Windscreen)

data_DrivGender_Windscreen=mapvalues(data_training_sev_subset$DrivGender, from = c("F","M")
                                 , to = c("1", "2"))
data_DrivGender_Windscreen=data.frame(as.factor(data_DrivGender_Windscreen))
frequency_table_DrivGender <- table(data_DrivGender_Windscreen)

PR_windscreen=NULL
for( i in 1:3){ 
  PR_windscreen[i]=frequency_table[i]/(frequency_table[1]+frequency_table[2]+frequency_table[3])
}
PR_windscreen

PR_VehGas=NULL
for( i in 1:2){ 
  PR_VehGas[i]=frequency_table_VehGas[i]/(frequency_table_VehGas[1]+frequency_table_VehGas[2])
}
PR_VehGas

PR_DrivGender=NULL
for( i in 1:2){ 
  PR_DrivGender[i]=frequency_table_DrivGender[i]/(frequency_table_DrivGender[1]+frequency_table_DrivGender[2])
}
PR_DrivGender

# sort 
nama <- c("Cheapest","Cheaper","other")
cumprob_windscreen=data.frame(cbind(nama,PR_windscreen))
prob_windscreen=cumprob_windscreen[order(cumprob_windscreen$PR_windscreen,decreasing = TRUE),]
prob_windscreen$PR_windscreen=as.numeric(prob_windscreen$PR_windscreen)

nama_VehGas <- c("Diesel","Regular")
cumprob_VehGas=data.frame(cbind(nama_VehGas,PR_VehGas))
prob_VehGas=cumprob_VehGas[order(cumprob_VehGas$PR_VehGas,decreasing = TRUE),]
prob_VehGas$PR_VehGas=as.numeric(prob_VehGas$PR_VehGas)

nama_DrivGender <- c("F","M")
cumprob_DrivGender=data.frame(cbind(nama_DrivGender,PR_DrivGender))
prob_DrivGender=cumprob_DrivGender[order(cumprob_DrivGender$PR_DrivGender,decreasing = TRUE),]
prob_DrivGender$PR_DrivGender=as.numeric(prob_DrivGender$PR_DrivGender)

# shape 1 dan shape 2 beta 
alpha_cheapest_beta= prob_windscreen$PR_windscreen[1]*(((prob_windscreen$PR_windscreen[1])*(1-prob_windscreen$PR_windscreen[1])/var(prob_windscreen$PR_windscreen))-1)
beta_cheapest_beta= (1-prob_windscreen$PR_windscreen[1])*(((prob_windscreen$PR_windscreen[1])*(1-prob_windscreen$PR_windscreen[1])/var(prob_windscreen$PR_windscreen))-1)

alpha_cheaper_beta= prob_windscreen$PR_windscreen[2]*(((prob_windscreen$PR_windscreen[2])*(1-prob_windscreen$PR_windscreen[2])/var(prob_windscreen$PR_windscreen))-1)
beta_cheaper_beta= (1-prob_windscreen$PR_windscreen[2])*(((prob_windscreen$PR_windscreen[2])*(1-prob_windscreen$PR_windscreen[2])/var(prob_windscreen$PR_windscreen))-1)

alpha_other_beta= prob_windscreen$PR_windscreen[3]*(((prob_windscreen$PR_windscreen[3])*(1-prob_windscreen$PR_windscreen[3])/var(prob_windscreen$PR_windscreen))-1)
beta_other_beta= (1-prob_windscreen$PR_windscreen[3])*(((prob_windscreen$PR_windscreen[3])*(1-prob_windscreen$PR_windscreen[3])/var(prob_windscreen$PR_windscreen))-1)

alpha_diesel=prob_VehGas$PR_VehGas[1]*(((prob_VehGas$PR_VehGas[1])*(1-prob_VehGas$PR_VehGas[1])/var(prob_VehGas$PR_VehGas))-1)
beta_diesel=(1-prob_VehGas$PR_VehGas[1])*(((prob_VehGas$PR_VehGas[1])*(1-prob_VehGas$PR_VehGas[1])/var(prob_VehGas$PR_VehGas))-1)

alpha_regular=prob_VehGas$PR_VehGas[2]*(((prob_VehGas$PR_VehGas[2])*(1-prob_VehGas$PR_VehGas[2])/var(prob_VehGas$PR_VehGas))-1)
beta_regular=(1-prob_VehGas$PR_VehGas[2])*(((prob_VehGas$PR_VehGas[2])*(1-prob_VehGas$PR_VehGas[2])/var(prob_VehGas$PR_VehGas))-1)

alpha_M=prob_DrivGender$PR_DrivGender[1]*(((prob_DrivGender$PR_DrivGender[1])*(1-prob_DrivGender$PR_DrivGender[1])/var(prob_DrivGender$PR_DrivGender))-1)
beta_M=(1-prob_DrivGender$PR_DrivGender[2])*(((prob_DrivGender$PR_DrivGender[2])*(1-prob_DrivGender$PR_DrivGender[2])/var(prob_DrivGender$PR_DrivGender))-1)

# menggunakan rbeta
set.seed(105)
cheapest_beta_training=rbeta(nrow(data_training_sev_subset),shape1 = alpha_cheapest_beta,shape2 = beta_cheapest_beta)
set.seed(106)
other_beta_training =rbeta(nrow(data_training_sev_subset),shape1 = alpha_other_beta,shape2 = beta_other_beta)
set.seed(107)
cheaper_beta_training= rbeta(nrow(data_training_sev_subset),shape1 = alpha_cheaper_beta,shape2 = beta_cheaper_beta)
set.seed(108)
diesel_beta_training = rbeta(nrow(data_training_sev_subset),shape1 = alpha_diesel,shape2 = beta_diesel)
set.seed(109)
regular_beta_training=rbeta(nrow(data_training_sev_subset),shape1 = alpha_regular,shape2 = beta_regular)
set.seed(110)
M_beta_training=rbeta(nrow(data_training_sev_subset),shape1 = alpha_M,shape2 = beta_M)

# Menggunakan Truncated Gaussian
set.seed(105)
cheapest_trun_norm_training=rtruncnorm(nrow(data_training_sev_subset),a=0,b=prob_windscreen$PR_windscreen[1],mean =(prob_windscreen$PR_windscreen[1]+0)/2,sd=(prob_windscreen$PR_windscreen[1]-0)/6)
set.seed(106)
cheaper_trun_norm_training= rtruncnorm(nrow(data_training_sev_subset),a=prob_windscreen$PR_windscreen[1],b=(prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]),mean =(prob_windscreen$PR_windscreen[1]+(prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]))/2,sd=((prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2])-prob_windscreen$PR_windscreen[1])/6)
set.seed(107)
other_trun_norm_training=rtruncnorm(nrow(data_training_sev_subset),a=(prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]),b=(prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]+prob_windscreen$PR_windscreen[3]),mean =((prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]+prob_windscreen$PR_windscreen[3])+(prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]))/2,sd=((prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]+prob_windscreen$PR_windscreen[3])-(prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]))/6)
set.seed(108)
diesel_trun_norm_training=rtruncnorm(nrow(data_training_sev_subset),a=0,b=prob_VehGas$PR_VehGas[1],mean =(0+prob_VehGas$PR_VehGas[1])/2,sd=((prob_VehGas$PR_VehGas[1]-0)/6))
set.seed(109)
regular_trun_norm_training=rtruncnorm(nrow(data_training_sev_subset),a=(prob_VehGas$PR_VehGas[1]),b=(prob_VehGas$PR_VehGas[1]+prob_VehGas$PR_VehGas[2]),mean =((prob_VehGas$PR_VehGas[1]+prob_VehGas$PR_VehGas[2])+(prob_VehGas$PR_VehGas[1]))/2,sd=((prob_VehGas$PR_VehGas[1]+prob_VehGas$PR_VehGas[2])-(prob_VehGas$PR_VehGas[1]))/6)
set.seed(110)
M_trun_norm_training=rtruncnorm(nrow(data_training_sev_subset),a=0,b=prob_DrivGender$PR_DrivGender[1],mean =((prob_DrivGender$PR_DrivGender[1])+0)/2,sd=((prob_DrivGender$PR_DrivGender[1]-0)/6))

data_training_sev_subset=cbind(data_training_sev_subset,cheapest_beta_training,other_beta_training,cheaper_beta_training,cheapest_trun_norm_training,other_trun_norm_training,cheaper_trun_norm_training
                               ,regular_beta_training,regular_trun_norm_training,M_beta_training,M_trun_norm_training)

set.seed(100)
# Family: Gamma(link="log") 
mod1_sev=glm(Sev_Windscreen~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = Gamma(link="log"),data = data_training_sev_subset)
summary(mod1_sev)
step_mod1_sev=stepAIC(mod1_sev,direction = "both")
summary(step_mod1_sev)

# Family : Gaussian (link="identity")
mod2_sev=glm(Sev_Windscreen~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = gaussian(link = "identity"),data = data_training_sev_subset)
summary(mod2_sev)
step_mod2_sev= stepAIC(mod2_sev,direction = "both")
summary(step_mod2_sev)

mod2_sev_log=glm(Sev_Windscreen~DrivAge+DrivGender+BonusMalus+VehClass+VehAge+VehGas+Channel,family = gaussian(link = "log"),data = data_training_sev_subset)
summary(mod2_sev_log) 
step_mod2_sev_log= stepAIC(mod2_sev_log,direction = "both")
summary(step_mod2_sev_log)

# Family : Inverse Gaussian (link="log")
mod3_sev=glm(Sev_Windscreen~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Area+Region+Channel,family = inverse.gaussian(link="log"),data = data_training_sev_subset)
summary(mod3_sev)#Garage  
step_mod3_sev= stepAIC(mod3_sev,direction = "both")
summary(step_mod3_sev)

# Predict 
y_hat_Windscreen_Gamma=predict(step_mod1_sev,newdata=data_testing_sev_subset,type = "response")
MSE_y_hat_Windscreen_Gamma=sum((data_testing_sev_subset$Sev_Windscreen-y_hat_Windscreen_Gamma)^2)/nrow(data_testing_sev_subset)
MSE_y_hat_Windscreen_Gamma

y_hat_Windscreen_Gaussian= predict(step_mod2_sev,data_testing_sev_subset,type = "response")
MSE_y_hat_Windscreen_Gaussian=sum((data_testing_sev_subset$Sev_Windscreen-y_hat_Windscreen_Gaussian)^2)/nrow(data_testing_sev_subset)
MSE_y_hat_Windscreen_Gaussian

y_hat_Windscreen_Gaussian_log= predict(step_mod2_sev_log,data_testing_sev_subset,type = "response")
MSE_y_hat_Windscreen_Gaussian_log=sum((data_testing_sev_subset$Sev_Windscreen-y_hat_Windscreen_Gaussian_log)^2)/nrow(data_testing_sev_subset)
MSE_y_hat_Windscreen_Gaussian_log

y_hat_Windscreen_Inverse_Gaussian=predict(step_mod3_sev,data_testing_sev_subset,type = "response")
MSE_y_hat_Windscreen_Inverse_Gaussian=sum((data_testing_sev_subset$Sev_Windscreen-y_hat_Windscreen_Inverse_Gaussian)^2)/nrow(data_testing_sev_subset)
MSE_y_hat_Windscreen_Inverse_Gaussian
#-----------------------------------------------------------------------------------------------------------------------
# copula sev windscrreen 
#copula 
# Replace data ke-677 dengan quantile data 99.95% 
data_training_sev_subset01=data_training_sev_subset
data_training_sev_subset01$Sev_Windscreen[677]=quantile(data_training_sev_subset01$Sev_Windscreen,0.9995)
set.seed(100)
u_training_windscreen1=pobs(data_training_sev_subset01[c(21,37,31,6,35)])
fit.copula_training_windcreen1=fitCopula(normalCopula(dim = 5,dispstr = "un"),u_training_windscreen1,method="ml")
summary(fit.copula_training_windcreen1) 

# Membentuk matrix parameter copula R dari data training 
R_windscreen_training1=matrix(1,nrow=5,ncol=5)
for (i in 1:5) {
  if(i>=2 && i<=5){R_windscreen_training1[1,i]=fit.copula_training_windcreen1@estimate[i-1]}
  if(i>=3 && i<=5){R_windscreen_training1[2,i]=fit.copula_training_windcreen1@estimate[i+2]}
  if(i>=4 && i<=5){R_windscreen_training1[3,i]=fit.copula_training_windcreen1@estimate[i+4]}
  if(i>=5 && i<=5){R_windscreen_training1[4,i]=fit.copula_training_windcreen1@estimate[i+5]}
  }
segitiga_bawah <- lower.tri(R_windscreen_training1,diag = TRUE)
R_windscreen_training1[segitiga_bawah]=0
t_R_windscreen_training1=t(R_windscreen_training1)
R_windscreen_training1=R_windscreen_training1+t_R_windscreen_training1
diag(R_windscreen_training1)=1

R2_windscreen_training1=R_windscreen_training1[1:4,1:4]
r_windscreen_training1=R_windscreen_training1[1:4,5]

y_simull_training=NULL
mean_training=NULL
y_windscreen_training=NULL
pl_training=NULL
q_windscreen_training=NULL
v1_cheapest_trun_norm_training=NULL
v2_DrivGender_trun_norm_training=NULL
v3_VehAge_training=NULL
v4_VehGas_training=NULL
b_windscreen_training=NULL
random_training=NULL
v_mat_windscreen_training=matrix(NA,ncol=4,nrow = nrow(data_training_sev_subset))
acceptance_rates01=NULL
set.seed(100)
for(j in 1:nrow(data_training_sev_subset)){
  y_simull_training=NULL
  sampel_yang_diterima = 0 
  total_sampel = 0 
  for (i in 1:100){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_cheapest_trun_norm_training[j]=qnorm(ptruncnorm(data_training_sev_subset01$cheapest_trun_norm_training[j],a=0,b=prob_windscreen$PR_windscreen[1],mean =(prob_windscreen$PR_windscreen[1]+0)/2,sd=(prob_windscreen$PR_windscreen[1]-0)/6),0,1,lower.tail = TRUE)
    v2_DrivGender_trun_norm_training[j]=qnorm(ptruncnorm(data_training_sev_subset01$M_trun_norm_training[j],a=0,b=prob_DrivGender$PR_DrivGender[1],mean =((prob_DrivGender$PR_DrivGender[1])+0)/2,sd=((prob_DrivGender$PR_DrivGender[1]-0)/6)),0,1,lower.tail = TRUE)
    v3_VehAge_training[j]=qnorm(pnorm(data_training_sev_subset01$VehAge[j],mean=6.9964,sd=3.9417),0,1,lower.tail = TRUE)
    v4_VehGas_training[j]=qnorm(ptruncnorm(data_training_sev_subset01$regular_trun_norm_training[j],a=(prob_VehGas$PR_VehGas[1]),b=(prob_VehGas$PR_VehGas[1]+prob_VehGas$PR_VehGas[2]),mean =((prob_VehGas$PR_VehGas[1]+prob_VehGas$PR_VehGas[2])+(prob_VehGas$PR_VehGas[1]))/2,sd=((prob_VehGas$PR_VehGas[1]+prob_VehGas$PR_VehGas[2])-(prob_VehGas$PR_VehGas[1]))/6))
    # matrix baris dari v*  
    v_mat_windscreen_training[j,1]<-v1_cheapest_trun_norm_training[j]
    v_mat_windscreen_training[j,2]<-v2_DrivGender_trun_norm_training[j]
    v_mat_windscreen_training[j,3]<-v3_VehAge_training[j]
    v_mat_windscreen_training[j,4]<-v4_VehGas_training[j]
    vmat_windscreen_training=t(v_mat_windscreen_training)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_windscreen  
    a_windscreen_training= 1-t(r_windscreen_training1)%*%inv(R2_windscreen_training1)%*%r_windscreen_training1
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
    b_windscreen_training[j]= t(r_windscreen_training1)%*%inv(R2_windscreen_training1)%*%(vmat_windscreen_training[,j])
    
    # pdf(y) yang digunakan disini adalah normal 
    random=rnorm(nrow(data_training_sev_subset),mean = 280.87 ,sd=434.07)
    
    y_windscreen_training[j]=qnorm(pnorm(random[j],mean = 280.87 ,sd=434.07),0,1)
    
    # q disini merupakan pdf dari y|x1,x
    q_windscreen_training[j]=(dnorm(random[j],mean = 280.87 ,sd=434.07 ))*exp(-0.5*(((y_windscreen_training[j]-b_windscreen_training[j])^2/a_windscreen_training)-(y_windscreen_training[j])^2))*(a_windscreen_training)^(-0.5)

    pl_training[j]=1*dnorm(random[j],mean = 300, sd=450)
    u=runif(1,0)
    if(u<q_windscreen_training[j]/pl_training[j]){
      y_simull_training = c(y_simull_training,random)
      mean_training[j]=mean(y_simull_training)
      sampel_yang_diterima = sampel_yang_diterima + 1
    }
    total_sampel = total_sampel + 1
  }
    acceptance_rates01[j] <- sampel_yang_diterima / total_sampel
}

sum(acceptance_rates01==1)/nrow(data_training_sev_subset)
mean_training

plot(density(pl_training),col="blue",main="Plot Densitas Distribus Target VS \nProposal Normal-truncated Normal-Normal Windscreen")
lines(density(q_windscreen_training),col="red")

plot(density(mean_training)) 

#uji normalitas 
shapiro.test(mean_training)

# model testing 
set.seed(105)
cheapest_trun_norm_testing=rtruncnorm(nrow(data_testing_sev_subset),a=0,b=prob_windscreen$PR_windscreen[1],mean =(prob_windscreen$PR_windscreen[1]+0)/2,sd=(prob_windscreen$PR_windscreen[1]-0)/6)
set.seed(106)
cheaper_trun_norm_testing= rtruncnorm(nrow(data_testing_sev_subset),a=prob_windscreen$PR_windscreen[1],b=(prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]),mean =(prob_windscreen$PR_windscreen[1]+(prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]))/2,sd=((prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2])-prob_windscreen$PR_windscreen[1])/6)
set.seed(107)
other_trun_norm_testing=rtruncnorm(nrow(data_testing_sev_subset),a=(prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]),b=(prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]+prob_windscreen$PR_windscreen[3]),mean =((prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]+prob_windscreen$PR_windscreen[3])+(prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]))/2,sd=((prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]+prob_windscreen$PR_windscreen[3])-(prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]))/6)
set.seed(108)
diesel_trun_norm_testing=rtruncnorm(nrow(data_testing_sev_subset),a=0,b=prob_VehGas$PR_VehGas[1],mean =(0+prob_VehGas$PR_VehGas[1])/2,sd=((prob_VehGas$PR_VehGas[1]-0)/6))
set.seed(109)
regular_trun_norm_testing=rtruncnorm(nrow(data_testing_sev_subset),a=(prob_VehGas$PR_VehGas[1]),b=(prob_VehGas$PR_VehGas[1]+prob_VehGas$PR_VehGas[2]),mean =((prob_VehGas$PR_VehGas[1]+prob_VehGas$PR_VehGas[2])+(prob_VehGas$PR_VehGas[1]))/2,sd=((prob_VehGas$PR_VehGas[1]+prob_VehGas$PR_VehGas[2])-(prob_VehGas$PR_VehGas[1]))/6)
set.seed(110)
M_trun_norm_testing=rtruncnorm(nrow(data_testing_sev_subset),a=0,b=prob_DrivGender$PR_DrivGender[1],mean =((prob_DrivGender$PR_DrivGender[1])+0)/2,sd=((prob_DrivGender$PR_DrivGender[1]-0)/6))

set.seed(105)
cheapest_beta_testing=rbeta(nrow(data_testing_sev_subset),shape1 = alpha_cheapest_beta,shape2 = beta_cheapest_beta)
set.seed(106)
other_beta_testing =rbeta(nrow(data_testing_sev_subset),shape1 = alpha_other_beta,shape2 = beta_other_beta)
set.seed(107)
cheaper_beta_testing= rbeta(nrow(data_testing_sev_subset),shape1 = alpha_cheaper_beta,shape2 = beta_cheaper_beta)
set.seed(108)
diesel_beta_testing = rbeta(nrow(data_testing_sev_subset),shape1 = alpha_diesel,shape2 = beta_diesel)
set.seed(109)
regular_beta_testing=rbeta(nrow(data_testing_sev_subset),shape1 = alpha_regular,shape2 = beta_regular)
set.seed(110)
M_beta_testing=rbeta(nrow(data_testing_sev_subset),shape1 = alpha_M,shape2 = beta_M)


data_testing_sev_subset01=cbind(data_testing_sev_subset,cheapest_trun_norm_testing,cheaper_trun_norm_testing,regular_trun_norm_testing,M_trun_norm_testing
                                ,cheapest_beta_testing,regular_beta_testing,M_beta_testing)

y_simull_testing=NULL
mean_testing01=NULL
y_windscreen_testing=NULL
pl_testing=NULL
q_windscreen_testing=NULL
v1_cheapest_trun_norm_testing=NULL
v2_DrivGender_trun_norm_testing=NULL
v3_VehAge_testing=NULL
v4_VehGas_testing=NULL
b_windscreen_testing=NULL
random=NULL
v_mat_windscreen_testing=matrix(NA,ncol=4,nrow = nrow(data_testing_sev_subset))
set.seed(100)
for(j in 1:nrow(data_testing_sev_subset01)){
  y_simull_testing=NULL
  for (i in 1:450){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_cheapest_trun_norm_testing[j]=qnorm(ptruncnorm(data_testing_sev_subset01$cheapest_trun_norm_testing[j],a=0,b=prob_windscreen$PR_windscreen[1],mean =(prob_windscreen$PR_windscreen[1]+0)/2,sd=(prob_windscreen$PR_windscreen[1]-0)/6),0,1,lower.tail = TRUE)
    v2_DrivGender_trun_norm_testing[j]=qnorm(ptruncnorm(data_testing_sev_subset01$M_trun_norm_testing[j],a=0,b=prob_DrivGender$PR_DrivGender[1],mean =((prob_DrivGender$PR_DrivGender[1])+0)/2,sd=((prob_DrivGender$PR_DrivGender[1]-0)/6)),0,1,lower.tail = TRUE)
    v3_VehAge_testing[j]=qnorm(pnorm(data_testing_sev_subset01$VehAge[j],mean=6.9964,sd=3.9417),0,1,lower.tail = TRUE)
    v4_VehGas_testing[j]=qnorm(ptruncnorm(data_testing_sev_subset01$regular_trun_norm_testing[j],a=(prob_VehGas$PR_VehGas[1]),b=(prob_VehGas$PR_VehGas[1]+prob_VehGas$PR_VehGas[2]),mean =((prob_VehGas$PR_VehGas[1]+prob_VehGas$PR_VehGas[2])+(prob_VehGas$PR_VehGas[1]))/2,sd=((prob_VehGas$PR_VehGas[1]+prob_VehGas$PR_VehGas[2])-(prob_VehGas$PR_VehGas[1]))/6))
    # matrix baris dari v*  
    v_mat_windscreen_testing[j,1]<-v1_cheapest_trun_norm_testing[j]
    v_mat_windscreen_testing[j,2]<-v2_DrivGender_trun_norm_testing[j]
    v_mat_windscreen_testing[j,3]<-v3_VehAge_testing[j]
    v_mat_windscreen_testing[j,4]<-v4_VehGas_testing[j]
    vmat_windscreen_testing=t(v_mat_windscreen_testing)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_windscreen  
    a_windscreen_testing= 1-t(r_windscreen_training1)%*%inv(R2_windscreen_training1)%*%r_windscreen_training1
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
    b_windscreen_testing[j]= t(r_windscreen_training1)%*%inv(R2_windscreen_training1)%*%(vmat_windscreen_testing[,j])
    
    # pdf(y) yang digunakan disini adalah normal 
    random=rnorm(nrow(data_testing_sev_subset),mean = 280.87 ,sd=434.07)
    
    y_windscreen_testing[j]=qnorm(pnorm(random[j],mean = 280.87 ,sd=434.07),0,1)
    
    
    # q disini merupakan pdf dari y|x1,x
    q_windscreen_testing[j]=(dnorm(random[j],mean = 280.87 ,sd=434.07 ))*exp(-0.5*(((y_windscreen_testing[j]-b_windscreen_testing[j])^2/a_windscreen_testing)-(y_windscreen_testing[j])^2))*(a_windscreen_testing)^(-0.5)
    pl_testing[j]=1*dnorm(random[j],mean = 300, sd=450)

        u=runif(1,0)
    if(u<q_windscreen_testing[j]/pl_testing[j]){
      y_simull_testing= c(y_simull_testing,random)
      mean_testing01[j]=mean(y_simull_testing)
    }
  }
}

MSE_copula_Windscreen_testing=sum((data_testing_sev_subset01$Sev_Windscreen-mean_testing01)^2)/nrow(data_testing_sev_subset01)
MSE_copula_Windscreen_testing

plot(density(mean_testing01))

#uji normalitas 
shapiro.test(mean_testing01)

# normal dan beta 
set.seed(100)
u_training_windscreen1_beta=pobs(data_training_sev_subset01[c(21,28,34,36,6)])
fit.copula_training_windcreen1_beta=fitCopula(normalCopula(dim = 5,dispstr = "un"),u_training_windscreen1_beta,method="ml")
summary(fit.copula_training_windcreen1_beta) 

# Membentuk matrix parameter copula R dari data training 
R_windscreen_training1_beta=matrix(1,nrow=5,ncol=5)
for (i in 1:5) {
  if(i>=2 && i<=5){R_windscreen_training1_beta[1,i]=fit.copula_training_windcreen1_beta@estimate[i-1]}
  if(i>=3 && i<=5){R_windscreen_training1_beta[2,i]=fit.copula_training_windcreen1_beta@estimate[i+2]}
  if(i>=4 && i<=5){R_windscreen_training1_beta[3,i]=fit.copula_training_windcreen1_beta@estimate[i+4]}
  if(i>=5 && i<=5){R_windscreen_training1_beta[4,i]=fit.copula_training_windcreen1_beta@estimate[i+5]}
}
segitiga_bawah <- lower.tri(R_windscreen_training1_beta,diag = TRUE)
R_windscreen_training1_beta[segitiga_bawah]=0
t_R_windscreen_training1_beta=t(R_windscreen_training1_beta)
R_windscreen_training1_beta=R_windscreen_training1_beta+t_R_windscreen_training1_beta
diag(R_windscreen_training1_beta)=1

R2_windscreen_training1_beta=R_windscreen_training1_beta[1:4,1:4]
r_windscreen_training1_beta=R_windscreen_training1_beta[1:4,5]

y_simull_training_beta=NULL
mean_training_beta01=NULL
y_windscreen_training_beta=NULL
pl_training_beta=NULL
q_windscreen_training_beta=NULL
v1_cheapest_beta_training=NULL
v2_DrivGender_beta_training=NULL
v3_VehAge_training_beta=NULL
v4_VehGas_training_beta=NULL
b_windscreen_training_beta=NULL
random_training=NULL
v_mat_windscreen_training_beta=matrix(NA,ncol=4,nrow = nrow(data_training_sev_subset))
acceptance_rates01_beta=NULL
set.seed(100)
for(j in 1:nrow(data_training_sev_subset)){
  y_simull_training_beta=NULL
  sampel_yang_diterima = 0 
  total_sampel = 0 
  for (i in 1:100){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_cheapest_beta_training[j]=qnorm(pbeta(data_training_sev_subset01$cheapest_beta_training[j],shape1 = alpha_cheapest_beta,shape2 = beta_cheapest_beta),0,1,lower.tail = TRUE)
    v2_DrivGender_beta_training[j]=qnorm(pbeta(data_training_sev_subset01$M_beta_training[j],shape1 = alpha_M,shape2 = beta_M),0,1,lower.tail = TRUE)
    v3_VehAge_training_beta[j]=qnorm(pnorm(data_training_sev_subset01$VehAge[j],mean=6.9964,sd=3.9417),0,1,lower.tail = TRUE)
    v4_VehGas_training_beta[j]=qnorm(pbeta(data_training_sev_subset01$regular_beta_training[j],shape1 = alpha_regular,shape2 = beta_regular))
    # matrix baris dari v*  
    v_mat_windscreen_training_beta[j,1]<-v1_cheapest_beta_training[j]
    v_mat_windscreen_training_beta[j,2]<-v2_DrivGender_beta_training[j]
    v_mat_windscreen_training_beta[j,3]<-v3_VehAge_training_beta[j]
    v_mat_windscreen_training_beta[j,4]<-v4_VehGas_training_beta[j]
    vmat_windscreen_training_beta=t(v_mat_windscreen_training_beta)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_windscreen  
    a_windscreen_training_beta= 1-t(r_windscreen_training1_beta)%*%inv(R2_windscreen_training1_beta)%*%r_windscreen_training1_beta
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
    b_windscreen_training_beta[j]= t(r_windscreen_training1_beta)%*%inv(R2_windscreen_training1_beta)%*%(vmat_windscreen_training_beta[,j])
    
    # pdf(y) yang digunakan disini adalah normal 
    random=rnorm(nrow(data_training_sev_subset),mean = 280.87 ,sd=434.07)
    
    y_windscreen_training_beta[j]=qnorm(pnorm(random[j],mean = 280.87 ,sd=434.07),0,1)
    
    # q disini merupakan pdf dari y|x1,x
    q_windscreen_training_beta[j]=(dnorm(random[j],mean = 280.87 ,sd=434.07 ))*exp(-0.5*(((y_windscreen_training_beta[j]-b_windscreen_training_beta[j])^2/a_windscreen_training_beta)-(y_windscreen_training_beta[j])^2))*(a_windscreen_training_beta)^(-0.5)
    
    pl_training_beta[j]=0.98*dnorm(random[j],mean = 192 ,sd=434)
    u=runif(1,0)
    if(u<q_windscreen_training_beta[j]/pl_training_beta[j]){
      y_simull_training_beta = c(y_simull_training_beta,random)
      mean_training_beta01[j]=mean(y_simull_training_beta)
      sampel_yang_diterima = sampel_yang_diterima + 1
   }
    total_sampel = total_sampel + 1
  }
  acceptance_rates01_beta[j] <- sampel_yang_diterima / total_sampel
}
sum(acceptance_rates01_beta==1)/nrow(data_training_sev_subset)
mean_training_beta01

plot(density(pl_training_beta),col="blue",main="Normal-Beta")
lines(density(q_windscreen_training_beta),col="red")

plot(density(mean_training_beta01))

#uji normalitas 
shapiro.test(mean_training_beta01)

# model testing normal dan beta
y_simull_testing_beta=NULL
mean_testing_beta01=NULL
y_windscreen_testing_beta=NULL
pl_testing_beta=NULL
q_windscreen_testing_beta=NULL
v1_cheapest_beta_testing=NULL
v2_DrivGender_beta_testing=NULL
v3_VehAge_testing_beta=NULL
v4_VehGas_testing_beta=NULL
b_windscreen_testing_beta=NULL
v_mat_windscreen_testing_beta=matrix(NA,ncol=4,nrow = nrow(data_testing_sev_subset))

set.seed(100)
for(j in 1:nrow(data_testing_sev_subset01)){
  y_simull_testing_beta=NULL
  for (i in 1:450){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
  v1_cheapest_beta_testing[j]=qnorm(pbeta(data_testing_sev_subset01$cheapest_beta_testing[j],shape1 = alpha_cheapest_beta,shape2 = beta_cheapest_beta),0,1,lower.tail = TRUE)
  v2_DrivGender_beta_testing[j]=qnorm(pbeta(data_testing_sev_subset01$M_beta_testing[j],shape1 = alpha_M,shape2 = beta_M),0,1,lower.tail = TRUE)
  v3_VehAge_testing_beta[j]=qnorm(pnorm(data_testing_sev_subset01$VehAge[j],mean=6.9964,sd=3.9417),0,1,lower.tail = TRUE)
  v4_VehGas_testing_beta[j]=qnorm(pbeta(data_testing_sev_subset01$regular_beta_testing[j],shape1 = alpha_regular,shape2 = beta_regular))
  # matrix baris dari v*  
    v_mat_windscreen_testing_beta[j,1]<-v1_cheapest_beta_testing[j]
    v_mat_windscreen_testing_beta[j,2]<-v2_DrivGender_beta_testing[j]
    v_mat_windscreen_testing_beta[j,3]<-v3_VehAge_testing_beta[j]
    v_mat_windscreen_testing_beta[j,4]<-v4_VehGas_testing_beta[j]
    vmat_windscreen_testing_beta=t(v_mat_windscreen_testing_beta)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_windscreen  
    a_windscreen_testing= 1-t(r_windscreen_training1_beta)%*%inv(R2_windscreen_training1_beta)%*%r_windscreen_training1_beta
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
    b_windscreen_testing_beta[j]= t(r_windscreen_training1_beta)%*%inv(R2_windscreen_training1_beta)%*%(vmat_windscreen_testing_beta[,j])
    
    # pdf(y) yang digunakan disini adalah normal 
    random=rnorm(nrow(data_testing_sev_subset),mean = 280.87 ,sd=434.07)
    
    y_windscreen_testing_beta[j]=qnorm(pnorm(random[j],mean = 280.87 ,sd=434.07),0,1)
    
    # q disini merupakan pdf dari y|x1,x
    q_windscreen_testing_beta[j]=(dnorm(random[j],mean = 280.87 ,sd=434.07 ))*exp(-0.5*(((y_windscreen_testing_beta[j]-b_windscreen_testing_beta[j])^2/a_windscreen_testing)-(y_windscreen_testing_beta[j])^2))*(a_windscreen_testing)^(-0.5)
 
    pl_testing_beta[j]=0.98*dnorm(random[j],mean = 192 ,sd=434)
    u=runif(1,0)
    if(u<q_windscreen_testing_beta[j]/pl_testing_beta[j]){
      y_simull_testing_beta= c(y_simull_testing_beta,random)
      mean_testing_beta01[j]=mean(y_simull_testing_beta)
    }
  }
}

MSE_copula_Windscreen_testing_beta=sum((data_testing_sev_subset01$Sev_Windscreen-mean_testing_beta01)^2)/nrow(data_testing_sev_subset01)
MSE_copula_Windscreen_testing_beta


plot(density(mean_testing_beta01),main ="Plot Densitas Prediksi Copula \n Normal-beta-Normal Windscreen")

#uji normalitas 
shapiro.test(mean_testing_beta01)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#model dengan data ke-677 dihapus 
data_training_sev_subset02=data_training_sev_subset01[-677,]


y_simull_training02=NULL
mean_training02=NULL
y_windscreen_training02=NULL
pl_training02=NULL
q_windscreen_training02=NULL
v1_cheapest_trun_norm_training02=NULL
v2_DrivGender_trun_norm_training02=NULL
v3_VehAge_training02=NULL
v4_VehGas_training02=NULL
b_windscreen_training02=NULL
random_training02=NULL
v_mat_windscreen_training02=matrix(NA,ncol=4,nrow = nrow(data_training_sev_subset02))
acceptance_rates02=NULL
set.seed(100)
for(j in 1:nrow(data_training_sev_subset02)){
  y_simull_training02=NULL
  sampel_yang_diterima = 0 
  total_sampel = 0 
  for (i in 1:100){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_cheapest_trun_norm_training02[j]=qnorm(ptruncnorm(data_training_sev_subset02$cheapest_trun_norm_training[j],a=0,b=prob_windscreen$PR_windscreen[1],mean =(prob_windscreen$PR_windscreen[1]+0)/2,sd=(prob_windscreen$PR_windscreen[1]-0)/6),0,1,lower.tail = TRUE)
    v2_DrivGender_trun_norm_training02[j]=qnorm(ptruncnorm(data_training_sev_subset02$M_trun_norm_training[j],a=0,b=prob_DrivGender$PR_DrivGender[1],mean =((prob_DrivGender$PR_DrivGender[1])+0)/2,sd=((prob_DrivGender$PR_DrivGender[1]-0)/6)),0,1,lower.tail = TRUE)
    v3_VehAge_training02[j]=qnorm(pnorm(data_training_sev_subset02$VehAge[j],mean=6.9964,sd=3.9417),0,1,lower.tail = TRUE)
    v4_VehGas_training02[j]=qnorm(ptruncnorm(data_training_sev_subset02$regular_trun_norm_training[j],a=(prob_VehGas$PR_VehGas[1]),b=(prob_VehGas$PR_VehGas[1]+prob_VehGas$PR_VehGas[2]),mean =((prob_VehGas$PR_VehGas[1]+prob_VehGas$PR_VehGas[2])+(prob_VehGas$PR_VehGas[1]))/2,sd=((prob_VehGas$PR_VehGas[1]+prob_VehGas$PR_VehGas[2])-(prob_VehGas$PR_VehGas[1]))/6))
    # matrix baris dari v*  
    v_mat_windscreen_training02[j,1]<-v1_cheapest_trun_norm_training02[j]
    v_mat_windscreen_training02[j,2]<-v2_DrivGender_trun_norm_training02[j]
    v_mat_windscreen_training02[j,3]<-v3_VehAge_training02[j]
    v_mat_windscreen_training02[j,4]<-v4_VehGas_training02[j]
    vmat_windscreen_training02=t(v_mat_windscreen_training02)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_windscreen  
    a_windscreen_training02= 1-t(r_windscreen_training1)%*%inv(R2_windscreen_training1)%*%r_windscreen_training1
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
    b_windscreen_training02[j]= t(r_windscreen_training1)%*%inv(R2_windscreen_training1)%*%(vmat_windscreen_training02[,j])
    
    # pdf(y) yang digunakan disini adalah normal 
    random=rnorm(nrow(data_training_sev_subset02),mean = 280.87 ,sd=434.07)
    
    y_windscreen_training02[j]=qnorm(pnorm(data_training_sev_subset02$Sev_Windscreen[j],mean = 280.87 ,sd=434.07),0,1)
    
    # q disini merupakan pdf dari y|x1,x
    q_windscreen_training02[j]=(dnorm(random[j],mean = 280.87 ,sd=434.07 ))*exp(-0.5*(((y_windscreen_training02[j]-b_windscreen_training02[j])^2/a_windscreen_training02)-(y_windscreen_training02[j])^2))*(a_windscreen_training02)^(-0.5)
    
    pl_training02[j]=0.9999*dnorm(random[j],mean = 300 ,sd=450)
    u=runif(1,0)
    if(u<q_windscreen_training02[j]/pl_training02[j]){
      y_simull_training02 = c(y_simull_training02,random)
      mean_training02[j]=mean(y_simull_training02)
      sampel_yang_diterima = sampel_yang_diterima + 1
    }
    total_sampel = total_sampel + 1
  }
  acceptance_rates02[j] <- sampel_yang_diterima / total_sampel
}

sum(acceptance_rates02==1)/nrow(data_training_sev_subset02)
mean_training02

plot(density(pl_training02),col="blue")
lines(density(q_windscreen_training02),col="red")

plot(density(mean_training02))

#uji normalitas 
shapiro.test(mean_training02)

# 677 dihapus dan di fit ulang 
# data ke-551
y_windscreen_training02=NULL
for(j in 1:nrow(data_training_sev_subset02)){
  y_windscreen_training02[j]=qnorm(pnorm(data_training_sev_subset02$Sev_Windscreen[j],mean =271.8 ,sd=174.88),0,1)
  }
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# inverse gaussian dan truncated normal
# yang signifikan pada model GLM adalah vehclass cheaper and cheapest 
u_training_windscreen_inv_gaussian=pobs(data_training_sev_subset[c(21,31,33)])
fit.copula_training_windcreen=fitCopula(normalCopula(dim = 3,dispstr = "un"),u_training_windscreen_inv_gaussian,method="ml")
summary(fit.copula_training_windcreen) 

R_windscreen_training_inv_gaussian=matrix(1,nrow=3,ncol=3)

R_windscreen_training_inv_gaussian[1,2]=fit.copula_training_windcreen@estimate[[1]]
R_windscreen_training_inv_gaussian[1,3]=fit.copula_training_windcreen@estimate[[2]]
R_windscreen_training_inv_gaussian[2,3]=fit.copula_training_windcreen@estimate[[3]]

segitiga_bawah <- lower.tri(R_windscreen_training_inv_gaussian,diag = TRUE)
R_windscreen_training_inv_gaussian[segitiga_bawah]=0
t_R_windscreen=t(R_windscreen_training_inv_gaussian)
R_windscreen_training_inv_gaussian=R_windscreen_training_inv_gaussian+t_R_windscreen
diag(R_windscreen_training_inv_gaussian)=1

R2_windscreen_training_inv_gaussian=R_windscreen_training_inv_gaussian[1:2,1:2]
r_windscreen_training_inv_gaussian=R_windscreen_training_inv_gaussian[1:2,3]

y_simull_training=NULL
mean_training_inv_gaussian=NULL
y_windscreen_training_inv_gaussian=NULL
pl_training_inv_gaussian=NULL
q_windscreen_training_inv_gaussian=NULL
v1_cheapest_trun_norm_training=NULL
v2_cheaper_trun_norm_training=NULL
b_windscreen_training_inv_gaussian=NULL
random_training=NULL
v_mat_windscreen_training=matrix(NA,ncol=2,nrow = nrow(data_training_sev_subset))
acceptance_rates_windscreen=NULL
set.seed(100)
for(j in 1:nrow(data_training_sev_subset)){
  y_simull_training=NULL
  sampel_yang_diterima = 0 
  total_sampel = 0 
 for (i in 1:100){

  #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
  v1_cheapest_trun_norm_training[j]=qnorm(ptruncnorm(data_training_sev_subset$cheapest_trun_norm_training[j],a=0,b=prob_windscreen$PR_windscreen[1],mean =(prob_windscreen$PR_windscreen[1]+0)/2,sd=(prob_windscreen$PR_windscreen[1]-0)/6),0,1,lower.tail = TRUE)
  v2_cheaper_trun_norm_training[j]=qnorm(ptruncnorm(data_training_sev_subset$cheaper_trun_norm_training[j],a=(prob_windscreen$PR_windscreen[1]),b=(prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]),mean =((prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2])+(prob_windscreen$PR_windscreen[1]))/2,sd=((prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2])-(prob_windscreen$PR_windscreen[1]))/6),0,1,lower.tail = TRUE)
  
  # matrix baris dari v*  
  v_mat_windscreen_training[j,1]<-v1_cheapest_trun_norm_training[j]
  v_mat_windscreen_training[j,2]<-v2_cheaper_trun_norm_training[j]
  vmat_windscreen_training=t(v_mat_windscreen_training)
  
  
  # misalkan 1-r^t*R(n-1)^-1 r  adalah a_windscreen  
  a_windscreen_training_inv_gaussian= 1-t(r_windscreen_training_inv_gaussian)%*%inv(R2_windscreen_training_inv_gaussian)%*%r_windscreen_training_inv_gaussian
  
  #misalkan t(r)*inverse(R_(n-1))* v* adalah b
  
  b_windscreen_training_inv_gaussian[j]= t(r_windscreen_training_inv_gaussian)%*%inv(R2_windscreen_training_inv_gaussian)%*%(vmat_windscreen_training[,j])
  
  # pdf(y) yang digunakan disini adalah inverse gaussian 
  random_inv_gaussian=rinvgauss(nrow(data_training_sev_subset),mean = 280.87,shape = 117.59)
  
  y_windscreen_training_inv_gaussian[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean = 280.87,shape = 117.59),0,1)
  
  # q disini merupakan pdf dari y|x1,x
  q_windscreen_training_inv_gaussian[j]=(dinvgauss(random_inv_gaussian[j],mean = 280.87,shape = 117.59 ))*exp(-0.5*(((y_windscreen_training_inv_gaussian[j]-b_windscreen_training_inv_gaussian[j])^2/a_windscreen_training_inv_gaussian)-(y_windscreen_training_inv_gaussian[j])^2))*(a_windscreen_training_inv_gaussian)^(-0.5)
 
   # distribusi proposal 
  pl_training_inv_gaussian[j]=1*dinvgauss(random_inv_gaussian[j],mean = 320,shape = 110)
  
  u=runif(1,0)
  if(u<q_windscreen_training_inv_gaussian[j]/pl_training_inv_gaussian[j]){
    y_simull_training = c(y_simull_training,random_inv_gaussian)
    mean_training_inv_gaussian[j]=mean(y_simull_training)
    sampel_yang_diterima = sampel_yang_diterima + 1
  }
  total_sampel = total_sampel + 1
  }
  acceptance_rates_windscreen[j] <- sampel_yang_diterima / total_sampel
}
sum(acceptance_rates_windscreen==1)/nrow(data_training_sev_subset)
plot(density(pl_training_inv_gaussian),col="blue",main="Plot Densitas Distribusi Target VS Proposal \n Inverse Gaussian-Truncated Gaussian Windscreen")
lines(density(q_windscreen_training_inv_gaussian),col="red")

plot(density(mean_training_inv_gaussian))
# cek normality 
#null : normally distributed
shapiro.test(mean_training_inv_gaussian) # H0 diterima

#copula testing dengan menggunakan parameter pada data training dengan y=inverse.gaussian
# parameer a,b,mean, dan sd yang digunakan menggunakan parameter pada data training 

v1_cheapest_trun_norm_testing=NULL
v2_cheaper_trun_norm_testing=NULL
v_mat_windscreen_testing=matrix(NA,ncol=2,nrow = nrow(data_testing_sev_subset))
b_windscreen_testing_inv_gaussian=NULL
y_windscreen_testing_inv_gaussian=NULL
q_windscreen_testing_inv_gaussian=NULL
y_simull_testing=NULL
mean_testing_inv_gaussian=NULL
pl_testing_inv_gaussian=NULL
set.seed(100)
for(j in 1:nrow(data_testing_sev_subset)){
  y_simull_testing=NULL
   for (i in 1:450){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_cheapest_trun_norm_testing[j]=qnorm(ptruncnorm(data_testing_sev_subset01$cheapest_trun_norm_testing[j],a=0,b=prob_windscreen$PR_windscreen[1],mean =(prob_windscreen$PR_windscreen[1]+0)/2,sd=(prob_windscreen$PR_windscreen[1]-0)/6),0,1,lower.tail = TRUE)
    v2_cheaper_trun_norm_testing[j]=qnorm(ptruncnorm(data_testing_sev_subset01$cheaper_trun_norm_testing[j],a=(prob_windscreen$PR_windscreen[1]),b=(prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2]),mean =((prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2])+(prob_windscreen$PR_windscreen[1]))/2,sd=((prob_windscreen$PR_windscreen[1]+prob_windscreen$PR_windscreen[2])-(prob_windscreen$PR_windscreen[1]))/6),0,1,lower.tail = TRUE)

    # matrix baris dari v*  
    v_mat_windscreen_testing[j,1]<-v1_cheapest_trun_norm_testing[j]
    v_mat_windscreen_testing[j,2]<-v2_cheaper_trun_norm_testing[j]
    vmat_windscreen_testing=t(v_mat_windscreen_testing)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_windscreen  
    a_windscreen_testing_inv_gaussian= 1-t(r_windscreen_training_inv_gaussian)%*%inv(R2_windscreen_training_inv_gaussian)%*%r_windscreen_training_inv_gaussian
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
    
    b_windscreen_testing_inv_gaussian[j]= t(r_windscreen_training_inv_gaussian)%*%inv(R2_windscreen_training_inv_gaussian)%*%(vmat_windscreen_testing[,j])
    
    random_inv_gaussian=rinvgauss(nrow(data_testing_sev_subset01),mean = 280.87,shape = 117.59)
    
    y_windscreen_testing_inv_gaussian[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean = 280.87,shape = 117.59),0,1)
    
    # q disini merupakan pdf dari y|x1,x
    q_windscreen_testing_inv_gaussian[j]=(dinvgauss(random_inv_gaussian[j],mean = 280.87,shape = 117.59 ))*exp(-0.5*(((y_windscreen_testing_inv_gaussian[j]-b_windscreen_testing_inv_gaussian[j])^2/a_windscreen_testing_inv_gaussian)-(y_windscreen_testing_inv_gaussian[j])^2))*(a_windscreen_testing_inv_gaussian)^(-0.5)
    
    # distribusi proposal 
    pl_testing_inv_gaussian[j]=1*dinvgauss(random_inv_gaussian[j],mean = 320,shape = 110)
    
    u=runif(1,0)
    if(u<q_windscreen_testing_inv_gaussian[j]/pl_testing_inv_gaussian[j]){
      y_simull_testing= c(y_simull_testing,random_inv_gaussian)
      mean_testing_inv_gaussian[j]=mean(y_simull_testing)
    }
  }
}  
mean_testing_inv_gaussian
plot(density(mean_testing_inv_gaussian),main="Plot Densitas Prediksi Copula \n Inverse Gaussian-Truncated Gaussian")

# uji normalitas 
shapiro.test(mean_testing_inv_gaussian)

MSE_copula_Windscreen_testing_inv_gaussian=sum((data_testing_sev_subset$Sev_Windscreen-mean_testing_inv_gaussian)^2)/nrow(data_testing_sev_subset)
MSE_copula_Windscreen_testing_inv_gaussian
#_________________________________________________________________________________________________________________________________________________________________
# inverse gaussian dan beta 
u_training_windscreen_beta=pobs(data_training_sev_subset01[c(21,28,30)])
fit.copula_training_windcreen_beta=fitCopula(normalCopula(dim = 3,dispstr = "un"),u_training_windscreen_beta,method="ml")
summary(fit.copula_training_windcreen_beta) 

R_windscreen_training_inv_gaussian_beta=matrix(1,nrow=3,ncol=3)

R_windscreen_training_inv_gaussian_beta[1,2]=fit.copula_training_windcreen_beta@estimate[[1]]
R_windscreen_training_inv_gaussian_beta[1,3]=fit.copula_training_windcreen_beta@estimate[[2]]
R_windscreen_training_inv_gaussian_beta[2,3]=fit.copula_training_windcreen_beta@estimate[[3]]

segitiga_bawah <- lower.tri(R_windscreen_training_inv_gaussian_beta,diag = TRUE)
R_windscreen_training_inv_gaussian_beta[segitiga_bawah]=0
t_R_windscreen_beta=t(R_windscreen_training_inv_gaussian_beta)
R_windscreen_training_inv_gaussian_beta=R_windscreen_training_inv_gaussian_beta+t_R_windscreen_beta
diag(R_windscreen_training_inv_gaussian_beta)=1

R2_windscreen_training_inv_gaussian_beta=R_windscreen_training_inv_gaussian_beta[1:2,1:2]
r_windscreen_training_inv_gaussian_beta=R_windscreen_training_inv_gaussian_beta[1:2,3]

v1_cheapest_beta_training =NULL
v2_cheaper_beta_training=NULL
v_mat_windscreen_training_beta=matrix(NA,ncol=2,nrow = nrow(data_training_sev_subset))
b_windscreen_training_inv_gaussian_beta=NULL
q_windscreen_training_inv_gaussian_beta=NULL
pl_training_inv_gaussian_beta=NULL
y_simull_training_inv_gaussian_beta=NULL
mean_training_inv_gaussian_beta=NULL
acceptance_rates_beta=NULL
set.seed(100)
for(j in 1:nrow(data_training_sev_subset)){
y_simull_training_inv_gaussian_beta=NULL
sampel_yang_diterima = 0 
total_sampel = 0 
  for (i in 1:100){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_cheapest_beta_training[j]=qnorm(pbeta(data_training_sev_subset$cheapest_beta_training[j],shape1 = alpha_cheapest_beta,shape2 = beta_cheapest_beta),0,1,lower.tail = TRUE)
    v2_cheaper_beta_training[j]=qnorm(pbeta(data_training_sev_subset$cheaper_beta_training[j],shape1 = alpha_cheaper_beta,shape2 = beta_cheaper_beta),0,1,lower.tail = TRUE)
    
    # matrix baris dari v*  
    v_mat_windscreen_training_beta[j,1]<-v1_cheapest_beta_training[j]
    v_mat_windscreen_training_beta[j,2]<-v2_cheaper_beta_training[j]
    vmat_windscreen_training_beta=t(v_mat_windscreen_training_beta)
    
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_windscreen  
    a_windscreen_training_inv_gaussian= 1-t(r_windscreen_training_inv_gaussian_beta)%*%inv(R2_windscreen_training_inv_gaussian_beta)%*%r_windscreen_training_inv_gaussian_beta
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
    
    b_windscreen_training_inv_gaussian_beta[j]= t(r_windscreen_training_inv_gaussian_beta)%*%inv(R2_windscreen_training_inv_gaussian_beta)%*%(vmat_windscreen_training_beta[,j])
    
    # pdf(y) yang digunakan disini adalah inverse gaussian 
    random_inv_gaussian=rinvgauss(nrow(data_training_sev_subset),mean = 280.87,shape = 117.59)
    
    y_windscreen_training_inv_gaussian[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean = 280.87,shape = 117.59),0,1)
    
    # q disini merupakan pdf dari y|x1,x
    q_windscreen_training_inv_gaussian_beta[j]=(dinvgauss(random_inv_gaussian[j],mean = 280.87,shape = 117.59 ))*exp(-0.5*(((y_windscreen_training_inv_gaussian[j]-b_windscreen_training_inv_gaussian_beta[j])^2/a_windscreen_training_inv_gaussian)-(y_windscreen_training_inv_gaussian[j])^2))*(a_windscreen_training_inv_gaussian)^(-0.5)
    
    # distribusi proposal 
    pl_training_inv_gaussian_beta[j]=0.99*dinvgauss(random_inv_gaussian[j],mean = 710,shape=84)
    
   u=runif(1,0)
    if(u<q_windscreen_training_inv_gaussian_beta[j]/pl_training_inv_gaussian_beta[j]){
      y_simull_training_inv_gaussian_beta = c(y_simull_training_inv_gaussian_beta,random_inv_gaussian)
      mean_training_inv_gaussian_beta[j]=mean(y_simull_training_inv_gaussian_beta)
      sampel_yang_diterima = sampel_yang_diterima + 1
   }
   total_sampel = total_sampel + 1
  }
  acceptance_rates_beta[j] <- sampel_yang_diterima / total_sampel
  
}
mean_training_inv_gaussian_beta
sum(acceptance_rates_beta==1)/nrow(data_training_sev_subset)

plot(density(pl_training_inv_gaussian_beta),col="blue",main="Plot Densitas Distribusi Target VS Proposal \n Inverse Gaussian-Beta Windscreen")
lines(density(q_windscreen_training_inv_gaussian_beta),col="red")

# plot density dari training 
plot(density(mean_training_inv_gaussian_beta))

# uji normalitas 
shapiro.test(mean_training_inv_gaussian_beta)# H0 diterima

# model testing inverse gaussian dan beta 

#copula testing dengan menggunakan parameter pada data training dengan y=inverse.gaussian
set.seed(105)
cheapest_beta_testing=rbeta(nrow(data_testing_sev_subset),shape1 = alpha_cheapest_beta,shape2 = beta_cheapest_beta)
set.seed(106)
cheaper_beta_testing= rbeta(nrow(data_testing_sev_subset),shape1 = alpha_cheaper_beta,shape2 = beta_cheapest_beta)
set.seed(107)
other_beta_testing=rbeta(nrow(data_testing_sev_subset),shape1 = alpha_other_beta,shape2 = beta_other_beta)

data_testing_sev_subset=cbind(data_testing_sev_subset,cheapest_beta_testing,cheaper_beta_testing,other_beta_testing)

v1_cheapest_beta_testing=NULL
v2_cheaper_beta_testing=NULL
v_mat_windscreen_testing_beta=matrix(NA,ncol=2,nrow = nrow(data_testing_sev_subset))
b_windscreen_testing_inv_gaussian_beta=NULL
y_windscreen_testing_inv_gaussian_beta=NULL
q_windscreen_testing_inv_gaussian_beta=NULL
y_simull_testing_beta=NULL
pl_testing_inv_gaussian_beta=NULL
mean_testing_inv_gaussian_beta=NULL
set.seed(100)
for(j in 1:nrow(data_testing_sev_subset)){
  y_simull_testing_beta=NULL
  for (i in 1:500){
   #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_cheapest_beta_testing[j]=qnorm(pbeta(data_testing_sev_subset$cheapest_beta_testing[j],shape1 = alpha_cheapest_beta,shape2 = beta_cheapest_beta),0,1,lower.tail = TRUE)
    v2_cheaper_beta_testing[j]=qnorm(pbeta(data_testing_sev_subset$cheaper_beta_testing[j],shape1 = alpha_cheaper_beta,shape2 = beta_cheaper_beta),0,1,lower.tail = TRUE)
    
    # matrix baris dari v*  
    v_mat_windscreen_testing_beta[j,1]<-v1_cheapest_beta_testing[j]
    v_mat_windscreen_testing_beta[j,2]<-v2_cheaper_beta_testing[j]
    vmat_windscreen_testing_beta=t(v_mat_windscreen_testing_beta)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_windscreen  
    a_windscreen_testing_inv_gaussian= 1-t(r_windscreen_training_inv_gaussian)%*%inv(R2_windscreen_training_inv_gaussian)%*%r_windscreen_training_inv_gaussian
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
    
    b_windscreen_testing_inv_gaussian_beta[j]= t(r_windscreen_training_inv_gaussian)%*%inv(R2_windscreen_training_inv_gaussian)%*%(vmat_windscreen_testing_beta[,j])
    
    random_inv_gaussian=rinvgauss(nrow(data_testing_sev_subset),mean = 280.87,shape = 117.59)
    
    y_windscreen_testing_inv_gaussian[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean = 280.87,shape = 117.59),0,1)
    
    # q disini merupakan pdf dari y|x1,x
    q_windscreen_testing_inv_gaussian_beta[j]=(dinvgauss(random_inv_gaussian[j],mean = 280.87,shape = 117.59 ))*exp(-0.5*(((y_windscreen_testing_inv_gaussian[j]-b_windscreen_testing_inv_gaussian_beta[j])^2/a_windscreen_testing_inv_gaussian)-(y_windscreen_testing_inv_gaussian[j])^2))*(a_windscreen_testing_inv_gaussian)^(-0.5)
    
    # distribusi proposal 
    pl_testing_inv_gaussian_beta[j]=0.99*dinvgauss(random_inv_gaussian[j],mean = 710,shape=84)
    
    
    u=runif(1,0)
    if(u<q_windscreen_testing_inv_gaussian_beta[j]/ pl_testing_inv_gaussian_beta[j]){
      y_simull_testing_beta= c( y_simull_testing_beta,random_inv_gaussian)
      mean_testing_inv_gaussian_beta[j]=mean(y_simull_testing_beta)
    }
  }
}  
mean_testing_inv_gaussian_beta
plot(density(mean_testing_inv_gaussian_beta))
# uji normalitas 
shapiro.test(mean_testing_inv_gaussian_beta)

MSE_copula_Windscreen_testing_inv_gaussian_beta=sum((data_testing_sev_subset$Sev_Windscreen-mean_testing_inv_gaussian_beta)^2)/nrow(data_testing_sev_subset)
MSE_copula_Windscreen_testing_inv_gaussian_beta

#_______________________________________________________________________________________________________________________________________________________________________________


# Model frekuensi 

#Model frekuensi untuk Sev_Windscreen 

mod1_freq_pois=glm(Windscreen~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = poisson(log),data = data_training)
summary(mod1_freq_pois)
mod2_freq_pois=stepAIC(mod1_freq_pois,direction = "both")
summary(mod2_freq_pois)

#predict
y_hat_freq_Poisson=predict(mod2_freq_pois,data_testing_sev_subset, type = "response")

#Model pure premium 
pure_prem_GammaPoisson=y_hat_Windscreen_Gamma*y_hat_freq_Poisson
pure_prem_GaussianPoisson=y_hat_Windscreen_Gaussian*y_hat_freq_Poisson
pure_prem_InverseGaussianPoisson=y_hat_Windscreen_Inverse_Gaussian*y_hat_freq_Poisson
pure_prem_GaussianPoisson_log=y_hat_Windscreen_Gaussian_log*y_hat_freq_Poisson

pure_prem_InverseGaussianPoisson_copula=mean_testing_inv_gaussian*y_hat_freq_Poisson
pure_prem_InverseGaussianPoisson_copula_beta=mean_testing_inv_gaussian_beta*y_hat_freq_Poisson

pure_prem_GaussianPoisson_copula=mean_testing01*y_hat_freq_Poisson
pure_prem_GaussianPoisson_copula_beta=mean_testing_beta01*y_hat_freq_Poisson

#MSE
S_windscreen=data_testing_sev_subset$Sev_Windscreen*data_testing_sev_subset$Windscreen
MSE_Windscreen_GammaPoisson=sum((S_windscreen-pure_prem_GammaPoisson)^2)/nrow(data_testing_sev_subset)
MSE_Windscreen_GammaPoisson

MSE_WIndscreen_GaussianPoisson=sum((S_windscreen-pure_prem_GaussianPoisson)^2)/nrow(data_testing_sev_subset)
MSE_WIndscreen_GaussianPoisson

MSE_WIndscreen_GaussianPoisson_log=sum((S_windscreen-pure_prem_GaussianPoisson_log)^2)/nrow(data_testing_sev_subset)
MSE_WIndscreen_GaussianPoisson_log

MSE_Windscreen_InverseGaussianPoisson=sum((S_windscreen-pure_prem_InverseGaussianPoisson)^2)/nrow(data_testing_sev_subset)
MSE_Windscreen_InverseGaussianPoisson

MSE_Windscreen_InverseGaussianPoisson_copula=sum((S_windscreen-pure_prem_InverseGaussianPoisson_copula)^2)/nrow(data_testing_sev_subset)
MSE_Windscreen_InverseGaussianPoisson_copula

MSE_Windscreen_InverseGaussianPoisson_copula_beta=sum((S_windscreen-pure_prem_InverseGaussianPoisson_copula_beta)^2)/nrow(data_testing_sev_subset)
MSE_Windscreen_InverseGaussianPoisson_copula_beta

MSE_Windscreen_GaussianPoisson_copula=sum((S_windscreen-pure_prem_GaussianPoisson_copula)^2)/nrow(data_testing_sev_subset)
MSE_Windscreen_GaussianPoisson_copula

MSE_Windscreen_GaussianPoisson_copula_beta=sum((S_windscreen-pure_prem_GaussianPoisson_copula_beta)^2)/nrow(data_testing_sev_subset)
MSE_Windscreen_GaussianPoisson_copula_beta

# Boxplot
Sev_windscreen_testing=data.frame(cbind(data_testing_sev_subset$Sev_Windscreen,y_hat_Windscreen_Gamma,y_hat_Windscreen_Gaussian,y_hat_Windscreen_Gaussian_log
                             ,y_hat_Windscreen_Inverse_Gaussian,mean_testing01,mean_testing_beta01,mean_testing_inv_gaussian
                             ,mean_testing_inv_gaussian_beta))
colnames(Sev_windscreen_testing)<-c("sev_asli","yhat_Gamma","yhat_Gaussian","yhat_Gaussian_log","yhat_invGauss", 
                                    "yhat_Normal-trunGauss-Gamma","yhat_Normal-beta-Gamma","yhat_invGauss-trunNorm", "yhat_invGauss-Beta")
df_windscreen <- tidyr::gather(Sev_windscreen_testing, key = "Model", value = "Predicted_Values")
mean_values <- aggregate(Predicted_Values ~ Model, data = df_windscreen, FUN = mean)

ggplot(df_windscreen, aes(x = Model, y = Predicted_Values)) +
  geom_boxplot()+
  geom_point(data = mean_values, aes(x = Model, y = Predicted_Values), col = "gray", pch = 19)+
  geom_text(data = mean_values, aes(x = Model, y = Predicted_Values, label = round(Predicted_Values, 2)),
            vjust = -0.5, hjust = 0.5, size = 3, col = "black") +
  labs(title = "Boxplot Model Besar Klaim Windscreen", 
       x = "Models", y = "Predicted Values") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Boxplot premi murni 
premi_murni_windscreen=data.frame(cbind(S_windscreen,pure_prem_GammaPoisson,pure_prem_GaussianPoisson,pure_prem_GaussianPoisson_log
                                        ,pure_prem_InverseGaussianPoisson,pure_prem_GaussianPoisson_copula,pure_prem_GaussianPoisson_copula_beta
                                        ,pure_prem_InverseGaussianPoisson_copula,pure_prem_InverseGaussianPoisson_copula_beta))
colnames(premi_murni_windscreen) <- c("Data", "Pois & Gamma", "Pois & Normal", "Pois & Normal_log",
                                      "Pois & InvGauss", "Poiss & InvGauss-\ntrunGauss-Normal",
                                      "Pois & InvGauss-\nbeta-Normal", "Pois & invGauss-\ntrunGauss",
                                      "Pois & invGauss-\nbeta")
df_premi_murni_windscreen <- tidyr::gather(premi_murni_windscreen, key = "Model", value = "Predicted_Values")
df_premi_murni_windscreen$Model <- factor(df_premi_murni_windscreen$Model, levels = colnames(premi_murni_windscreen))
mean_premi_murni_windscreen <- aggregate(Predicted_Values ~ Model, data = df_premi_murni_windscreen, FUN = mean)

ggplot(df_premi_murni_windscreen, aes(x = Model, y = Predicted_Values)) +
  geom_boxplot()+
  geom_point(data = mean_premi_murni_windscreen, aes(x = Model, y = Predicted_Values), col = "gray", pch = 19)+
  geom_text(data = mean_premi_murni_windscreen, aes(x = Model, y = Predicted_Values, label = round(Predicted_Values, 2)),
            vjust = -0.5, hjust = 0.5, size = 3, col = "black")+
  labs(title = "Boxplot Premi Murni Windscreen", 
       x = "Models", y = "Predicted Values") +
  scale_y_continuous(expand = c(0.1, 0))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

###########################################################################################################################################

# Model Severity untuk TPL 
data_training_sev_subset_TPL=subset(data_training,data_training$Sev_TPL>0)
data_testing_sev_subset_TPL=data_testing
summary(data_training_sev_subset_TPL)
set.seed(100)

mod1_TPL=glm(Sev_TPL~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Area+Channel,family = Gamma(link="log"),data = data_training_sev_subset_TPL)
# Garage Region  
summary(mod1_TPL)
step_mod1_TPL=stepAIC(mod1_TPL,direction = "both",trace = TRUE)
summary(step_mod1_TPL)

mod2_TPL=glm(Sev_TPL~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = gaussian(link="identity"),data = data_training_sev_subset_TPL)
summary(mod2_TPL)
step_mod2_TPL=stepAIC(mod2_TPL, direction = "both")
summary(step_mod2_TPL)

mod2_TPL_log=glm(Sev_TPL~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehGas+Area+Channel,family = gaussian(link="log"),data = data_training_sev_subset_TPL)
summary(mod2_TPL_log) #(-) Garage,Region,VehPower
step_mod2_TPL_log=stepAIC(mod2_TPL_log, direction = "both")
summary(step_mod2_TPL_log)

mod3_TPL=glm(Sev_TPL~DrivAge,family= inverse.gaussian(link="log"),data = data_training_sev_subset_TPL)
summary(mod3_TPL) 
step_mod3_TPL=stepAIC(mod3_TPL, direction = "both")
summary(step_mod3_TPL)


#Predict 

y_hat_TPL_Gamma=predict(step_mod1_TPL,data_testing_sev_subset_TPL,type = "response")
MSE_TPL_Gamma=sum((data_testing_sev_subset_TPL$Sev_TPL-y_hat_TPL_Gamma)^2)/nrow(data_testing_sev_subset_TPL)
MSE_TPL_Gamma

y_hat_TPL_Gaussian=predict(step_mod2_TPL,data_testing_sev_subset_TPL,type = "response")
MSE_TPL_Gaussian=sum((data_testing_sev_subset_TPL$Sev_TPL-y_hat_TPL_Gaussian)^2)/nrow(data_testing_sev_subset_TPL)
MSE_TPL_Gaussian

y_hat_TPL_Gaussian_log=predict(step_mod2_TPL_log,data_testing_sev_subset_TPL,type = "response")
MSE_TPL_Gaussian_log=sum((data_testing_sev_subset_TPL$Sev_TPL-y_hat_TPL_Gaussian_log)^2)/nrow(data_testing_sev_subset_TPL)
MSE_TPL_Gaussian_log

y_hat_TPL_Inverse_Gaussian=predict(step_mod3_TPL,data_testing_sev_subset_TPL,type = "response")
MSE_TPL_InverseGaussian=sum((data_testing_sev_subset_TPL$Sev_TPL-y_hat_TPL_Inverse_Gaussian)^2)/nrow(data_testing_sev_subset_TPL)
MSE_TPL_InverseGaussian


#................................................................................................................................................

# sev TPL dan DrivAge 
u_training_TPL=pobs(data_training_sev_subset_TPL[c(22,12)])
fit.copula_training_TPL=fitCopula(normalCopula(dim = 2,dispstr = "un"),u_training_TPL,method="ml")
summary(fit.copula_training_TPL) 

#matriks R_training

R_TPL_training_inv_gaussian=matrix(1,nrow=2,ncol=2)
R_TPL_training_inv_gaussian[1,2]=fit.copula_training_TPL@estimate[[1]]
R_TPL_training_inv_gaussian[2,1]=fit.copula_training_TPL@estimate[[1]]

R2_TPL_training_inv_gaussian=R_TPL_training_inv_gaussian[1:1,1:1]
r_TPL_training=R_TPL_training_inv_gaussian[1:1,2]

y_simull_training_TPL=NULL
mean_training_inv_gaussian_TPL=NULL
y_TPL_training_inv_gaussian=NULL
pl_training_inv_gaussian=NULL
q_TPL_training_inv_gaussian=NULL
v1_DrivAge=NULL
b_TPL_training_inv_gaussian=NULL
random_training=NULL
v_mat_TPL_training=matrix(NA,ncol=1,nrow = nrow(data_training_sev_subset_TPL))
acceptance_rates_TPL=NULL
set.seed(100)
for(j in 1:nrow(data_training_sev_subset_TPL)){
  y_simull_training_TPL=NULL
  sampel_yang_diterima = 0 
  total_sampel = 0 
  for (i in 1:100){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_DrivAge[j]=qnorm(pgamma(data_training_sev_subset_TPL$DrivAge[j],shape = 11.404,scale = 3.4705),0,1,lower.tail = TRUE)
   
    # matrix baris dari v*  
    v_mat_TPL_training[j,1]<-v1_DrivAge[j]
    vmat_TPL_training=t(v_mat_TPL_training)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_windscreen  
    a_TPL_training_inv_gaussian= 1-t(r_TPL_training)%*%(R2_TPL_training_inv_gaussian)%*%r_TPL_training
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
    b_TPL_training_inv_gaussian[j]= t(r_TPL_training)%*%(R2_TPL_training_inv_gaussian)%*%(vmat_TPL_training[,j])
    
    # pdf(y) yang digunakan disini adalah inverse gaussian 
    random_inv_gaussian=rinvgauss(nrow(data_training_sev_subset_TPL),mean = 1558.1,shape = 58.432)
    
    y_TPL_training_inv_gaussian[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean = 1558.1,shape = 58.432),0,1)

    # q disini merupakan pdf dari y|x1,x
    q_TPL_training_inv_gaussian[j]=(dinvgauss(random_inv_gaussian[j],mean = 1558.1,shape = 58.432 ))*exp(-0.5*(((y_TPL_training_inv_gaussian[j]-b_TPL_training_inv_gaussian[j])^2/a_TPL_training_inv_gaussian)-(y_TPL_training_inv_gaussian[j])^2))*(a_TPL_training_inv_gaussian)^(-0.5)
    
    # distribusi proposal 
    pl_training_inv_gaussian[j]=0.94*dinvgauss(random_inv_gaussian[j],mean = 1500,shape =51)
    
    u=runif(1,0)
    if(u< q_TPL_training_inv_gaussian[j]/ pl_training_inv_gaussian[j]){
      y_simull_training_TPL = c(y_simull_training_TPL,random_inv_gaussian)
      mean_training_inv_gaussian_TPL[j]=mean(y_simull_training_TPL)
      sampel_yang_diterima = sampel_yang_diterima + 1
    }
   total_sampel = total_sampel + 1
  }
  acceptance_rates_TPL[j] <- sampel_yang_diterima / total_sampel
  
}

plot(density(q_TPL_training_inv_gaussian),col="red",main="Plot Densitas Distribusi Target VS Proposal \nInverse Gaussian-Gamma TPL")
lines(density(pl_training_inv_gaussian),col="blue")

mean_training_inv_gaussian_TPL

sum(acceptance_rates_TPL==1)/nrow(data_training_sev_subset_TPL)

# model testing 
v1_DrivAge_testing=NULL
v_mat_TPL_testing=matrix(NA,ncol=1,nrow = nrow(data_testing_sev_subset_TPL))
b_TPL_testing_inv_gaussian=NULL
y_TPL_testing_inv_gaussian=NULL
q_TPL_testing_inv_gaussian=NULL
pl_testing_inv_gaussian_TPL=NULL
y_simull_testing_TPL=NULL
mean_testing_inv_gaussian_TPL=NULL
set.seed(100)
for(j in 1:nrow(data_testing_sev_subset_TPL)){
  y_simull_testing_TPL=NULL
  for (i in 1:450){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_DrivAge_testing[j]=  qnorm(pgamma(data_testing_sev_subset_TPL$DrivAge[j],shape = 11.404,scale = 3.4705),0,1,lower.tail = TRUE)
    
    # matrix baris dari v*  
    v_mat_TPL_testing[j,1]<-v1_DrivAge_testing[j]
    vmat_TPL_testing=t(v_mat_TPL_testing)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_windscreen  
    a_TPL_testing_inv_gaussian= 1-t(r_TPL_training)%*%(R2_TPL_training_inv_gaussian)%*%r_TPL_training
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
    b_TPL_testing_inv_gaussian[j]=t(r_TPL_training)%*%(R2_TPL_training_inv_gaussian)%*%(vmat_TPL_testing[,j])
    
    random_inv_gaussian=rinvgauss(nrow(data_testing_sev_subset_TPL),mean = 1558.1,shape = 58.432)
    
    y_TPL_testing_inv_gaussian[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean = 1558.1,shape = 58.432),0,1)
    
    # q disini merupakan pdf dari y|x1,x
    q_TPL_testing_inv_gaussian[j]=(dinvgauss(random_inv_gaussian[j],mean = 1558.1,shape = 58.432 ))*exp(-0.5*(((y_TPL_testing_inv_gaussian[j]-b_TPL_testing_inv_gaussian[j])^2/a_TPL_testing_inv_gaussian)-(y_TPL_testing_inv_gaussian[j])^2))*(a_TPL_testing_inv_gaussian)^(-0.5)
    
    # distribusi proposal 
    pl_testing_inv_gaussian_TPL[j]=0.94*dinvgauss(random_inv_gaussian[j],mean = 1500,shape =51)
    
    u=runif(1,0)
    if(u<q_TPL_testing_inv_gaussian[j]/ pl_testing_inv_gaussian_TPL[j]){
      y_simull_testing_TPL= c(y_simull_testing_TPL,random_inv_gaussian)
      mean_testing_inv_gaussian_TPL[j]=mean(y_simull_testing_TPL)
   }
  }
}  
mean_testing_inv_gaussian_TPL
plot(density(mean_testing_inv_gaussian_TPL),main="Plot Densitas Prediksi Copula \n Inverse Gaussian-Gamma TPL")

# uji normalitas 
shapiro.test(mean_testing_inv_gaussian_TPL)

MSE_copula_TPL_testing_inv_gaussian=sum((data_testing_sev_subset_TPL$Sev_TPL-mean_testing_inv_gaussian_TPL)^2)/nrow(data_testing_sev_subset_TPL)
MSE_copula_TPL_testing_inv_gaussian
#................................................................................................................................................

# Model frekuensi TPL
# family: Poisson (link="log")
mod1_freq_pois_TPL=glm(TPL~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = poisson(link="log"),data = data_training)
summary(mod1_freq_pois_TPL)
step_mod1_freq_pois_TPL=stepAIC(mod1_freq_pois_TPL,direction = "both")
summary(step_mod1_freq_pois_TPL)

# Predict 
y_hat_freq_Poisson_TPL=predict(step_mod1_freq_pois_TPL,data_testing_sev_subset_TPL,type = "response")

# pure premium 
pure_prem_GammaPoisson_TPL=y_hat_TPL_Gamma*y_hat_freq_Poisson_TPL
pure_prem_GaussianPoisson_TPL=y_hat_TPL_Gaussian*y_hat_freq_Poisson_TPL
pure_prem_GaussianPoisson_TPL_log=y_hat_TPL_Gaussian_log*y_hat_freq_Poisson_TPL
pure_prem_InverseGaussianPoisson_TPL=y_hat_TPL_Inverse_Gaussian*y_hat_freq_Poisson_TPL
pure_prem_copula_TPL=mean_testing_inv_gaussian_TPL*y_hat_freq_Poisson_TPL

#MSE
S_TPL=data_testing_sev_subset_TPL$Sev_TPL*data_testing_sev_subset_TPL$TPL
MSE_TPL_GammaPoisson=sum((S_TPL-pure_prem_GammaPoisson_TPL)^2)/nrow(data_testing_sev_subset_TPL)
MSE_TPL_GammaPoisson

MSE_TPL_GaussianPoisson=sum((S_TPL-pure_prem_GaussianPoisson_TPL)^2)/nrow(data_testing_sev_subset_TPL)
MSE_TPL_GaussianPoisson

MSE_TPL_GaussianPoisson_log=sum((S_TPL-pure_prem_GaussianPoisson_TPL_log)^2)/nrow(data_testing_sev_subset_TPL)
MSE_TPL_GaussianPoisson_log

MSE_TPL_InverseGaussianPoisson=sum((S_TPL-pure_prem_InverseGaussianPoisson_TPL)^2)/nrow(data_testing_sev_subset_TPL)
MSE_TPL_InverseGaussianPoisson

MSE_TPL_copula=sum((S_TPL-pure_prem_copula_TPL)^2)/nrow(data_testing_sev_subset_TPL)
MSE_TPL_copula

#boxplot 

Sev_TPL_testing=data.frame(cbind(data_testing_sev_subset_TPL$Sev_TPL,y_hat_TPL_Gamma,y_hat_TPL_Gaussian
                      ,y_hat_TPL_Gaussian_log,y_hat_TPL_Inverse_Gaussian,mean_testing_inv_gaussian_TPL))
colnames(Sev_TPL_testing)<-c("data asli","y hat Gamma","y hat Gaussian","y hat Gaussian Log","y hat \nInverse Gaussian","y hat Inverse Gaussian\n-Gamma")
df_TPL <- tidyr::gather(Sev_TPL_testing, key = "Model", value = "Predicted_Values")
mean_values_TPL <- aggregate(Predicted_Values ~ Model, data = df_TPL, FUN = mean)
ggplot(df_TPL, aes(x = Model, y = Predicted_Values)) +
  geom_boxplot()+
  geom_point(data = mean_values_TPL, aes(x = Model, y = Predicted_Values), col = "gray", pch = 19)+
  geom_text(data = mean_values_TPL, aes(x = Model, y = Predicted_Values, label = round(Predicted_Values, 2)),
            vjust = -0.5, hjust = 0.5, size = 3, col = "black") +
  labs(title = "Boxplot Model Besar Klaim TPL", 
       x = "Models", y = "Predicted Values") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+scale_y_continuous(limits =c(0,10000))

# Boxplot premi murni 
premi_murni_TPL=data.frame(cbind(S_TPL,pure_prem_GammaPoisson_TPL,pure_prem_GaussianPoisson_TPL,pure_prem_GaussianPoisson_TPL_log
                                  ,pure_prem_InverseGaussianPoisson_TPL,pure_prem_copula_TPL))
colnames(premi_murni_TPL) <- c("Data", "Pois &\n Gamma", "Pois & \nNormal", "Pois & \nNormal_log","Pois & \nInvGauss", "Poiss & \nInvGauss-Gamma")
df_premi_murni_TPL <- tidyr::gather(premi_murni_TPL, key = "Model", value = "Predicted_Values")
df_premi_murni_TPL$Model <- factor(df_premi_murni_TPL$Model, levels = colnames(premi_murni_TPL))
mean_premi_murni_TPL <- aggregate(Predicted_Values ~ Model, data = df_premi_murni_TPL, FUN = mean)

ggplot(df_premi_murni_TPL, aes(x = Model, y = Predicted_Values)) +
  geom_boxplot()+
  geom_point(data = mean_premi_murni_TPL, aes(x = Model, y = Predicted_Values), col = "gray", pch = 19)+
  geom_text(data = mean_premi_murni_TPL, aes(x = Model, y = Predicted_Values, label = round(Predicted_Values, 2)),
            vjust = -0.5, hjust = 0.5, size = 3, col = "black")+
  labs(title = "Boxplot Premi Murni TPL", 
       x = "Models", y = "Predicted Values") +
  scale_y_continuous(limits = c(0,5000),expand = c(0.1, 0))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


#######################################################################################################################################################

# Model severity untuk Sev_Damage 

data_training_sev_subset_Damage=subset(data_training,data_training$Sev_Damage>0)
data_testing_sev_subset_Damage=data_testing
summary(data_training_sev_subset_Damage)

# Mengubah level data_kategorikal
data_Garage_Damage=mapvalues(data_training_sev_subset_Damage$Garage, from = c("Opened & closed collective parking","Closed zbox","Street")
                                   , to = c("1", "2","3"))
data_Garage_Damage=data.frame(as.factor(data_Garage_Damage))

frequency_table_Garage_Damage <- table(data_Garage_Damage)

PR_Garage_Damage=NULL
for( i in 1:3){ 
  PR_Garage_Damage[i]=frequency_table_Garage_Damage[i]/(frequency_table_Garage_Damage[1]+frequency_table_Garage_Damage[2]+frequency_table_Garage_Damage[3])
}
PR_Garage_Damage

Garage <- c("Opened & closed collective parking","Closed zbox","Street")
Cumprob_Garage_Damage=data.frame(cbind(Garage,PR_Garage_Damage))
Prob_Garage_Damage=Cumprob_Garage_Damage[order(Cumprob_Garage_Damage$PR_Garage_Damage,decreasing = TRUE),]
Prob_Garage_Damage$PR_Garage_Damage=as.numeric(Prob_Garage_Damage$PR_Garage_Damage)

# Truncated Gaussian 

a_closed_zbox=0
b_closed_zbox=Prob_Garage_Damage$PR_Garage_Damage[1]

a_Opened_closed=Prob_Garage_Damage$PR_Garage_Damage[1]
b_Opened_closed=Prob_Garage_Damage$PR_Garage_Damage[1]+Prob_Garage_Damage$PR_Garage_Damage[2]

a_Street=Prob_Garage_Damage$PR_Garage_Damage[1]+Prob_Garage_Damage$PR_Garage_Damage[2]
b_Street=Prob_Garage_Damage$PR_Garage_Damage[1]+Prob_Garage_Damage$PR_Garage_Damage[2]+Prob_Garage_Damage$PR_Garage_Damage[3]
set.seed(105)
closed_zbox_trun_norm_training=rtruncnorm(nrow(data_training_sev_subset_Damage),a=a_closed_zbox,b=b_closed_zbox,mean = (a_closed_zbox+b_closed_zbox)/2,sd=(b_closed_zbox-a_closed_zbox)/6)
set.seed(106)
opened_closed_trun_norm_training= rtruncnorm(nrow(data_training_sev_subset_Damage),a=a_Opened_closed,b=b_Opened_closed,mean=(a_Opened_closed+b_Opened_closed)/2,sd=(b_Opened_closed-a_Opened_closed)/6)
set.seed(107)
street_trun_norm_training=rtruncnorm(nrow(data_training_sev_subset_Damage),a=a_Street,b=b_Street,mean = (a_Street+b_Street)/2,sd=(b_Street-a_Street)/6)

# beta 
alpha_closed_zbox=Prob_Garage_Damage$PR_Garage_Damage[1]*(((Prob_Garage_Damage$PR_Garage_Damage[1]*(1-Prob_Garage_Damage$PR_Garage_Damage[1]))/var(Prob_Garage_Damage$PR_Garage_Damage))-1)
beta_closed_zbox=(1-Prob_Garage_Damage$PR_Garage_Damage[1])*(((Prob_Garage_Damage$PR_Garage_Damage[1]*(1-Prob_Garage_Damage$PR_Garage_Damage[1]))/var(Prob_Garage_Damage$PR_Garage_Damage))-1)

alpha_Opened_closed=Prob_Garage_Damage$PR_Garage_Damage[2]*(((Prob_Garage_Damage$PR_Garage_Damage[2]*(1-Prob_Garage_Damage$PR_Garage_Damage[2]))/var(Prob_Garage_Damage$PR_Garage_Damage))-1)
beta_Opened_closed=(1-Prob_Garage_Damage$PR_Garage_Damage[2])*(((Prob_Garage_Damage$PR_Garage_Damage[2]*(1-Prob_Garage_Damage$PR_Garage_Damage[2]))/var(Prob_Garage_Damage$PR_Garage_Damage))-1)

alpha_Street=Prob_Garage_Damage$PR_Garage_Damage[3]*(((Prob_Garage_Damage$PR_Garage_Damage[3]*(1-Prob_Garage_Damage$PR_Garage_Damage[3]))/var(Prob_Garage_Damage$PR_Garage_Damage))-1)
beta_Street=(1-Prob_Garage_Damage$PR_Garage_Damage[3])*(((Prob_Garage_Damage$PR_Garage_Damage[3]*(1-Prob_Garage_Damage$PR_Garage_Damage[3]))/var(Prob_Garage_Damage$PR_Garage_Damage))-1) 
set.seed(105)
closed_zbox_beta_training=rbeta(nrow(data_training_sev_subset_Damage),shape1 = alpha_closed_zbox,shape2 = beta_closed_zbox)
set.seed(106)
opened_closed_beta_training=rbeta(nrow(data_training_sev_subset_Damage),shape1 = alpha_Opened_closed,shape2 = beta_Opened_closed)
set.seed(107)
street_beta_training=rbeta(nrow(data_training_sev_subset_Damage),shape1 = alpha_Street,shape2 = beta_Street)

data_training_sev_subset_Damage=cbind(data_training_sev_subset_Damage,closed_zbox_trun_norm_training,opened_closed_trun_norm_training,street_trun_norm_training
                                      ,closed_zbox_beta_training,opened_closed_beta_training,street_beta_training)
set.seed(100)
# family =Gamma(link="log")
mod1_sev_damage=glm(Sev_Damage~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = Gamma(link="log"),data = data_training_sev_subset_Damage)
summary(mod1_sev_damage)
step_mod1_sev_damage=stepAIC(mod1_sev_damage,direction = "both")
summary(step_mod1_sev_damage)

# family=Gaussian(link="identity")
mod2_sev_damage=glm(Sev_Damage~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = gaussian(link="identity"), data = data_training_sev_subset_Damage)
summary(mod2_sev_damage)
step_mod2_sev_damage=stepAIC(mod2_sev_damage,direction = "both")
summary(step_mod2_sev_damage)

#family=Gaussian(link="log")
mod2_sev_damage_log=glm(Sev_Damage~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = gaussian(link="log"), data = data_training_sev_subset_Damage)
summary(mod2_sev_damage_log)
step_mod2_sev_damage_log=stepAIC(mod2_sev_damage_log,direction = "both")
summary(step_mod2_sev_damage_log)


# family= inverse.gaussian(link="log")
mod3_sev_damage=glm(Sev_Damage~Garage,family = inverse.gaussian(link="log"), data = data_training_sev_subset_Damage)
summary(mod3_sev_damage) 
step_mod3_sev_damage=stepAIC(mod3_sev_damage,direction = "both")
summary(step_mod3_sev_damage)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# copula model training 
u_training_damage_inv_gaussian=pobs(data_training_sev_subset_Damage[c(23,30)])
fit.copula_training_damage=fitCopula(normalCopula(dim = 2,dispstr = "un"),u_training_damage_inv_gaussian,method="ml")
summary(fit.copula_training_damage) 

R_damage_training_inv_gaussian=matrix(1,nrow=2,ncol=2)
R_damage_training_inv_gaussian[1,2]=fit.copula_training_damage@estimate[[1]]
R_damage_training_inv_gaussian[2,1]=fit.copula_training_damage@estimate[[1]]

R2_damage_training_inv_gaussian=R_damage_training_inv_gaussian[1:1,1:1]
r_damage_training_inv_gaussian=R_damage_training_inv_gaussian[1:1,2]

v1_street_training =NULL
v_mat_damage_training_trunc_norm=matrix(NA,ncol=1,nrow = nrow(data_training_sev_subset_Damage))
b_damage_training_inv_gaussian=NULL
q_damage_training_inv_gaussian=NULL
pl_training_inv_gaussian_damage=NULL
y_simull_training_inv_gaussian_damage=NULL
y_damage_training_inv_gaussian=NULL
mean_training_inv_gaussian_damage=NULL
acceptance_rates_damage=NULL
set.seed(100)
for(j in 1:nrow(data_training_sev_subset_Damage)){
  y_simull_training_inv_gaussian_damage=NULL
  sampel_yang_diterima = 0 
  total_sampel = 0 
  for (i in 1:100){
      
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_street_training[j]=qnorm(ptruncnorm(data_training_sev_subset_Damage$street_trun_norm_training[j],a=a_Street,b=b_Street,mean = (a_Street+b_Street)/2,sd=(b_Street-a_Street)/6),0,1,lower.tail = TRUE)

    # matrix baris dari v*  
    v_mat_damage_training_trunc_norm[j,1]<-v1_street_training[j]
    vmat_damage_training_trunc_norm=t(v_mat_damage_training_trunc_norm)

    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_damage_training_inv_gaussian  
    a_damage_training_inv_gaussian= 1-t(r_damage_training_inv_gaussian)%*%(R2_damage_training_inv_gaussian)%*%r_damage_training_inv_gaussian
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b_damage_training_inv_gaussian
    
    b_damage_training_inv_gaussian[j]= t(r_damage_training_inv_gaussian)%*%(R2_damage_training_inv_gaussian)%*%(vmat_damage_training_trunc_norm[,j])
    
    # pdf(y) yang digunakan disini adalah inverse gaussian 
    random_inv_gaussian=rinvgauss(nrow(data_training_sev_subset_Damage),mean = 2173.8,shape=943.07)
    
    y_damage_training_inv_gaussian[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean = 2173.8,shape=943.07),0,1)
    
    # q disini merupakan pdf dari y|x1,x
    q_damage_training_inv_gaussian[j]=(dinvgauss(random_inv_gaussian[j],mean =2173.8 ,shape = 943.07 ))*exp(-0.5*(((y_damage_training_inv_gaussian[j]-b_damage_training_inv_gaussian[j])^2/a_damage_training_inv_gaussian)-(y_damage_training_inv_gaussian[j])^2))*(a_damage_training_inv_gaussian)^(-0.5)
    
    # distribusi proposal 
    pl_training_inv_gaussian_damage[j]=0.986*dinvgauss(random_inv_gaussian[j],mean =2400,shape=910)
    
    u=runif(1,0)
    if(u<q_damage_training_inv_gaussian[j]/pl_training_inv_gaussian_damage[j]){
      y_simull_training_inv_gaussian_damage= c(y_simull_training_inv_gaussian_damage,random_inv_gaussian)
      mean_training_inv_gaussian_damage[j]=mean(y_simull_training_inv_gaussian_damage)
      sampel_yang_diterima = sampel_yang_diterima + 1
    }
   total_sampel = total_sampel + 1
  }
 acceptance_rates_damage[j] <- sampel_yang_diterima / total_sampel
  
}

mean_training_inv_gaussian_damage
sum(acceptance_rates_damage=1)/nrow(data_training_sev_subset_Damage)

plot(density(pl_training_inv_gaussian_damage),col="blue",main="Plot Densitas Distribusi Target VS Proposal \n Inverse Gaussian-Truncated Gaussian Damage")
lines(density(q_damage_training_inv_gaussian),col="red")
plot(density(mean_training_inv_gaussian_damage))
# uji normalitas 
shapiro.test(mean_training_inv_gaussian_damage)# H0 diterima

# model testing 
set.seed(105)
closed_zbox_trun_norm_testing=rtruncnorm(nrow(data_testing_sev_subset_Damage),a=a_closed_zbox,b=b_closed_zbox,mean = (a_closed_zbox+b_closed_zbox)/2,sd=(b_closed_zbox-a_closed_zbox)/6)
set.seed(106)
opened_closed_trun_norm_testing= rtruncnorm(nrow(data_testing_sev_subset_Damage),a=a_Opened_closed,b=b_Opened_closed,mean=(a_Opened_closed+b_Opened_closed)/2,sd=(b_Opened_closed-a_Opened_closed)/6)
set.seed(107)
street_trun_norm_testing=rtruncnorm(nrow(data_testing_sev_subset_Damage),a=a_Street,b=b_Street,mean = (a_Street+b_Street)/2,sd=(b_Street-a_Street)/6)

set.seed(107)
street_beta_testing=rbeta(nrow(data_testing_sev_subset_Damage),shape1 = alpha_Street,shape2 = beta_Street)


data_testing_sev_subset_Damage=cbind(data_testing_sev_subset_Damage,closed_zbox_trun_norm_testing
                                     ,opened_closed_trun_norm_testing,street_trun_norm_testing,street_beta_testing)

v1_street_testing=NULL
v_mat_damage_testing=matrix(NA,ncol=1,nrow = nrow(data_testing_sev_subset_Damage))
b_damage_testing_inv_gaussian=NULL
q_damage_testing_inv_gaussian=NULL
pl_testing_inv_gaussian_damage=NULL
y_simull_testing_inv_gaussian_damage=NULL
y_damage_testing_inv_gaussian=NULL
mean_testing_inv_gaussian_damage=NULL
set.seed(100)
for(j in 1:nrow(data_testing_sev_subset_Damage)){
  y_simull_testing_inv_gaussian_damage=NULL
  for (i in 1:450){
#    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_street_testing[j]=qnorm(ptruncnorm(data_testing_sev_subset_Damage$street_trun_norm_testing[j],a=a_Street,b=b_Street,mean = (a_Street+b_Street)/2,sd=(b_Street-a_Street)/6),0,1,lower.tail = TRUE)
  
    # matrix baris dari v*  
    v_mat_damage_testing[j,1]<- v1_street_testing[j]
    vmat_damage_testing=t(v_mat_damage_testing)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_damage 
    a_damage_testing_inv_gaussian= 1-t(r_damage_training_inv_gaussian)%*%(R2_damage_training_inv_gaussian)%*%r_damage_training_inv_gaussian
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
    
    b_damage_testing_inv_gaussian[j]= t(r_damage_training_inv_gaussian)%*%(R2_damage_training_inv_gaussian)%*%(vmat_damage_testing[,j])

    random_inv_gaussian=rinvgauss(nrow(data_testing_sev_subset_Damage),mean = 2173.8,shape=943.07)
    
    y_damage_testing_inv_gaussian[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean = 2173.8,shape=943.07),0,1)    
    
    # q disini merupakan pdf dari y|x1,x
    q_damage_testing_inv_gaussian[j]=(dinvgauss(random_inv_gaussian[j],mean = 2173.8,shape=943.07))*exp(-0.5*(((y_damage_testing_inv_gaussian[j]-b_damage_testing_inv_gaussian[j])^2/a_damage_testing_inv_gaussian)-(y_damage_testing_inv_gaussian[j])^2))*(a_damage_testing_inv_gaussian)^(-0.5)
    
    # distribusi proposal 
    pl_testing_inv_gaussian_damage[j]=0.986*dinvgauss(random_inv_gaussian[j],mean =2400,shape=910)
    
    u=runif(1,0)
    if(u<q_damage_testing_inv_gaussian[j]/pl_testing_inv_gaussian_damage[j]){
      y_simull_testing_inv_gaussian_damage= c(y_simull_testing_inv_gaussian_damage,random_inv_gaussian)
      mean_testing_inv_gaussian_damage[j]=mean(y_simull_testing_inv_gaussian_damage)
    }
  }
}  
mean_testing_inv_gaussian_damage
plot(density( mean_testing_inv_gaussian_damage))
# uji normalitas 
shapiro.test( mean_testing_inv_gaussian_damage)

MSE_copula_damage_testing_inv_gaussian=sum((data_testing_sev_subset_Damage$Sev_Damage- mean_testing_inv_gaussian_damage)^2)/nrow(data_testing_sev_subset_Damage)
MSE_copula_damage_testing_inv_gaussian


# sev_damage dan beta 
# model training
u_training_damage_inv_gaussian_beta=pobs(data_training_sev_subset_Damage[c(23,33)])
fit.copula_training_damage_beta=fitCopula(normalCopula(dim = 2,dispstr = "un"),u_training_damage_inv_gaussian_beta,method="ml")
summary(fit.copula_training_damage_beta)

R_damage_training_inv_gaussian_beta=matrix(1,nrow=2,ncol=2)
R_damage_training_inv_gaussian_beta[1,2]=fit.copula_training_damage_beta@estimate[[1]]
R_damage_training_inv_gaussian_beta[2,1]=fit.copula_training_damage_beta@estimate[[1]]

R2_damage_training_inv_gaussian_beta=R_damage_training_inv_gaussian_beta[1:1,1:1]
r_damage_training_inv_gaussian_beta=R_damage_training_inv_gaussian_beta[1:1,2]

v1_street_training_beta=NULL
v_mat_damage_training_beta=matrix(NA,ncol=1,nrow = nrow(data_training_sev_subset_Damage))
a_damage_training_inv_gaussian_beta=NULL
b_damage_training_inv_gaussian_beta=NULL
q_damage_training_inv_gaussian_beta=NULL
pl_training_inv_gaussian_damage_beta=NULL
y_simull_training_inv_gaussian_damage_beta=NULL
y_damage_training_inv_gaussian_beta=NULL
mean_training_inv_gaussian_damage_beta=NULL
acceptance_rates_damage_beta=NULL
set.seed(100)
for(j in 1:nrow(data_training_sev_subset_Damage)){
  y_simull_training_inv_gaussian_damage_beta=NULL
  sampel_yang_diterima = 0 
  total_sampel = 0 
  for (i in 1:100){
    
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_street_training_beta[j]=qnorm(pbeta(data_training_sev_subset_Damage$street_beta_training[j],shape1 = alpha_Street,shape2 = beta_Street),0,1,lower.tail = TRUE)
    
    # matrix baris dari v*  
    v_mat_damage_training_beta[j,1]<-v1_street_training_beta[j]
    vmat_damage_training_beta=t(v_mat_damage_training_beta)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_damage_training_inv_gaussian  
    a_damage_training_inv_gaussian_beta=1-t(r_damage_training_inv_gaussian_beta)%*%(R2_damage_training_inv_gaussian_beta)%*%r_damage_training_inv_gaussian_beta
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b_damage_training_inv_gaussian
    b_damage_training_inv_gaussian_beta[j]=t(r_damage_training_inv_gaussian_beta)%*%(R2_damage_training_inv_gaussian_beta)%*%(vmat_damage_training_beta[,j])
    
    # pdf(y) yang digunakan disini adalah inverse gaussian 
    random_inv_gaussian=rinvgauss(nrow(data_training_sev_subset_Damage),mean = 2173.8,shape=943.07)
    y_damage_training_inv_gaussian_beta[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean = 2173.8,shape=943.07),0,1)
    
    # q disini merupakan pdf dari y|x1,x
    q_damage_training_inv_gaussian_beta[j]=(dinvgauss(random_inv_gaussian[j],mean =2173.8 ,shape = 943.07 ))*exp(-0.5*(((y_damage_training_inv_gaussian_beta[j]-b_damage_training_inv_gaussian_beta[j])^2/a_damage_training_inv_gaussian_beta)-(y_damage_training_inv_gaussian_beta[j])^2))*(a_damage_training_inv_gaussian_beta)^(-0.5)
    
    # distribusi proposal 
    pl_training_inv_gaussian_damage_beta[j]=0.945*dinvgauss(random_inv_gaussian[j],mean =2200,shape=810)
    
    u=runif(1,0)
    if(u<q_damage_training_inv_gaussian_beta[j]/pl_training_inv_gaussian_damage_beta[j]){
      y_simull_training_inv_gaussian_damage_beta= c(y_simull_training_inv_gaussian_damage_beta,random_inv_gaussian)
      mean_training_inv_gaussian_damage_beta[j]=mean(y_simull_training_inv_gaussian_damage_beta)
      sampel_yang_diterima = sampel_yang_diterima + 1
    }
    total_sampel = total_sampel + 1
  }
  acceptance_rates_damage_beta[j] <- sampel_yang_diterima / total_sampel
  
}

mean_training_inv_gaussian_damage_beta

plot(density(q_damage_training_inv_gaussian_beta),col="red",main="Plot Distribusi Target VS Proposal \n Inverse Gaussian-Beta Damage")
lines(density(pl_training_inv_gaussian_damage_beta),col="blue")
# plot density dari training 
plot(density(mean_training_inv_gaussian_damage_beta))

# uji normalitas 
shapiro.test(mean_training_inv_gaussian_damage_beta)# H0 diterima
sum(acceptance_rates_damage_beta==1)/nrow(data_training_sev_subset_Damage)

v1_street_testing_beta=NULL
v_mat_damage_testing_beta=matrix(NA,ncol=1,nrow = nrow(data_testing_sev_subset_Damage))
b_damage_testing_inv_gaussian_beta=NULL
q_damage_testing_inv_gaussian_beta=NULL
pl_testing_inv_gaussian_damage_beta=NULL
y_simull_testing_inv_gaussian_damage_beta=NULL
y_damage_testing_inv_gaussian_beta=NULL
mean_testing_inv_gaussian_damage_beta=NULL
set.seed(100)
for(j in 1:nrow(data_testing_sev_subset_Damage)){
  y_simull_testing_inv_gaussian_damage_beta=NULL
  for (i in 1:450){
    #    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_street_testing_beta[j]=qnorm(pbeta(data_testing_sev_subset_Damage$street_beta_testing[j],shape1 = alpha_Street,shape2 = beta_Street),0,1,lower.tail = TRUE)
    
    # matrix baris dari v*  
    v_mat_damage_testing_beta[j,1]<- v1_street_testing_beta[j]
    vmat_damage_testing_beta=t(v_mat_damage_testing_beta)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_damage 
    a_damage_testing_inv_gaussian_beta= 1-t(r_damage_training_inv_gaussian)%*%(R2_damage_training_inv_gaussian)%*%r_damage_training_inv_gaussian
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
    b_damage_testing_inv_gaussian_beta[j]= t(r_damage_training_inv_gaussian)%*%(R2_damage_training_inv_gaussian)%*%(vmat_damage_testing_beta[,j])
    
    random_inv_gaussian=rinvgauss(nrow(data_testing_sev_subset_Damage),mean = 2173.8,shape=943.07)
    
    y_damage_testing_inv_gaussian_beta[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean = 2173.8,shape=943.07),0,1)    
    
    # q disini merupakan pdf dari y|x1,x
    q_damage_testing_inv_gaussian_beta[j]=(dinvgauss(random_inv_gaussian[j],mean = 2173.8,shape=943.07))*exp(-0.5*(((y_damage_testing_inv_gaussian_beta[j]-b_damage_testing_inv_gaussian_beta[j])^2/a_damage_testing_inv_gaussian_beta)-(y_damage_testing_inv_gaussian_beta[j])^2))*(a_damage_testing_inv_gaussian_beta)^(-0.5)
    
    # distribusi proposal 
    pl_testing_inv_gaussian_damage_beta[j]=0.945*dinvgauss(random_inv_gaussian[j],mean =2200,shape=810)
    
    u=runif(1,0)
    if(u<q_damage_testing_inv_gaussian_beta[j]/pl_testing_inv_gaussian_damage_beta[j]){
      y_simull_testing_inv_gaussian_damage_beta= c(y_simull_testing_inv_gaussian_damage_beta,random_inv_gaussian)
      mean_testing_inv_gaussian_damage_beta[j]=mean(y_simull_testing_inv_gaussian_damage_beta)
    }
  }
}  
mean_testing_inv_gaussian_damage_beta
plot(density( mean_testing_inv_gaussian_damage_beta),main="Plot Densitas Prediksi Copula \n Inverse Gaussian-Beta Damage")
# uji normalitas 
shapiro.test( mean_testing_inv_gaussian_damage_beta)

MSE_copula_damage_testing_inv_gaussian_beta=sum((data_testing_sev_subset_Damage$Sev_Damage- mean_testing_inv_gaussian_damage_beta)^2)/nrow(data_testing_sev_subset_Damage)
MSE_copula_damage_testing_inv_gaussian_beta



#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Predict
y_hat_Damage_Gamma=predict(step_mod1_sev_damage,data_testing_sev_subset_Damage,type = "response")
MSE_Damage_Gamma=sum((data_testing_sev_subset_Damage$Sev_Damage-y_hat_Damage_Gamma)^2)/nrow(data_testing_sev_subset_Damage)
MSE_Damage_Gamma

y_hat_Damage_Gaussian=predict(step_mod2_sev_damage,data_testing_sev_subset_Damage,type = "response")
MSE_Damage_Gaussian=sum((data_testing_sev_subset_Damage$Sev_Damage-y_hat_Damage_Gaussian)^2)/nrow(data_testing_sev_subset_Damage)
MSE_Damage_Gaussian

y_hat_Damage_Gaussian_log=predict(step_mod2_sev_damage_log,data_testing_sev_subset_Damage,type = "response")
MSE_Damage_Gaussian_log=sum((data_testing_sev_subset_Damage$Sev_Damage-y_hat_Damage_Gaussian_log)^2)/nrow(data_testing_sev_subset_Damage)
MSE_Damage_Gaussian_log

y_hat_Damage_Inverse_Gaussian=predict(step_mod3_sev_damage,data_testing_sev_subset_Damage,type = "response")
MSE_Damage_InverseGaussian=sum((data_testing_sev_subset_Damage$Sev_Damage-y_hat_Damage_Inverse_Gaussian)^2)/nrow(data_testing_sev_subset_Damage)
MSE_Damage_InverseGaussian

#Frekuensi 
mod1_freq_pois_damage=glm(Damage~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = poisson(log),data = data_training)
summary(mod1_freq_pois_damage)
step_mod1_freq_pois_TPL=stepAIC(mod1_freq_pois_damage,direction = "both")
summary(step_mod1_freq_pois_TPL)

#Predict 
y_hat_freq_Poisson_damage=predict(mod1_freq_pois_damage,data_testing_sev_subset_Damage,type="response") 
y_hat_freq_Poisson_damage

#Pure_premium_damage 

pure_prem_GammaPoisson_damage=y_hat_Damage_Gamma*y_hat_freq_Poisson_damage
pure_prem_GaussianPoisson_damage=y_hat_Damage_Gaussian*y_hat_freq_Poisson_damage
pure_prem_GaussianPoisson_damage_log=y_hat_Damage_Gaussian_log*y_hat_freq_Poisson_damage
pure_prem_InverseGaussianPoisson_damage=y_hat_Damage_Inverse_Gaussian*y_hat_freq_Poisson_damage

pure_prem_InvGaussPoisson_copula_damage=mean_testing_inv_gaussian_damage*y_hat_freq_Poisson_damage
pure_prem_InvGaussPoisson_copula_damage_beta=mean_testing_inv_gaussian_damage_beta*y_hat_freq_Poisson_damage

#MSE
S_Damage=data_testing_sev_subset_Damage$Sev_Damage*data_testing_sev_subset_Damage$Damage
MSE_Damage_GammaPoisson=sum((S_Damage-pure_prem_GammaPoisson_damage)^2)/nrow(data_testing_sev_subset_Damage)
MSE_Damage_GammaPoisson

MSE_Damage_GaussianPoisson=sum((S_Damage-pure_prem_GaussianPoisson_damage)^2)/nrow(data_testing_sev_subset_Damage)
MSE_Damage_GaussianPoisson

MSE_Damage_GaussianPoisson_log=sum((S_Damage-pure_prem_GaussianPoisson_damage_log)^2)/nrow(data_testing_sev_subset_Damage)
MSE_Damage_GaussianPoisson_log


MSE_Damage_InverseGaussianPoisson=sum((S_Damage-pure_prem_InverseGaussianPoisson_damage)^2)/nrow(data_testing_sev_subset_Damage)
MSE_Damage_InverseGaussianPoisson

MSE_Damage_InvGausPoisson_copula=sum((S_Damage-pure_prem_InvGaussPoisson_copula_damage)^2)/nrow(data_testing_sev_subset_Damage)
MSE_Damage_InvGausPoisson_copula

MSE_Damage_InvGausPoisson_copula_beta=sum((S_Damage-pure_prem_InvGaussPoisson_copula_damage_beta)^2)/nrow(data_testing_sev_subset_Damage)
MSE_Damage_InvGausPoisson_copula_beta

# boxplot 
Sev_Damage_testing=data.frame(cbind(data_testing_sev_subset_Damage$Sev_Damage,y_hat_Damage_Gamma,y_hat_Damage_Gaussian,y_hat_Damage_Gaussian_log,
                                    y_hat_Damage_Inverse_Gaussian,mean_testing_inv_gaussian_damage,mean_testing_inv_gaussian_damage_beta))
colnames(Sev_Damage_testing)<-c("Data","y hat Gamma","y hat Normal","y hat Normal\n Log","y hat inverse \nGaussian","y hat \n inverse Gaussian-\ntruncated Gaussian"
                                ,"y hat inverse \nGaussian-Beta")
df_damage <- tidyr::gather(Sev_Damage_testing, key = "Model", value = "Predicted_Values")
df_damage$Model <- factor(df_damage$Model, levels = colnames(Sev_Damage_testing))

mean_values_damage <- aggregate(Predicted_Values ~ Model, data = df_damage, FUN = mean)

ggplot(df_damage, aes(x = Model, y = Predicted_Values)) +
  geom_boxplot()+
  geom_point(data = mean_values_damage, aes(x = Model, y = Predicted_Values), col = "gray", pch = 19)+
  geom_text(data = mean_values_damage, aes(x = Model, y = Predicted_Values, label = round(Predicted_Values, 2)),
            vjust = -0.5, hjust = 0.5, size = 3, col = "black") +
  scale_y_continuous(limits = c(0,6500))+
  labs(title = "Boxplot Model Besar Klaim Damage", 
       x = "Models", y = "Predicted Values") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Boxplot premi murni 
premi_murni_damage=data.frame(cbind(S_Damage,pure_prem_GammaPoisson_damage,pure_prem_GaussianPoisson_damage,pure_prem_GaussianPoisson_damage_log
                                 ,pure_prem_InverseGaussianPoisson_damage,pure_prem_InvGaussPoisson_copula_damage
                                 ,pure_prem_InvGaussPoisson_copula_damage_beta))
colnames(premi_murni_damage) <- c("Data", "Pois &\n Gamma", "Pois & \nNormal", "Pois & \nNormal_log","Pois & Inverse \nGaussian"
                               ,"Pois & inverse \n Gaussian- \ntruncated Gaussian", "Pois & inverse \n Gaussian- Beta")
df_premi_murni_damage <- tidyr::gather(premi_murni_damage, key = "Model", value = "Predicted_Values")
df_premi_murni_damage$Model <- factor(df_premi_murni_damage$Model, levels = colnames(premi_murni_damage))
mean_premi_murni_damage <- aggregate(Predicted_Values ~ Model, data = df_premi_murni_damage, FUN = mean)

ggplot(df_premi_murni_damage, aes(x = Model, y = Predicted_Values)) +
  geom_boxplot()+
  geom_point(data = mean_premi_murni_damage, aes(x = Model, y = Predicted_Values), col = "gray", pch = 19)+
  geom_text(data = mean_premi_murni_damage, aes(x = Model, y = Predicted_Values, label = round(Predicted_Values, 4)),
            vjust = -0.5, hjust = 0.5, size = 3, col = "black")+
  labs(title = "Boxplot Premi Murni Damage", 
       x = "Models", y = "Predicted Values") +
  scale_y_continuous(limits = c(0,3000),expand = c(0.1, 0))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

############################################################################################################################################################################################################

# Sev_Other
data_training_Other_subset=subset(data_training,Sev_Other>0)
data_testing_Other_subset=data_testing
summary(data_training_Other_subset)

data_malus_other=mapvalues(data_training_Other_subset$BonusMalus, from = c("bonus","malus")
                             , to = c("1", "2"))
data_malus_other=data.frame(as.factor(data_malus_other))

frequency_table_malus_other <- table(data_malus_other)

PR_malus_other=NULL
for( i in 1:2){ 
  PR_malus_other[i]=frequency_table_malus_other[i]/(frequency_table_malus_other[1]+frequency_table_malus_other[2])
}
PR_malus_other

malus<- c("bonus","malus")
Cumprob_malus_other=data.frame(cbind(malus,PR_malus_other))
Prob_malus_other=Cumprob_malus_other[order(Cumprob_malus_other$PR_malus_other,decreasing = TRUE),]
Prob_malus_other$PR_malus_other=as.numeric(Prob_malus_other$PR_malus_other)

# Truncated Gaussian 
# BonusMalus:malus 
a_bonus_other=0
b_bonus_other=Prob_malus_other$PR_malus_other[1]

a_malus_other=Prob_malus_other$PR_malus_other[1]
b_malus_other=Prob_malus_other$PR_malus_other[1]+Prob_malus_other$PR_malus_other[2]

set.seed(105)
bonus_trun_norm_training_other= rtruncnorm(nrow(data_training_Other_subset),a=a_bonus_other,b=b_bonus_other,mean=(a_bonus_other+b_bonus_other)/2,sd=(b_bonus_other-a_bonus_other)/6)

set.seed(106)
malus_trun_norm_training_other=rtruncnorm(nrow(data_training_Other_subset),a=a_malus_other,b=b_malus_other,mean = (a_malus_other+b_malus_other)/2,sd=(b_malus_other-a_malus_other)/6)

# VehClass: Cheapest 

data_VehClass_other=mapvalues(data_training_Other_subset$VehClass, from = c("other","Cheaper", "Cheapest")
                           , to = c("1", "2","3"))
data_VehClass_other=data.frame(as.factor(data_VehClass_other))

frequency_table_VehClass_other <- table(data_VehClass_other)

PR_VehClass_other=NULL
for( i in 1:3){ 
  PR_VehClass_other[i]=frequency_table_VehClass_other[i]/(frequency_table_VehClass_other[1]+frequency_table_VehClass_other[2]+frequency_table_VehClass_other[3])
}
PR_VehClass_other

VehClass<- c("other","Cheaper", "Cheapest")
Cumprob_VehClass_other=data.frame(cbind(VehClass,PR_VehClass_other))
Prob_VehClass_other=Cumprob_VehClass_other[order(Cumprob_VehClass_other$PR_VehClass_other,decreasing = TRUE),]
Prob_VehClass_other$PR_VehClass_other=as.numeric(Prob_VehClass_other$PR_VehClass_other)

#VehClass
a_other_other=0
b_other_other=Prob_VehClass_other$PR_VehClass_other[1]

a_cheaper_other=Prob_VehClass_other$PR_VehClass_other[1]
b_cheaper_other=Prob_VehClass_other$PR_VehClass_other[1]+Prob_VehClass_other$PR_VehClass_other[2]

a_cheapest_other=Prob_VehClass_other$PR_VehClass_other[1]+Prob_VehClass_other$PR_VehClass_other[2]
b_cheapest_other=Prob_VehClass_other$PR_VehClass_other[1]+Prob_VehClass_other$PR_VehClass_other[2]+Prob_VehClass_other$PR_VehClass_other[3]

set.seed(107)
other_trun_norm_training_other= rtruncnorm(nrow(data_training_Other_subset),a=a_other_other,b=b_other_other,mean=(a_other_other+b_other_other)/2,sd=(b_other_other-a_other_other)/6)

set.seed(108)
cheaper_trun_norm_training_other=rtruncnorm(nrow(data_training_Other_subset),a=a_cheaper_other,b=b_cheaper_other,mean = (a_cheaper_other+b_cheaper_other)/2,sd=(b_cheaper_other-a_cheaper_other)/6)

set.seed(109)
cheapest_trun_norm_training_other= rtruncnorm(nrow(data_training_Other_subset),a=a_cheapest_other,b=b_cheapest_other,mean = (a_cheapest_other+b_cheapest_other)/2,sd=(b_cheapest_other-a_cheapest_other)/6)

# VehPower

data_VehPower_other=mapvalues(data_training_Other_subset$VehPower, from = c("P10+P11+P12","P13-P17","P2-P9")
                              , to = c("1", "2","3"))
data_VehPower_other=data.frame(as.factor(data_VehPower_other))

frequency_table_VehPower_other <- table(data_VehPower_other)

PR_VehPower_other=NULL
for( i in 1:3){ 
  PR_VehPower_other[i]=frequency_table_VehPower_other[i]/(frequency_table_VehPower_other[1]+frequency_table_VehPower_other[2]+frequency_table_VehPower_other[3])
}
PR_VehPower_other

VehPower<- c("P10+P11+P12","P13-P17","P2-P9")
Cumprob_VehPower_other=data.frame(cbind(VehPower,PR_VehPower_other))
Prob_VehPower_other=Cumprob_VehPower_other[order(Cumprob_VehPower_other$PR_VehPower_other,decreasing = TRUE),]
Prob_VehPower_other$PR_VehPower_other=as.numeric(Prob_VehPower_other$PR_VehPower_other)

a_P10_P12_other=0
b_P10_P12_other=Prob_VehPower_other$PR_VehPower_other[1]

a_P13_P17_other=Prob_VehPower_other$PR_VehPower_other[1]
b_P13_P17_other=Prob_VehPower_other$PR_VehPower_other[1]+Prob_VehPower_other$PR_VehPower_other[2]

a_P2_P9_other=Prob_VehPower_other$PR_VehPower_other[1]+Prob_VehPower_other$PR_VehPower_other[2]
b_P2_P9_other=Prob_VehPower_other$PR_VehPower_other[1]+Prob_VehPower_other$PR_VehPower_other[2]+Prob_VehPower_other$PR_VehPower_other[3]

set.seed(110)
P10_P12_trun_norm_training_other= rtruncnorm(nrow(data_training_Other_subset),a=a_P10_P12_other,b=b_P10_P12_other,mean=(a_P10_P12_other+b_P10_P12_other)/2,sd=(b_P10_P12_other-a_P10_P12_other)/6)

set.seed(111)
P13_P17_trun_norm_training_other=rtruncnorm(nrow(data_training_Other_subset),a=a_P13_P17_other,b=b_P13_P17_other,mean = (a_P13_P17_other+b_P13_P17_other)/2,sd=(b_P13_P17_other-a_P13_P17_other)/6)

set.seed(112)
P2_P9_trun_norm_training_other= rtruncnorm(nrow(data_training_Other_subset),a=a_P2_P9_other,b=b_P2_P9_other,mean = (a_P2_P9_other+b_P2_P9_other)/2,sd=(b_P2_P9_other-a_P2_P9_other)/6)

# Channel:B+L
data_Channel_other=mapvalues(data_training_Other_subset$Channel, from = c("A","B+L")
                              , to = c("1", "2"))
data_Channel_other=data.frame(as.factor(data_Channel_other))

frequency_table_Channel_other <- table(data_Channel_other)

PR_Channel_other=NULL
for( i in 1:2){ 
  PR_Channel_other[i]=frequency_table_Channel_other[i]/(frequency_table_Channel_other[1]+frequency_table_Channel_other[2])
}
PR_Channel_other

Channel<- c("A","B+L")
Cumprob_Channel_other=data.frame(cbind(Channel,PR_Channel_other))
Prob_Channel_other=Cumprob_Channel_other[order(Cumprob_Channel_other$PR_Channel_other,decreasing = TRUE),]
Prob_Channel_other$PR_Channel_other=as.numeric(Prob_Channel_other$PR_Channel_other)

a_A_other=0
b_A_other=Prob_Channel_other$PR_Channel_other[1]

a_B_L_other=Prob_Channel_other$PR_Channel_other[1]
b_B_L_other=Prob_Channel_other$PR_Channel_other[1]+Prob_Channel_other$PR_Channel_other[2]

set.seed(113)
A_trun_norm_training_other= rtruncnorm(nrow(data_training_Other_subset),a=a_A_other,b=b_A_other,mean=(a_A_other+b_A_other)/2,sd=(b_A_other-a_A_other)/6)

set.seed(114)
B_L_trun_norm_training_other=rtruncnorm(nrow(data_training_Other_subset),a=a_B_L_other,b=b_B_L_other,mean = (a_B_L_other+b_B_L_other)/2,sd=(b_B_L_other-b_A_other)/6)


# beta 

data_training_Other_subset01=cbind(data_training_Other_subset,malus_trun_norm_training_other, cheapest_trun_norm_training_other
                                 ,P2_P9_trun_norm_training_other,B_L_trun_norm_training_other)
#model testing
set.seed(106)
malus_trun_norm_testing_other=rtruncnorm(nrow(data_testing_Other_subset),a=a_malus_other,b=b_malus_other,mean = (a_malus_other+b_malus_other)/2,sd=(b_malus_other-a_malus_other)/6)
set.seed(109)
cheapest_trun_norm_testing_other= rtruncnorm(nrow(data_testing_Fire_subset),a=a_cheapest_other,b=b_cheapest_other,mean = (a_cheapest_other+b_cheapest_other)/2,sd=(b_cheapest_other-a_cheapest_other)/6)
set.seed(112)
P2_P9_trun_norm_testing_other= rtruncnorm(nrow(data_testing_Other_subset),a=a_P2_P9_other,b=b_P2_P9_other,mean = (a_P2_P9_other+b_P2_P9_other)/2,sd=(b_P2_P9_other-a_P2_P9_other)/6)
set.seed(114)
B_L_trun_norm_testing_other=rtruncnorm(nrow(data_testing_Other_subset),a=a_B_L_other,b=b_B_L_other,mean = (a_B_L_other+b_B_L_other)/2,sd=(b_B_L_other-b_A_other)/6)

data_testing_Other_subset01=cbind(data_testing_Other_subset,malus_trun_norm_testing_other, cheapest_trun_norm_testing_other
                                 ,P2_P9_trun_norm_testing_other,B_L_trun_norm_testing_other)


set.seed(100) 
#family=Gamma(link="log")
mod1_Other=glm(Sev_Other~DrivAge+DrivGender+BonusMalus+PayFreq+VehAge+VehGas+Area+VehClass+Channel,family = Gamma(link="log"),data = data_training_Other_subset)
summary(mod1_Other) #Region+Garage+VehPower
step_mod1_Other=stepAIC(mod1_Other,direction = "both")
summary(step_mod1_Other)

#family: Gaussian (link="identity")
mod2_Other=glm(Sev_Other~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = gaussian(link="identity"),data = data_training_Other_subset)
summary(mod2_Other)
step_mod2_Other=stepAIC(mod2_Other,direction = "both")
summary(step_mod2_Other)

#family: Gaussian (link="log")
mod2_Other_log=glm(Sev_Other~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehGas+Area+Channel,family = gaussian(link="log"),data = data_training_Other_subset)
summary(mod2_Other_log) # Region,Garage, VehPower
step_mod2_Other_log=stepAIC(mod2_Other_log,direction = "both")
summary(step_mod2_Other_log)

# family: InverseGaussian(link="log")
mod3_Other=glm(Sev_Other~VehClass+VehAge,family = inverse.gaussian(link="log"),data = data_training_Other_subset)
summary(mod3_Other) 
step_mod3_Other=stepAIC(mod3_Other,direction = "both")
summary(step_mod3_Other)

# Predict
y_hat_Other_Gamma=predict(mod1_Other,data_testing_Other_subset,type = "response")
MSE_Other_Gamma=sum((data_testing_Other_subset$Sev_Other-y_hat_Other_Gamma)^2)/nrow(data_testing_Other_subset)
MSE_Other_Gamma

y_hat_Other_Gaussian=predict(step_mod2_Other,data_testing_Other_subset,type = "response")
MSE_Other_Gaussian=sum((data_testing_Other_subset$Sev_Other-y_hat_Other_Gaussian)^2)/nrow(data_testing_Other_subset)
MSE_Other_Gaussian

y_hat_Other_Gaussian_log=predict(step_mod2_Other_log,data_testing_Other_subset,type = "response")
MSE_Other_Gaussian_log=sum((data_testing_Other_subset$Sev_Other-y_hat_Other_Gaussian_log)^2)/nrow(data_testing_Other_subset)
MSE_Other_Gaussian_log

y_hat_Other_Inverse_Gaussian=predict(step_mod3_Other,data_testing_Other_subset,type = "response")
MSE_Other_InverseGaussian=sum((data_testing_Other_subset$Sev_Other-y_hat_Other_Inverse_Gaussian)^2)/nrow(data_testing_Other_subset)
MSE_Other_InverseGaussian


#--------------------------------------------------------------------------------------------------------------------------------------------------
# Copula training Other dengan Sev_Other dan (BonusMalus: malus, VehClass: Cheapest, VehPower:P2-P9, Channel:B+L)

u_training_other_Normal=pobs(data_training_Other_subset01[c(24,28,29,30,31)])
fit.copula_training_other_Normal=fitCopula(normalCopula(dim = 5,dispstr = "un"),u_training_other_Normal,method="ml")
summary(fit.copula_training_other_Normal)

# Membentuk matrix parameter copula R dari data training 
R_other_training_Normal=matrix(1,nrow=5,ncol=5)
for (i in 1:5) {
  if(i>=2 && i<=5){R_other_training_Normal[1,i]=fit.copula_training_other_Normal@estimate[i-1]}
  if(i>=3 && i<=5){R_other_training_Normal[2,i]=fit.copula_training_other_Normal@estimate[i+2]}
  if(i>=4 && i<=5){R_other_training_Normal[3,i]=fit.copula_training_other_Normal@estimate[i+4]}
  if(i>=5 && i<=5){R_other_training_Normal[4,i]=fit.copula_training_other_Normal@estimate[i+5]}
}
segitiga_bawah <- lower.tri(R_other_training_Normal,diag = TRUE)
R_other_training_Normal[segitiga_bawah]=0
t_R_other_training_Normal=t(R_other_training_Normal)
R_other_training_Normal=R_other_training_Normal+t_R_other_training_Normal
diag(R_other_training_Normal)=1

R2_other_training_Normal=R_other_training_Normal[1:4,1:4]
r_other_training_Normal=R_other_training_Normal[1:4,5]

v1_malus_training_other=NULL
v1_cheapest_training_other=NULL
v1_vehpower_training_other=NULL
v1_channel_training_other=NULL
v_mat_other_Normal=matrix(NA,ncol=4,nrow = nrow(data_training_Other_subset01))
b_other_training_Normal=NULL
q_other_training_Normal=NULL
pl_training_Normal_other=NULL
y_simull_training_Normal_other=NULL
y_other_training_Normal=NULL
mean_training_Normal_other=NULL
acceptance_rates_other_Normal=NULL
set.seed(100)
for(j in 1:nrow(data_training_Other_subset)){
  y_simull_training_Normal_other=NULL
  sampel_yang_diterima = 0 
  total_sampel = 0 
  for (i in 1:100){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2 # data ke 15 dan 33
    v1_malus_training_other[j]=qnorm(ptruncnorm(data_training_Other_subset01$malus_trun_norm_training_other[j],a=a_malus_other,b_malus_other,mean = (a_malus_other+b_malus_other)/2,sd=(b_malus_other-a_malus_other)/6),0,1,lower.tail = TRUE)
    v1_cheapest_training_other[j]=qnorm(ptruncnorm(data_training_Other_subset01$cheapest_trun_norm_training_other[j],a=a_cheapest_other,b=b_cheapest_other,mean=(a_cheapest_other+b_cheapest_other)/2,sd=(b_cheapest_other-a_cheapest_other)/6),0,1,lower.tail = TRUE)
    v1_vehpower_training_other[j]=qnorm(ptruncnorm(data_training_Other_subset01$P2_P9_trun_norm_training_other[j],a=a_P2_P9_other,b=b_P2_P9_other,mean=(b_P2_P9_other+a_P2_P9_other)/2,sd=(b_P2_P9_other-a_P2_P9_other)/6),0,1,lower.tail = TRUE)
    v1_channel_training_other[j]=qnorm(ptruncnorm(data_training_Other_subset01$B_L_trun_norm_training_other[j],a=a_B_L_other,b=b_B_L_other,mean=(a_B_L_other+b_B_L_other)/2,sd=(b_B_L_other-a_B_L_other)/6),0,1,lower.tail = TRUE)  
    # matrix baris dari v*  
    v_mat_other_Normal[j,1]<-v1_malus_training_other[j]
    v_mat_other_Normal[j,2]<-v1_cheapest_training_other[j]
    v_mat_other_Normal[j,3]<-v1_vehpower_training_other[j]
    v_mat_other_Normal[j,4]<-v1_channel_training_other[j]
    vmat_other_Normal=t(v_mat_other_Normal)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_damage_training_inv_gaussian  
    a_other_training_Normal=1-t(r_other_training_Normal)%*%inv(R2_other_training_Normal)%*%r_other_training_Normal
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b_damage_training_inv_gaussian
    b_other_training_Normal[j]=t(r_other_training_Normal)%*%inv(R2_other_training_Normal)%*%(vmat_other_Normal[,j])
    
    # pdf(y) yang digunakan disini adalah inverse gaussian 
    random_Normal=rnorm(nrow(data_training_Other_subset01),mean = 848.94,sd=1081.9)
    
    y_other_training_Normal[j]=qnorm(pnorm(random_Normal[j],mean = 848.94,sd=1081.9),0,1)
    # q disini merupakan pdf dari y|x1,x
    q_other_training_Normal[j]=(dnorm(random_Normal[j],mean = 848.94,sd=1081.9 ))*exp(-0.5*(((y_other_training_Normal[j]-b_other_training_Normal[j])^2/a_other_training_Normal)-(y_other_training_Normal[j])^2))*(a_other_training_Normal)^(-0.5)
    
    # distribusi proposal 
    pl_training_Normal_other[j]=1.024*dnorm(random_Normal[j],mean =850,sd=1091)    
    u=runif(1,0)
    if(u<q_other_training_Normal[j]/pl_training_Normal_other[j]){
      y_simull_training_Normal_other=c(y_simull_training_Normal_other,random_Normal)
      mean_training_Normal_other[j]=mean(y_simull_training_Normal_other)
      sampel_yang_diterima = sampel_yang_diterima + 1
    }
    total_sampel = total_sampel + 1
  }
  acceptance_rates_other_Normal[j] <- sampel_yang_diterima / total_sampel
}
sum(acceptance_rates_other_Normal==1)/nrow(data_training_Other_subset)

plot(density(pl_training_Normal_other),col="blue",main="Plot Densitas Distribusi Target VS Distribusi Proposal \n Copula Normal-Truncated Gaussian Other")
lines(density(q_other_training_Normal),col="red")

# plot density dari training 
plot(density(mean_training_Normal_other))

# Uji Normalitas 
shapiro.test(mean_training_Normal_other)

# Copula testing 

v1_malus_testing_other=NULL
v1_cheapest_testing_other=NULL
v1_vehpower_testing_other=NULL
v1_channel_testing_other=NULL
v_mat_other_Normal_testing=matrix(NA,ncol=4,nrow = nrow(data_testing_Other_subset01))
b_other_testing_Normal=NULL
q_other_testing_Normal=NULL
pl_testing_Normal_other=NULL
y_simull_testing_Normal_other=NULL
y_other_testing_Normal=NULL
mean_testing_Normal_other=NULL
set.seed(100)
for(j in 1:nrow(data_testing_Other_subset01)){
  y_simull_testing_Normal_other=NULL
  for (i in 1:450){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2 # data ke 15 dan 33
    v1_malus_testing_other[j]=qnorm(ptruncnorm(data_testing_Other_subset01$malus_trun_norm_testing_other[j],a=a_malus_other,b_malus_other,mean = (a_malus_other+b_malus_other)/2,sd=(b_malus_other-a_malus_other)/6),0,1,lower.tail = TRUE)
    v1_cheapest_testing_other[j]=qnorm(ptruncnorm(data_testing_Other_subset01$cheapest_trun_norm_testing_other[j],a=a_cheapest_other,b=b_cheapest_other,mean=(a_cheapest_other+b_cheapest_other)/2,sd=(b_cheapest_other-a_cheapest_other)/6),0,1,lower.tail = TRUE)
    v1_vehpower_testing_other[j]=qnorm(ptruncnorm(data_testing_Other_subset01$P2_P9_trun_norm_testing_other[j],a=a_P2_P9_other,b=b_P2_P9_other,mean=(b_P2_P9_other+a_P2_P9_other)/2,sd=(b_P2_P9_other-a_P2_P9_other)/6),0,1,lower.tail = TRUE)
    v1_channel_testing_other[j]=qnorm(ptruncnorm(data_testing_Other_subset01$B_L_trun_norm_testing_other[j],a=a_B_L_other,b=b_B_L_other,mean=(a_B_L_other+b_B_L_other)/2,sd=(b_B_L_other-a_B_L_other)/6),0,1,lower.tail = TRUE)  
    # matrix baris dari v*  
    v_mat_other_Normal_testing[j,1]<-v1_malus_testing_other[j]
    v_mat_other_Normal_testing[j,2]<-v1_cheapest_testing_other[j]
    v_mat_other_Normal_testing[j,3]<-v1_vehpower_testing_other[j]
    v_mat_other_Normal_testing[j,4]<-v1_channel_testing_other[j]
    vmat_other_Normal_testing=t(v_mat_other_Normal_testing)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_damage_training_inv_gaussian  
    a_other_testing_Normal=1-t(r_other_training_Normal)%*%inv(R2_other_training_Normal)%*%r_other_training_Normal
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b_damage_training_inv_gaussian
    b_other_testing_Normal[j]=t(r_other_training_Normal)%*%inv(R2_other_training_Normal)%*%(vmat_other_Normal_testing[,j])
    
    # pdf(y) yang digunakan disini adalah inverse gaussian 
    random_Normal=rnorm(nrow(data_testing_Other_subset01),mean = 848.94,sd=1081.9)
    
    y_other_testing_Normal[j]=qnorm(pnorm(random_Normal[j],mean = 848.94,sd=1081.9),0,1)
    # q disini merupakan pdf dari y|x1,x
    q_other_testing_Normal[j]=(dnorm(random_Normal[j],mean = 848.94,sd=1081.9 ))*exp(-0.5*(((y_other_testing_Normal[j]-b_other_testing_Normal[j])^2/a_other_testing_Normal)-(y_other_testing_Normal[j])^2))*(a_other_testing_Normal)^(-0.5)
    
    # distribusi proposal 
    pl_testing_Normal_other[j]=1.024*dnorm(random_Normal[j],mean =850,sd=1091)    
    u=runif(1,0)
    if(u<q_other_testing_Normal[j]/pl_testing_Normal_other[j]){
      y_simull_testing_Normal_other=c(y_simull_testing_Normal_other,random_Normal)
      mean_testing_Normal_other[j]=mean(y_simull_testing_Normal_other)
    }
  }
}

# plot density dari training 
plot(density(mean_testing_Normal_other))

# Uji Normalitas 
shapiro.test(mean_testing_Normal_other)

MSE_copula_other_testing_Normal=sum((data_testing_Other_subset$Sev_Other-mean_testing_Normal_other)^2)/nrow(data_testing_Other_subset)

# copula training Other dengan Sev_other dan VehAge

u_training_other_inv_gaussian=pobs(data_training_Other_subset[c(24,6)])
fit.copula_training_other=fitCopula(normalCopula(dim = 2,dispstr = "un"),u_training_other_inv_gaussian,method="ml")
summary(fit.copula_training_other) 

R_other_training_inv_gaussian=matrix(1,nrow=2,ncol=2)
R_other_training_inv_gaussian[1,2]=fit.copula_training_other@estimate[[1]]
R_other_training_inv_gaussian[2,1]=fit.copula_training_other@estimate[[1]]

R2_other_training_inv_gaussian=R_other_training_inv_gaussian[1:1,1:1]
r_other_training_inv_gaussian=R_other_training_inv_gaussian[1:1,2]

v1_VehAge_training_other=NULL
v_mat_VehAge=matrix(NA,ncol=1,nrow = nrow(data_training_Other_subset))
b_other_training_inv_gaussian=NULL
q_other_training_inv_gaussian=NULL
pl_training_inv_gaussian_other=NULL
y_simull_training_inv_gaussian_other=NULL
y_other_training_inv_gaussian=NULL
mean_training_inv_gaussian_other=NULL
acceptance_rates_other=NULL
set.seed(100)
for(j in 1:nrow(data_training_Other_subset)){
  y_simull_training_inv_gaussian_other=NULL
  sampel_yang_diterima = 0 
  total_sampel = 0 
  for (i in 1:100){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2 # data ke 15 dan 33
    v1_VehAge_training_other[j]=qnorm(pnorm(data_training_Other_subset$VehAge[j],mean = 7.73, sd=4.5856),0,1,lower.tail = TRUE)
    # matrix baris dari v*  
    v_mat_VehAge[j,1]<-v1_VehAge_training_other[j]
    vmat_VehAge_training=t(v_mat_VehAge)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_damage_training_inv_gaussian  
    a_VehAge_training=1-t(r_other_training_inv_gaussian)%*%(R2_other_training_inv_gaussian)%*%r_other_training_inv_gaussian
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b_damage_training_inv_gaussian
    b_other_training_inv_gaussian[j]=t(r_other_training_inv_gaussian)%*%(R2_other_training_inv_gaussian)%*%(vmat_VehAge_training[,j])
    
    # pdf(y) yang digunakan disini adalah inverse gaussian 
    random_inv_gaussian=rinvgauss(nrow(data_training_Other_subset),mean =848.94,shape=522.69)
    
    y_other_training_inv_gaussian[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean =848.94,shape=522.69),0,1)
    # q disini merupakan pdf dari y|x1,x
    q_other_training_inv_gaussian[j]=(dinvgauss(random_inv_gaussian[j],mean =848.94,shape=522.69 ))*exp(-0.5*(((y_other_training_inv_gaussian[j]-b_other_training_inv_gaussian[j])^2/a_VehAge_training)-(y_other_training_inv_gaussian[j])^2))*(a_VehAge_training)^(-0.5)
    
    # distribusi proposal 
    pl_training_inv_gaussian_other[j]=1*dinvgauss(random_inv_gaussian[j],mean =850,shape=460)    
    u=runif(1,0)
    if(u<q_other_training_inv_gaussian[j]/pl_training_inv_gaussian_other[j]){
      y_simull_training_inv_gaussian_other=c(y_simull_training_inv_gaussian_other,random_inv_gaussian)
      mean_training_inv_gaussian_other[j]=mean(y_simull_training_inv_gaussian_other)
      sampel_yang_diterima = sampel_yang_diterima + 1
    }
    total_sampel = total_sampel + 1
  }
  acceptance_rates_other[j] <- sampel_yang_diterima / total_sampel
}
sum(acceptance_rates_other==1)/nrow(data_training_Other_subset)

plot(density(pl_training_inv_gaussian_other),col="blue",main="Plot Densitas Distribusi Target VS Distribusi Proposal \n Copula Inverse Gaussian-Normal Other")
    lines(density(q_other_training_inv_gaussian),col="red")

# plot density dari training 
plot(density(mean_training_inv_gaussian_other))

# uji normalitas 
shapiro.test(mean_training_inv_gaussian_other)# H0 diterima

# copula testing 
# sev_other dan VehAge

v1_VehAge_testing=NULL
v_mat_other_testing=matrix(NA,ncol=1,nrow = nrow(data_testing_sev_subset_Damage))
b_other_testing_inv_gaussian=NULL
q_other_testing_inv_gaussian=NULL
pl_testing_inv_gaussian_other=NULL
y_simull_testing_inv_gaussian_other=NULL
y_other_testing_inv_gaussian=NULL
mean_testing_inv_gaussian_other=NULL
set.seed(100)
for(j in 1:nrow(data_testing_Other_subset)){
  y_simull_testing_inv_gaussian_other=NULL
  for (i in 1:450){
#ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_VehAge_testing[j]=qnorm(pnorm(data_testing_Other_subset$VehAge[j],mean = 7.73, sd=4.5856),0,1,lower.tail = TRUE)
  
    # matrix baris dari v*  
    v_mat_other_testing[j]<- v1_VehAge_testing[j]
    vmat_other_testing=t(v_mat_other_testing)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_damage 
    a_other_testing_inv_gaussian= 1-t(r_other_training_inv_gaussian)%*%(R2_other_training_inv_gaussian)%*%r_other_training_inv_gaussian
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
    b_other_testing_inv_gaussian[j]= t(r_other_training_inv_gaussian)%*%(R2_other_training_inv_gaussian)%*%(vmat_other_testing[,j])
    
    random_inv_gaussian=rinvgauss(nrow(data_testing_Other_subset),mean =848.94,shape=522.69)

    y_other_testing_inv_gaussian[j]= qnorm(pinvgauss(random_inv_gaussian[j],mean =848.94,shape=522.69),0,1)
    
    # q disini merupakan pdf dari y|x1,x
    q_other_testing_inv_gaussian[j]=(dinvgauss(random_inv_gaussian[j],mean =848.94,shape=522.69 ))*exp(-0.5*(((y_other_testing_inv_gaussian[j]-b_other_testing_inv_gaussian[j])^2/a_other_testing_inv_gaussian)-(y_other_testing_inv_gaussian[j])^2))*(a_other_testing_inv_gaussian)^(-0.5)
    
    # distribusi proposal 
    pl_testing_inv_gaussian_other[j]=1*dinvgauss(random_inv_gaussian[j],mean =850,shape=460)    
    u=runif(1,0)
    if(u< q_other_testing_inv_gaussian[j]/pl_testing_inv_gaussian_other[j]){
      y_simull_testing_inv_gaussian_other= c(y_simull_testing_inv_gaussian_other,random_inv_gaussian)
      mean_testing_inv_gaussian_other[j]=mean(y_simull_testing_inv_gaussian_other)
    }
  }
}  
mean_testing_inv_gaussian_other
plot(density(mean_testing_inv_gaussian_other),main="Plot Densitas Prediksi \n Copula Inverse Gaussian-Normal Other ")
# uji normalitas 
shapiro.test(mean_testing_inv_gaussian_other)

MSE_copula_other_testing_inv_gaussian=sum((data_testing_Other_subset$Sev_Other-mean_testing_inv_gaussian_other)^2)/nrow(data_testing_Other_subset)
MSE_copula_other_testing_inv_gaussian


#--------------------------------------------------------------------------------------------------------------------------------------------------
# Model frekuensi untuk Other
mod1_freq_pois_other=glm(Other~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = poisson("log"),data = data_training)
summary(mod1_freq_pois_other)
step_mod1_freq_other=stepAIC(mod1_freq_pois_other,direction = "both")
summary(step_mod1_freq_other)

#predict 
y_hat_freq_Poisson_other=predict(step_mod1_freq_other,data_testing_Other_subset,type = "response")

# Model pure premium other

pure_prem_GammaPoisson_other=y_hat_Other_Gamma*y_hat_freq_Poisson_other
pure_prem_GaussianPoisson_other=y_hat_Other_Gaussian*y_hat_freq_Poisson_other
pure_prem_GaussianPoisson_other_log=y_hat_Other_Gaussian_log*y_hat_freq_Poisson_other
pure_prem_InverseGaussianPoisson_other=y_hat_Other_Inverse_Gaussian*y_hat_freq_Poisson_other
pure_prem_InverseGaussianPoisson_other_copula=mean_testing_inv_gaussian_other*y_hat_freq_Poisson_other
pure_prem_NormalPoisson_other_copula=mean_testing_Normal_other*y_hat_freq_Poisson_other


#MSE 
S_other=data_testing_Other_subset$Sev_Other*data_testing_Other_subset$Other
MSE_other_GammaPoisson=sum((S_other-pure_prem_GammaPoisson_other)^2)/nrow(data_testing_Other_subset)
MSE_other_GammaPoisson

MSE_other_GaussianPoisson=sum((S_other-pure_prem_GaussianPoisson_other)^2)/nrow(data_testing_Other_subset)
MSE_other_GaussianPoisson

MSE_other_GaussianPoisson_log=sum((S_other-pure_prem_GaussianPoisson_other_log)^2)/nrow(data_testing_Other_subset)
MSE_other_GaussianPoisson_log

MSE_other_InverseGaussianPoisson=sum((S_other-pure_prem_InverseGaussianPoisson_other)^2)/nrow(data_testing_Other_subset)
MSE_other_InverseGaussianPoisson

MSE_other_InverseGaussianPoisson_copula=sum((S_other-pure_prem_InverseGaussianPoisson_other_copula)^2)/nrow(data_testing_Other_subset)
MSE_other_InverseGaussianPoisson_copula

MSE_other_NormalPoisson_copula=sum((S_other-pure_prem_NormalPoisson_other_copula)^2)/nrow(data_testing_Other_subset)
MSE_other_NormalPoisson_copula
# Boxplot
Sev_Other_testing=data.frame(cbind(data_testing_Other_subset$Sev_Other,y_hat_Other_Gamma,y_hat_Other_Gaussian,y_hat_Other_Gaussian_log,
                                  y_hat_Other_Inverse_Gaussian,mean_testing_inv_gaussian_other))
colnames(Sev_Other_testing)<-c("data asli","y hat Gamma","y hat Gaussian","y hat Gaussian log","y hat \nInverse Gaussian","y hat Inverse \nGaussian-Normal")

df_other <- tidyr::gather(Sev_Other_testing, key = "Model", value = "Predicted_Values")
mean_values_other <- aggregate(Predicted_Values ~ Model, data = df_other, FUN = mean)

ggplot(df_other, aes(x = Model, y = Predicted_Values)) +
  geom_boxplot()+
  geom_point(data = mean_values_other, aes(x = Model, y = Predicted_Values), col = "gray", pch = 19)+
  geom_text(data = mean_values_other, aes(x = Model, y = Predicted_Values, label = round(Predicted_Values, 2)),
            vjust = -0.5, hjust = 0.5, size = 3, col = "black")+
  labs(title = "Boxplot Model Besar Klaim Other", 
       x = "Models", y = "Predicted Values") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Boxplot Premi Murni other

premi_murni_other=data.frame(cbind(S_other,pure_prem_GammaPoisson_other,pure_prem_GaussianPoisson_other,pure_prem_GaussianPoisson_other_log
                                    ,pure_prem_InverseGaussianPoisson_other,pure_prem_InverseGaussianPoisson_other_copula,pure_prem_NormalPoisson_other_copula))
colnames(premi_murni_other) <- c("Data", "Pois &\n Gamma", "Pois & \nNormal", "Pois & \nNormal_log","Pois & Inverse \nGaussian"
                                  ,"Pois & inverse\n Gaussian- truncated \nGaussian-Normal", "Pois & Normal-truncated \n Gaussian")
df_premi_murni_other <- tidyr::gather(premi_murni_other, key = "Model", value = "Predicted_Values")
df_premi_murni_other$Model <- factor(df_premi_murni_other$Model, levels = colnames(premi_murni_other))
mean_premi_murni_other <- aggregate(Predicted_Values ~ Model, data = df_premi_murni_other, FUN = mean)

ggplot(df_premi_murni_other, aes(x = Model, y = Predicted_Values)) +
  geom_boxplot()+
  geom_point(data = mean_premi_murni_other, aes(x = Model, y = Predicted_Values), col = "gray", pch = 19)+
  geom_text(data = mean_premi_murni_other, aes(x = Model, y = Predicted_Values, label = round(Predicted_Values, 4)),
            vjust = -0.5, hjust = 0.5, size = 3, col = "black")+
  labs(title = "Boxplot Premi Murni Other", 
       x = "Models", y = "Predicted Values") +
  scale_y_continuous(limits = c(0,100),expand = c(0.1, 0))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


############################################################################################################################################################################################################
#Pure premium Sev_Fire
data_training_Fire_subset=subset(data_training,Sev_Fire>0)
data_testing_Fire_subset=data_testing
summary(data_training_Fire_subset)

# Mengubah data_kategorikal
data_VehGas_Fire=mapvalues(data_training_Fire_subset$VehGas, from = c("Diesel","Regular")
                           , to = c("1", "2"))
data_VehGas_Fire=data.frame(as.factor(data_VehGas_Fire))
frequency_table_VehGas_Fire <- table(data_VehGas_Fire)

PR_VehGas_Fire=NULL
for( i in 1:2){ 
  PR_VehGas_Fire[i]=frequency_table_VehGas_Fire[i]/(frequency_table_VehGas_Fire[1]+frequency_table_VehGas_Fire[2])
}
PR_VehGas_Fire
VehGas <- c("Diesel","Regular")

Cumprob_VehGas_Fire=data.frame(cbind(VehGas,PR_VehGas_Fire))
Prob_VehGas_Fire=Cumprob_VehGas_Fire[order(Cumprob_VehGas_Fire$PR_VehGas_Fire,decreasing = TRUE),]
Prob_VehGas_Fire$PR_VehGas_Fire=as.numeric(Prob_VehGas_Fire$PR_VehGas_Fire)


data_BonusMalus_Fire=mapvalues(data_training_Fire_subset$BonusMalus, from = c("bonus","malus")
                               , to = c("1", "2"))
data_BonusMalus_Fire=data.frame(as.factor(data_BonusMalus_Fire))
frequency_table_BonusMalus_Fire <- table(data_BonusMalus_Fire)
PR_BonusMalus_Fire=NULL
for( i in 1:2){ 
  PR_BonusMalus_Fire[i]=frequency_table_BonusMalus_Fire [i]/(frequency_table_BonusMalus_Fire [1]+frequency_table_BonusMalus_Fire [2])
}
PR_BonusMalus_Fire
BonusMalus <- c("bonus","malus")
Cumprob_BonusMalus_Fire=data.frame(cbind(BonusMalus,PR_BonusMalus_Fire))
Prob_BonusMalus_Fire=Cumprob_BonusMalus_Fire[order(Cumprob_BonusMalus_Fire$PR_BonusMalus_Fire,decreasing = TRUE),]
Prob_BonusMalus_Fire$PR_BonusMalus_Fire=as.numeric(Prob_BonusMalus_Fire$PR_BonusMalus_Fire)

a_bonus=0
b_bonus=Prob_BonusMalus_Fire$PR_BonusMalus_Fire[1]
a_malus=Prob_BonusMalus_Fire$PR_BonusMalus_Fire[1]
b_malus=Prob_BonusMalus_Fire$PR_BonusMalus_Fire[1] + Prob_BonusMalus_Fire$PR_BonusMalus_Fire[2]

a_regular=0
b_regular=Prob_VehGas_Fire$PR_VehGas_Fire[1]
a_diesel=Prob_VehGas_Fire$PR_VehGas_Fire[1]
b_diesel=Prob_VehGas_Fire$PR_VehGas_Fire[1]+Prob_VehGas_Fire$PR_VehGas_Fire[2]

alpha_bonus=Prob_BonusMalus_Fire$PR_BonusMalus_Fire[1]*((Prob_BonusMalus_Fire$PR_BonusMalus_Fire[1]*(1-Prob_BonusMalus_Fire$PR_BonusMalus_Fire[1])/var(Prob_BonusMalus_Fire$PR_BonusMalus_Fire))-1)
beta_bonus=(1-Prob_BonusMalus_Fire$PR_BonusMalus_Fire[1])*((Prob_BonusMalus_Fire$PR_BonusMalus_Fire[1]*(1-Prob_BonusMalus_Fire$PR_BonusMalus_Fire[1])/var(Prob_BonusMalus_Fire$PR_BonusMalus_Fire))-1)
alpha_malus=Prob_BonusMalus_Fire$PR_BonusMalus_Fire[2]*((Prob_BonusMalus_Fire$PR_BonusMalus_Fire[2]*(1-Prob_BonusMalus_Fire$PR_BonusMalus_Fire[2])/var(Prob_BonusMalus_Fire$PR_BonusMalus_Fire))-1)
beta_malus=(1-Prob_BonusMalus_Fire$PR_BonusMalus_Fire[2])*((Prob_BonusMalus_Fire$PR_BonusMalus_Fire[2]*(1-Prob_BonusMalus_Fire$PR_BonusMalus_Fire[2])/var(Prob_BonusMalus_Fire$PR_BonusMalus_Fire))-1)


# random truncated normal 
set.seed(105)
bonus_truncnorm=rtruncnorm(nrow(data_training_Fire_subset),a=a_bonus,b=b_bonus,mean = (a_bonus+b_bonus)/2,sd=(b_bonus-a_bonus)/6)
set.seed(106)
malus_truncnorm=rtruncnorm(nrow(data_training_Fire_subset),a=a_malus,b=b_malus,mean = (a_malus+b_malus)/2,sd=(b_malus-a_malus)/6)
set.seed(107)
regular_truncnorm=rtruncnorm(nrow(data_training_Fire_subset),a=a_regular,b=b_regular,mean = (a_regular+b_regular)/2,sd=(b_regular-a_regular)/6)
set.seed(108)
diesel_truncnorm=rtruncnorm(nrow(data_training_Fire_subset),a=a_diesel,b=b_diesel,mean = (a_diesel+b_diesel)/2,sd=(b_diesel-a_diesel)/6)

data_training_Fire_subset=cbind(data_training_Fire_subset,bonus_truncnorm,malus_truncnorm,regular_truncnorm,diesel_truncnorm)


set.seed(100)
#family=Gamma(link="log")
mod1_Fire=glm(Sev_Fire~Channel+DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehGas,family = Gamma(link="log"),data = data_training_Fire_subset)
summary(mod1_Fire) 
step_mod1_Fire=stepAIC(mod1_Fire,direction = "both")
summary(step_mod1_Fire)

#family=Gaussian(link="identity")
mod2_Fire=glm(Sev_Fire~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = gaussian(link="identity"),data = data_training_Fire_subset)
summary(mod2_Fire) 
step_mod2_Fire=stepAIC(mod2_Fire,direction = "both")
summary(step_mod2_Fire)

#family=Gaussian(link="log")
mod2_Fire_log=glm(Sev_Fire~DrivAge+DrivGender+PayFreq+VehAge+VehGas+Channel,family = gaussian(link="log"),data = data_training_Fire_subset)
summary(mod2_Fire_log)  
step_mod2_Fire_log=stepAIC(mod2_Fire_log,direction = "both")
summary(step_mod2_Fire_log)

#family=inverse.gaussian(link="log")
mod3_Fire=glm(Sev_Fire~PayFreq,family =inverse.gaussian(link="log"),data = data_training_Fire_subset)
summary(mod3_Fire) 
step_mod3_Fire=stepAIC(mod3_Fire,direction = "both")
summary(step_mod3_Fire)

# Predict 
y_hat_Fire_Gamma=predict(step_mod1_Fire,data_testing_Fire_subset,type="response")
MSE_Fire_Gamma=sum((data_testing_Fire_subset$Sev_Fire-y_hat_Fire_Gamma)^2)/nrow(data_testing_Fire_subset)
MSE_Fire_Gamma

y_hat_Fire_Gaussian=predict(step_mod2_Fire,data_testing_Fire_subset,type="response")
MSE_Fire_Gaussian=sum((data_testing_Fire_subset$Sev_Fire-y_hat_Fire_Gaussian)^2)/nrow(data_testing_Fire_subset)
MSE_Fire_Gaussian

y_hat_Fire_Gaussian_log=predict(step_mod2_Fire_log,data_testing_Fire_subset,type="response")
MSE_Fire_Gaussian_log=sum((data_testing_Fire_subset$Sev_Fire-y_hat_Fire_Gaussian_log)^2)/nrow(data_testing_Fire_subset)
MSE_Fire_Gaussian_log

y_hat_Fire_Inverse_Gaussian=predict(step_mod3_Fire,data_testing_Fire_subset,type="response")
MSE_Fire_Inverse_Gaussian=sum((data_testing_Fire_subset$Sev_Fire-y_hat_Fire_Inverse_Gaussian)^2)/nrow(data_testing_Fire_subset)
MSE_Fire_Inverse_Gaussian

# data_training model copula 
# Sev_theft dan DrivAge, BonusMalus, dan VehGas
u_training_fire=pobs(data_training_Fire_subset[c(26,12,29,30)])
fit.copula_training_fire=fitCopula(normalCopula(dim = 4,dispstr = "un"),u_training_fire,method="ml")
summary(fit.copula_training_fire) 

R_fire_training=matrix(1,nrow=4,ncol=4)
R_fire_training[1,2]=fit.copula_training_fire@estimate[[1]]
R_fire_training[1,3]=fit.copula_training_fire@estimate[[2]]
R_fire_training[1,4]=fit.copula_training_fire@estimate[[3]]
R_fire_training[2,3]=fit.copula_training_fire@estimate[[4]]
R_fire_training[2,4]=fit.copula_training_fire@estimate[[5]]
R_fire_training[3,4]=fit.copula_training_fire@estimate[[6]]

segitiga_bawah <- lower.tri(R_fire_training,diag = TRUE)
R_fire_training[segitiga_bawah]=0
t_R_fire_training=t(R_fire_training)
R_fire_training=R_fire_training+t_R_fire_training
diag(R_fire_training)=1

R2_fire_training=R_fire_training[1:3,1:3]
r_fire_training=R_fire_training[1:3,4]

v1_DrivAge_training_fire=NULL
v2_Malus_training_fire=NULL
v3_Regular_training_fire=NULL
v_mat_fire_training=matrix(NA,ncol=3,nrow = nrow(data_training_Fire_subset))
b_fire_training_normal=NULL
q_fire_training_normal=NULL
pl_training_normal_fire=NULL
y_simull_training_normal_fire=NULL
y_fire_training_normal=NULL
mean_training_normal_fire=NULL
acceptance_rates_fire=NULL
set.seed(100)
for(j in 1:nrow(data_training_Fire_subset)){
  y_simull_training_normal_fire=NULL
  sampel_yang_diterima = 0 
  total_sampel = 0 
  for (i in 1:100){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2 # data ke 15 dan 33
    v1_DrivAge_training_fire[j]=qnorm(pgamma(data_training_Fire_subset$DrivAge[j],shape =10.305,scale = 3.6838 ),0,1,lower.tail = TRUE)
    v2_Malus_training_fire[j]=qnorm(ptruncnorm(data_training_Fire_subset$malus_truncnorm[j],a=a_malus,b=b_malus,mean = (a_malus+b_malus)/2,sd=(b_malus-a_malus)/6))
    v3_Regular_training_fire[j]=qnorm(ptruncnorm(data_training_Fire_subset$regular_truncnorm[j],a=a_regular,b=b_regular,mean = (a_regular+b_regular)/2,sd=(b_regular-a_regular)/6))
    # matrix baris dari v*  
    v_mat_fire_training[j,1]<-v1_DrivAge_training_fire[j]
    v_mat_fire_training[j,2]<-v2_Malus_training_fire[j]
    v_mat_fire_training[j,3]<-v3_Regular_training_fire[j]
    vmat_fire_training=t(v_mat_fire_training)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_fire_training
    a_fire_training=1-t(r_fire_training)%*%inv(R2_fire_training)%*%r_fire_training
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b_fire_training_normal
    b_fire_training_normal[j]=t(r_fire_training)%*%inv(R2_fire_training)%*%(vmat_fire_training[,j])
    
    # pdf(y) yang digunakan disini adalah inverse gaussian 
    random_normal=rnorm(nrow(data_training_Fire_subset),mean =2723.0,sd=3111.5)
    
    y_fire_training_normal[j]=qnorm(pnorm(random_normal[j],mean =2723.0,sd=3111.5),0,1)
    
    # q disini merupakan pdf dari y|x1,x
    q_fire_training_normal[j]=(dnorm(random_normal[j],mean =2723.0,sd=3111.5 ))*exp(-0.5*(((y_fire_training_normal[j]-b_fire_training_normal[j])^2/a_fire_training)-(y_fire_training_normal[j])^2))*(a_fire_training)^(-0.5)
    
    # distribusi proposal 
    pl_training_normal_fire[j]=1*dnorm(random_normal[j],mean =2730,sd=2999)
    
    u=runif(1,0,1)
    if(u<q_fire_training_normal[j]/pl_training_normal_fire[j]){
      y_simull_training_normal_fire=c(y_simull_training_normal_fire,random_normal)
      mean_training_normal_fire[j]=mean(y_simull_training_normal_fire)
      sampel_yang_diterima = sampel_yang_diterima + 1
    }
    total_sampel = total_sampel + 1
  }
  acceptance_rates_fire[j] <- sampel_yang_diterima / total_sampel
}

sum(acceptance_rates_fire==1)/nrow(data_training_Fire_subset)
plot(density(q_fire_training_normal),col="red", main="Plot Densitas Distribusi Proposal VS Target Fire")
lines(density(pl_training_normal_fire),col="blue")

mean_training_normal_fire

plot(density(mean_training_inv_gaussian_other))

# uji normalitas 
shapiro.test(mean_training_normal_fire)# H0 diterima

# model testing 
set.seed(105)
bonus_truncnorm_testing=rtruncnorm(nrow(data_testing_Fire_subset),a=a_bonus,b=b_bonus,mean = (a_bonus+b_bonus)/2,sd=(b_bonus-a_bonus)/6)
set.seed(106)
malus_truncnorm_testing=rtruncnorm(nrow(data_testing_Fire_subset),a=a_malus,b=b_malus,mean = (a_malus+b_malus)/2,sd=(b_malus-a_malus)/6)
set.seed(107)
regular_truncnorm_testing=rtruncnorm(nrow(data_testing_Fire_subset),a=a_regular,b=b_regular,mean = (a_regular+b_regular)/2,sd=(b_regular-a_regular)/6)
set.seed(108)
diesel_truncnorm_testing=rtruncnorm(nrow(data_testing_Fire_subset),a=a_diesel,b=b_diesel,mean = (a_diesel+b_diesel)/2,sd=(b_diesel-a_diesel)/6)

data_testing_Fire_subset=cbind(data_testing_Fire_subset,bonus_truncnorm_testing,malus_truncnorm_testing
                               ,regular_truncnorm_testing,diesel_truncnorm_testing)

v1_DrivAge_testing_fire=NULL
v2_Malus_testing_fire=NULL
v3_Regular_testing_fire=NULL
v_mat_fire_testing=matrix(NA,ncol=3,nrow = nrow(data_testing_Fire_subset))
b_fire_testing_normal=NULL
q_fire_testing_normal=NULL
pl_testing_normal_fire=NULL
y_simull_testing_normal_fire=NULL
y_fire_testing_normal=NULL
mean_testing_normal_fire=NULL
set.seed(100)
for(j in 1:nrow(data_testing_Fire_subset)){
  y_simull_testing_normal_fire=NULL
  for (i in 1:450){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
  v1_DrivAge_testing_fire[j]=qnorm(pgamma(data_testing_Fire_subset$DrivAge[j],shape =10.305,scale = 3.6838 ),0,1,lower.tail = TRUE)
  v2_Malus_testing_fire[j]=qnorm(ptruncnorm(data_testing_Fire_subset$malus_truncnorm_testing[j],a=a_malus,b=b_malus,mean = (a_malus+b_malus)/2,sd=(b_malus-a_malus)/6))
  v3_Regular_testing_fire[j]=qnorm(ptruncnorm(data_testing_Fire_subset$regular_truncnorm_testing[j],a=a_regular,b=b_regular,mean = (a_regular+b_regular)/2,sd=(b_regular-a_regular)/6))
  
    # matrix baris dari v*  
  v_mat_fire_testing[j,1]<-v1_DrivAge_testing_fire[j]
  v_mat_fire_testing[j,2]<-v2_Malus_testing_fire[j]
  v_mat_fire_testing[j,3]<-v3_Regular_testing_fire[j]
  vmat_fire_testing=t(v_mat_fire_testing)

    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_fire_testing
  a_fire_testing=1-t(r_fire_training)%*%inv(R2_fire_training)%*%r_fire_training
  
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
  b_fire_testing_normal[j]= t(r_fire_training)%*%inv(R2_fire_training)%*%(vmat_fire_testing[,j])

  random_normal=rnorm(nrow(data_testing_Fire_subset),mean =2723.0,sd=3111.5)
  
  y_fire_testing_normal[j]=qnorm(pnorm(random_normal[j],mean =2723.0,sd=3111.5),0,1)
  
    # q disini merupakan pdf dari y|x1,x
  q_fire_testing_normal[j]=(dnorm(random_normal[j],mean =2723.0,sd=3111.5 ))*exp(-0.5*(((y_fire_testing_normal[j]-b_fire_testing_normal[j])^2/a_fire_testing)-(y_fire_testing_normal[j])^2))*(a_fire_testing)^(-0.5)
  
    # distribusi proposal 
  pl_testing_normal_fire[j]=1*dnorm(random_normal[j],mean =2730,sd=2999)
  
  u=runif(1,0)
    if(u< q_fire_testing_normal[j]/pl_testing_normal_fire[j]){
      y_simull_testing_normal_fire= c( y_simull_testing_normal_fire,random_normal)
      mean_testing_normal_fire[j]=mean( y_simull_testing_normal_fire)
    }
  }
}  
mean_testing_normal_fire
plot(density(mean_testing_normal_fire))
# uji normalitas 
shapiro.test(mean_testing_normal_fire)

MSE_copula_fire_testing_normal=sum((data_testing_Fire_subset$Sev_Fire-mean_testing_normal_fire)^2)/nrow(data_testing_Fire_subset)
MSE_copula_fire_testing_normal


#Frekuensi 
set.seed(100)
mod1_freq_pois_fire=glm(Fire~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = poisson("log"),data = data_training)
summary(mod1_freq_pois_fire)
mod2_freq_pois_fire=stepAIC(mod1_freq_pois_fire,direction = "both")
summary(mod2_freq_pois_fire)

#predict 
y_hat_freq_Poisson_fire=predict(mod2_freq_pois_fire,data_testing_Fire_subset,type = "response")
y_hat_freq_Poisson_fire

# pure premium fire
pure_prem_GammaPoisson_fire=y_hat_Fire_Gamma*y_hat_freq_Poisson_fire
pure_prem_GaussianPoisson_fire=y_hat_Fire_Gaussian*y_hat_freq_Poisson_fire
pure_prem_GaussianPoisson_fire_log=y_hat_Fire_Gaussian_log*y_hat_freq_Poisson_fire
pure_prem_InverseGaussianPoisson_fire=y_hat_Fire_Inverse_Gaussian*y_hat_freq_Poisson_fire
pure_prem_copula_NormalPoisson_fire=mean_testing_normal_fire*y_hat_freq_Poisson_fire
#MSE
S_fire=data_testing_Fire_subset$Sev_Fire*data_testing_Fire_subset$Fire

MSE_fire_GammaPoisson=sum((S_fire-pure_prem_GammaPoisson_fire)^2)/nrow(data_testing_Fire_subset)
MSE_fire_GammaPoisson

MSE_fire_GaussianPoisson=sum((S_fire-pure_prem_GaussianPoisson_fire)^2)/nrow(data_testing_Fire_subset)
MSE_fire_GaussianPoisson

MSE_fire_GaussianPoisson_log=sum((S_fire-pure_prem_GaussianPoisson_fire_log)^2)/nrow(data_testing_Fire_subset)
MSE_fire_GaussianPoisson_log

MSE_fire_InverseGaussianPoisson=sum((S_fire-pure_prem_InverseGaussianPoisson_fire)^2)/nrow(data_testing_Fire_subset)
MSE_fire_InverseGaussianPoisson

MSE_fire_NormalPoisson_copula=sum((S_fire-pure_prem_copula_NormalPoisson_fire)^2)/nrow(data_testing_Fire_subset)
MSE_fire_NormalPoisson_copula

#boxplot
Sev_fire_testing=data.frame(cbind(data_testing_Fire_subset$Sev_Fire,y_hat_Fire_Gamma,y_hat_Fire_Gaussian,y_hat_Fire_Gaussian_log,y_hat_Fire_Inverse_Gaussian,mean_testing_normal_fire))
colnames(Sev_fire_testing)<-c("sev_asli","yhat_Gamma","yhat_Gaussian","yhat_Gaussian_log","yhat_invGauss", 
                               "yhat_invGauss-trunNorm")

df_fire<- tidyr::gather(Sev_fire_testing, key = "Model", value = "Predicted_Values")
mean_values_fire<- aggregate(Predicted_Values ~ Model, data = df_fire, FUN = mean)

ggplot(df_fire, aes(x = Model, y = Predicted_Values)) +
  geom_boxplot()+
  geom_point(data = mean_values_fire, aes(x = Model, y = Predicted_Values), col = "gray", pch = 19)+
  geom_text(data = mean_values_fire, aes(x = Model, y = Predicted_Values, label = round(Predicted_Values, 2)),
            vjust = -0.5, hjust = 0.5, size = 3, col = "black")+
  labs(title = "Boxplot Model Besar Klaim Fire", 
       x = "Models", y = "Predicted Values") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


##############################################################################################################
# Sev_Theft
data_training_Theft_subset=subset(data_training,Sev_Theft>0)
data_testing_Theft_subset=(data_testing)
summary(data_training_Theft_subset)

# mengubah data katgorikal menjadi kontinu 
data_area_theft=mapvalues(data_training_Theft_subset$Area, from = c("A7-A10+A12","A2+A3+A4","A5+A6")
                             , to = c("1", "2","3"))
data_area_theft=data.frame(as.factor(data_area_theft))

frequency_table_area_theft <- table(data_area_theft)

PR_area_theft=NULL
for( i in 1:3){ 
  PR_area_theft[i]=frequency_table_area_theft[i]/(frequency_table_area_theft[1]+frequency_table_area_theft[2]+frequency_table_area_theft[3])
}
PR_area_theft

area <- c("A7-A10+A12","A2+A3+A4","A5+A6")
Cumprob_area_theft=data.frame(cbind(area,PR_area_theft))
Prob_area_theft=Cumprob_area_theft[order(Cumprob_area_theft$PR_area_theft,decreasing = TRUE),]
Prob_area_theft$PR_area_theft=as.numeric(Prob_area_theft$PR_area_theft)

# Truncated Gaussian 

a_A5_A6=0
b_A5_A6=Prob_area_theft$PR_area_theft[1]

a_A2_A4=Prob_area_theft$PR_area_theft[1]
b_A2_A4=Prob_area_theft$PR_area_theft[1]+Prob_area_theft$PR_area_theft[2]

a_A7_A12=Prob_area_theft$PR_area_theft[1]+Prob_area_theft$PR_area_theft[2]
b_A7_A12=Prob_area_theft$PR_area_theft[1]+Prob_area_theft$PR_area_theft[2]+Prob_area_theft$PR_area_theft[3]

set.seed(105)
A5_A6_trun_norm_training=rtruncnorm(nrow(data_training_Theft_subset),a=a_A5_A6,b=b_A5_A6,mean = (a_A5_A6+b_A5_A6)/2,sd=(b_A5_A6-a_A5_A6)/6)
set.seed(106)
A2_A4_trun_norm_training=rtruncnorm(nrow(data_training_Theft_subset),a=a_A2_A4,b=b_A2_A4,mean= (a_A2_A4+b_A2_A4)/2,sd=(b_A2_A4-a_A2_A4)/6)
set.seed(107)
A7_A12_trun_norm_training=rtruncnorm(nrow(data_training_Theft_subset),a=a_A7_A12,b=b_A7_A12,mean = (a_A7_A12+b_A7_A12)/2,sd=(b_A7_A12-a_A7_A12)/6)

# beta 
alpha_A5_A6=Prob_area_theft$PR_area_theft[1]*(((Prob_area_theft$PR_area_theft[1]*(1-Prob_area_theft$PR_area_theft[1]))/var(Prob_area_theft$PR_area_theft))-1)
beta_A5_A6=(1-Prob_area_theft$PR_area_theft[1])*(((Prob_area_theft$PR_area_theft[1]*(1-Prob_area_theft$PR_area_theft[1]))/var(Prob_area_theft$PR_area_theft))-1)

alpha_A2_A4=Prob_area_theft$PR_area_theft[2]*(((Prob_area_theft$PR_area_theft[2]*(1-Prob_area_theft$PR_area_theft[2]))/var(Prob_area_theft$PR_area_theft))-1)
beta_A2_A4=(1-Prob_area_theft$PR_area_theft[2])*(((Prob_area_theft$PR_area_theft[2]*(1-Prob_area_theft$PR_area_theft[2]))/var(Prob_area_theft$PR_area_theft))-1)

alpha_A7_A12=Prob_area_theft$PR_area_theft[3]*(((Prob_area_theft$PR_area_theft[3]*(1-Prob_area_theft$PR_area_theft[3]))/var(Prob_area_theft$PR_area_theft))-1)
beta_A7_A12=(1-Prob_area_theft$PR_area_theft[3])*(((Prob_area_theft$PR_area_theft[3]*(1-Prob_area_theft$PR_area_theft[3]))/var(Prob_area_theft$PR_area_theft))-1) 

set.seed(105)
A5_A6_beta_training=rbeta(nrow(data_training_Theft_subset),shape1 = alpha_A5_A6,shape2 = beta_A5_A6)
set.seed(106)
A2_A4_beta_training=rbeta(nrow(data_training_Theft_subset),shape1 = alpha_A2_A4,shape2 = beta_A2_A4)
set.seed(107)
A7_A12_beta_training=rbeta(nrow(data_training_Theft_subset),shape1 = alpha_A7_A12,shape2 = beta_A7_A12)

data_training_Theft_subset=cbind(data_training_Theft_subset,A5_A6_trun_norm_training,A2_A4_trun_norm_training,A7_A12_trun_norm_training,A5_A6_beta_training,A2_A4_beta_training,A7_A12_beta_training)


#family=Gamma(link="log")
mod1_Theft=glm(Sev_Theft~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = Gamma(link="log"),data = data_training_Theft_subset)
summary(mod1_Theft) 
step_mod1_Theft=stepAIC(mod1_Theft,direction = "both")
summary(step_mod1_Theft)

#family=Gaussian(link="identity")
mod2_Theft=glm(Sev_Theft~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = gaussian(link="identity"),data = data_training_Theft_subset)
summary(mod2_Theft) 
step_mod2_Theft=stepAIC(mod2_Theft,direction = "both")
summary(step_mod2_Theft)


#family=Gaussian(link="log")
mod2_Theft_log=glm(Sev_Theft~DrivAge+DrivGender+BonusMalus+PayFreq+VehAge+VehGas+Area+Channel,family = gaussian(link="log"),data = data_training_Theft_subset)
summary(mod2_Theft_log) #Garage,VehPower,Region,VehClass
step_mod2_Theft_log=stepAIC(mod2_Theft_log,direction = "both")
summary(step_mod2_Theft_log)


#family=inverse.gaussian(link="log")
mod3_Theft=glm(Sev_Theft~Area,family = inverse.gaussian(link="log"),data = data_training_Theft_subset)
summary(mod3_Theft) 
step_mod3_Theft=stepAIC(mod3_Theft,direction = "both")
summary(step_mod3_Theft)

#predict
y_hat_Theft_Gamma=predict(step_mod1_Theft,data_testing_Theft_subset,type="response") 
MSE_Theft_Gamma=sum((data_testing_Theft_subset$Sev_Theft-y_hat_Theft_Gamma)^2)/nrow(data_testing_Theft_subset)
MSE_Theft_Gamma

y_hat_Theft_Gaussian=predict(step_mod2_Theft,data_testing_Theft_subset,type="response") 
MSE_Theft_Gaussian=sum((data_testing_Theft_subset$Sev_Theft-y_hat_Theft_Gaussian)^2)/nrow(data_testing_Theft_subset)
MSE_Theft_Gaussian

y_hat_Theft_Gaussian_log=predict(step_mod2_Theft_log,data_testing_Theft_subset,type="response") 
MSE_Theft_Gaussian_log=sum((data_testing_Theft_subset$Sev_Theft-y_hat_Theft_Gaussian_log)^2)/nrow(data_testing_Theft_subset)
MSE_Theft_Gaussian_log

y_hat_Theft_Inverse_Gaussian=predict(step_mod3_Theft,data_testing_Theft_subset,type="response")
MSE_Theft_InverseGaussian=sum((data_testing_Theft_subset$Sev_Theft-y_hat_Theft_Inverse_Gaussian)^2)/nrow(data_testing_Theft_subset)
MSE_Theft_InverseGaussian

#-----------------------------------------------------------------------------------------------------------------------------------------------
# copula training sev theft dengan area dengan truncated normal dan inv gaussian 
u_training_theft_inv_gaussian=pobs(data_training_Theft_subset[c(25,28,29)])
fit.copula_training_theft=fitCopula(normalCopula(dim = 3,dispstr = "un"),u_training_theft_inv_gaussian,method="ml")
summary(fit.copula_training_theft) 

R_theft_training_inv_gaussian=matrix(1,nrow=3,ncol=3)
R_theft_training_inv_gaussian[1,2]=fit.copula_training_theft@estimate[[1]]
R_theft_training_inv_gaussian[1,3]=fit.copula_training_theft@estimate[[2]]
R_theft_training_inv_gaussian[2,3]=fit.copula_training_theft@estimate[[3]]

segitiga_bawah <- lower.tri(R_theft_training_inv_gaussian,diag = TRUE)
R_theft_training_inv_gaussian[segitiga_bawah]=0
t_R_theft=t(R_theft_training_inv_gaussian)
R_theft_training_inv_gaussian=R_theft_training_inv_gaussian+t_R_theft
diag(R_theft_training_inv_gaussian)=1

R2_theft_training_inv_gaussian=R_theft_training_inv_gaussian[1:2,1:2]
r_theft_training_inv_gaussian=R_theft_training_inv_gaussian[1:2,3]

v1_A5_A6_trunc_norm_training=NULL
v2_A2_A4_trunc_norm_training=NULL
v_mat_theft_training_trunc_norm=matrix(NA,ncol=2,nrow = nrow(data_training_Theft_subset))
b_theft_training_inv_gaussian=NULL
q_theft_training_inv_gaussian=NULL
pl_training_inv_gaussian_theft=NULL
y_simull_training_inv_gaussian_theft=NULL
y_theft_training_inv_gaussian=NULL
mean_training_inv_gaussian_theft=NULL
acceptance_rates_theft=NULL
set.seed(100)
for(j in 1:nrow(data_training_Theft_subset)){
  y_simull_training_inv_gaussian_theft=NULL
  sampel_yang_diterima = 0 
  total_sampel = 0 
  for (i in 1:100){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
  v1_A5_A6_trunc_norm_training[j]=qnorm(ptruncnorm(data_training_Theft_subset$A5_A6_trun_norm_training[j],a=a_A5_A6,b=b_A5_A6,mean = (a_A5_A6+b_A5_A6)/2,sd=(b_A5_A6-a_A5_A6)/6),0,1,lower.tail = TRUE)
  v2_A2_A4_trunc_norm_training[j]=qnorm(ptruncnorm(data_training_Theft_subset$A2_A4_trun_norm_training[j],a=a_A2_A4,b=b_A2_A4,mean = (a_A2_A4+b_A2_A4)/2,sd=(b_A2_A4-a_A2_A4)/6),0,1,lower.tail = TRUE)
  
    # matrix baris dari v*  
  v_mat_theft_training_trunc_norm[j,1]<-v1_A5_A6_trunc_norm_training[j]
  v_mat_theft_training_trunc_norm[j,2]<-v2_A2_A4_trunc_norm_training[j]
  vmat_theft_training_trunc_norm=t(v_mat_theft_training_trunc_norm)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_damage_training_inv_gaussian  
  a_theft_training_inv_gaussian= 1-t(r_theft_training_inv_gaussian)%*%inv(R2_theft_training_inv_gaussian)%*%r_theft_training_inv_gaussian
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b_damage_training_inv_gaussian
  
  b_theft_training_inv_gaussian[j]= t(r_theft_training_inv_gaussian)%*%inv(R2_theft_training_inv_gaussian)%*%(vmat_theft_training_trunc_norm[,j])
    
    # pdf(y) yang digunakan disini adalah inverse gaussian 
  random_inv_gaussian=rinvgauss(nrow(data_training_Theft_subset),mean =1952.7,shape=909.91)
  
  y_theft_training_inv_gaussian[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean =1952.7,shape=909.91),0,1)
    
    # q disini merupakan pdf dari y|x1,x
  q_theft_training_inv_gaussian[j]=(dinvgauss(random_inv_gaussian[j],mean =1952.7,shape=909.91 ))*exp(-0.5*(((y_theft_training_inv_gaussian[j]-b_theft_training_inv_gaussian[j])^2/a_theft_training_inv_gaussian)-(y_theft_training_inv_gaussian[j])^2))*(a_theft_training_inv_gaussian)^(-0.5)
    
    # distribusi proposal 
  pl_training_inv_gaussian_theft[j]=0.96*dinvgauss(random_inv_gaussian[j],mean = 2200,shape=845)  
      u=runif(1,0)
    if(u<q_theft_training_inv_gaussian[j]/pl_training_inv_gaussian_theft[j]){
      y_simull_training_inv_gaussian_theft = c(y_simull_training_inv_gaussian_theft,random_inv_gaussian)
      mean_training_inv_gaussian_theft[j]=mean(y_simull_training_inv_gaussian_theft)
      sampel_yang_diterima = sampel_yang_diterima + 1
    }
    total_sampel = total_sampel + 1
  }
  acceptance_rates_theft[j] <- sampel_yang_diterima / total_sampel
  
}

mean_training_inv_gaussian_theft
sum(acceptance_rates_theft==1)/nrow(data_training_Theft_subset)

plot(density(pl_training_inv_gaussian_theft),col="blue",main="Plot Densitas Distribusi Target VS Proposal \n Inverse Gaussian-Truncated Gaussian Theft ")
lines(density(q_theft_training_inv_gaussian),col="red")

# plot density dari training 
plot(density(mean_training_inv_gaussian_theft))

# uji normalitas 
shapiro.test(mean_training_inv_gaussian_theft)# H0 diterima

# model testing antara sev_theft dan trun norm 
set.seed(105)
A5_A6_trun_norm_testing=rtruncnorm(nrow(data_testing_Theft_subset),a=a_A5_A6,b=b_A5_A6,mean = (a_A5_A6+b_A5_A6)/2,sd=(b_A5_A6-a_A5_A6)/6)
set.seed(106)
A2_A4_trun_norm_testing=rtruncnorm(nrow(data_testing_Theft_subset),a=a_A2_A4,b=b_A2_A4,mean= (a_A2_A4+b_A2_A4)/2,sd=(b_A2_A4-a_A2_A4)/6)
set.seed(107)
A7_A12_trun_norm_testing=rtruncnorm(nrow(data_testing_Theft_subset),a=a_A7_A12,b=b_A7_A12,mean = (a_A7_A12+b_A7_A12)/2,sd=(b_A7_A12-a_A7_A12)/6)

set.seed(105)
A5_A6_beta_testing=rbeta(nrow(data_testing_Theft_subset),shape1 = alpha_A5_A6,shape2 = beta_A5_A6)
set.seed(106)
A2_A4_beta_testing=rbeta(nrow(data_testing_Theft_subset),shape1 = alpha_A2_A4,shape2 = beta_A2_A4)
set.seed(107)
A7_A12_beta_testing=rbeta(nrow(data_testing_Theft_subset),shape1 = alpha_A7_A12,shape2 = beta_A7_A12)

data_testing_Theft_subset=cbind(data_testing_Theft_subset,A5_A6_trun_norm_testing,A2_A4_trun_norm_testing,A7_A12_trun_norm_testing
                                 ,A5_A6_beta_testing,A2_A4_beta_testing,A7_A12_beta_testing)

v1_A5_A6_testing=NULL
v2_A2_A4_testing=NULL
v_mat_theft_testing=matrix(NA,ncol=2,nrow = nrow(data_testing_Theft_subset))
b_theft_testing_inv_gaussian=NULL
q_theft_testing_inv_gaussian=NULL
pl_testing_inv_gaussian_theft=NULL
y_simull_testing_inv_gaussian_theft=NULL
y_theft_testing_inv_gaussian=NULL
mean_testing_inv_gaussian_theft=NULL
set.seed(100)
for(j in 1:nrow(data_testing_Theft_subset)){
  y_simull_testing_inv_gaussian_theft=NULL
   for (i in 1:450){
    # ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_A5_A6_testing[j]=qnorm(ptruncnorm(data_testing_Theft_subset$A5_A6_trun_norm_testing[j],a=a_A5_A6,b=b_A5_A6,mean = (a_A5_A6+b_A5_A6)/2,sd=(b_A5_A6-a_A5_A6)/6),0,1,lower.tail = TRUE)
    v2_A2_A4_testing[j]=qnorm(ptruncnorm(data_testing_Theft_subset$A2_A4_trun_norm_testing[j],a=a_A2_A4,b=b_A2_A4,mean = (a_A2_A4+b_A2_A4)/2,sd=(b_A2_A4-a_A2_A4)/6),0,1,lower.tail = TRUE)
  
    # matrix baris dari v*  
    v_mat_theft_testing[j,1]<- v1_A5_A6_testing[j]
    v_mat_theft_testing[j,2]<- v1_A5_A6_testing[j]
    vmat_theft_testing=t(v_mat_theft_testing)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_damage 
    a_theft_testing_inv_gaussian= 1-t(r_theft_training_inv_gaussian)%*%inv(R2_theft_training_inv_gaussian)%*%r_theft_training_inv_gaussian
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
    b_theft_testing_inv_gaussian[j]= t(r_theft_training_inv_gaussian)%*%inv(R2_theft_training_inv_gaussian)%*%(vmat_theft_testing[,j])
    
    random_inv_gaussian=rinvgauss(nrow(data_testing_Theft_subset),mean =1952.7,shape=909.91)
    
    y_theft_testing_inv_gaussian[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean = 1952.7,shape=909.91),0,1)    
    
    # q disini merupakan pdf dari y|x1,x
    q_theft_testing_inv_gaussian[j]=(dinvgauss(random_inv_gaussian[j],mean = 1952.7,shape=909.91))*exp(-0.5*(((y_theft_testing_inv_gaussian[j]-b_theft_testing_inv_gaussian[j])^2/a_theft_testing_inv_gaussian)-(y_theft_testing_inv_gaussian[j])^2))*(a_theft_testing_inv_gaussian)^(-0.5)
    
    # distribusi proposal 
    pl_testing_inv_gaussian_theft[j]=0.96*dinvgauss(random_inv_gaussian[j],mean = 2200,shape=845) 
    
    u=runif(1,0)
    if(u<q_theft_testing_inv_gaussian[j]/pl_testing_inv_gaussian_theft[j]){
      y_simull_testing_inv_gaussian_theft= c(y_simull_testing_inv_gaussian_theft,random_inv_gaussian)
      mean_testing_inv_gaussian_theft[j]=mean(y_simull_testing_inv_gaussian_theft)
    }
  }
}  
mean_testing_inv_gaussian_theft
plot(density( mean_testing_inv_gaussian_theft),main="Plot Densitas Prediksi \nCopula Inverse Gaussian-Truncated Gaussian Theft")
# uji normalitas 
shapiro.test(mean_testing_inv_gaussian_theft)

MSE_copula_theft_testing_inv_gaussian=sum((data_testing_Theft_subset$Sev_Theft- mean_testing_inv_gaussian_theft)^2)/nrow(data_testing_Theft_subset)
MSE_copula_theft_testing_inv_gaussian
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# sev_theft dan beta
#model training 
u_training_theft_inv_gaussian_beta=pobs(data_training_Theft_subset[c(25,31,32)])
fit.copula_training_theft_beta=fitCopula(normalCopula(dim = 3,dispstr = "un"),u_training_theft_inv_gaussian_beta,method="ml")
summary(fit.copula_training_theft_beta) 

R_theft_training_inv_gaussian_beta=matrix(1,nrow=3,ncol=3)
R_theft_training_inv_gaussian_beta[1,2]=fit.copula_training_theft_beta@estimate[[1]]
R_theft_training_inv_gaussian_beta[1,3]=fit.copula_training_theft_beta@estimate[[2]]
R_theft_training_inv_gaussian_beta[2,3]=fit.copula_training_theft_beta@estimate[[3]]

segitiga_bawah <- lower.tri(R_theft_training_inv_gaussian_beta,diag = TRUE)
R_theft_training_inv_gaussian_beta[segitiga_bawah]=0
t_R_theft_beta=t(R_theft_training_inv_gaussian_beta)
R_theft_training_inv_gaussian_beta=R_theft_training_inv_gaussian_beta+t_R_theft_beta
diag(R_theft_training_inv_gaussian_beta)=1

R2_theft_training_inv_gaussian_beta=R_theft_training_inv_gaussian_beta[1:2,1:2]
r_theft_training_inv_gaussian_beta=R_theft_training_inv_gaussian_beta[1:2,3]

v1_A5_A6_beta_training=NULL
v2_A2_A4_beta_training=NULL
v_mat_theft_training_beta=matrix(NA,ncol=2,nrow = nrow(data_training_Theft_subset))
b_theft_training_inv_gaussian_beta=NULL
q_theft_training_inv_gaussian_beta=NULL
pl_training_inv_gaussian_theft_beta=NULL
y_simull_training_inv_gaussian_theft_beta=NULL
y_theft_training_inv_gaussian_beta=NULL
mean_training_inv_gaussian_theft_beta=NULL
acceptance_rates_theft_beta=NULL
set.seed(100)
for(j in 1:nrow(data_training_Theft_subset)){
  y_simull_training_inv_gaussian_theft_beta=NULL
  sampel_yang_diterima = 0 
  total_sampel = 0 
  for (i in 1:100){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_A5_A6_beta_training[j]=qnorm(pbeta(data_training_Theft_subset$A5_A6_beta_training[j],shape1= alpha_A5_A6,shape2 = beta_A5_A6),0,1,lower.tail = TRUE)
    v2_A2_A4_beta_training[j]=qnorm(pbeta(data_training_Theft_subset$A2_A4_beta_training[j],shape1 = alpha_A2_A4,shape2 = beta_A2_A4),0,1,lower.tail = TRUE)
    
    # matrix baris dari v*  
    v_mat_theft_training_beta[j,1]<-v1_A5_A6_beta_training[j]
    v_mat_theft_training_beta[j,2]<-v2_A2_A4_beta_training[j]
    vmat_theft_training_beta=t(v_mat_theft_training_beta)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_theft_training_inv_gaussian  
    a_theft_training_beta= 1-t(r_theft_training_inv_gaussian_beta)%*%inv(R2_theft_training_inv_gaussian_beta)%*%r_theft_training_inv_gaussian_beta
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b_theft_training_inv_gaussian
    b_theft_training_inv_gaussian_beta[j]= t(r_theft_training_inv_gaussian_beta)%*%inv(R2_theft_training_inv_gaussian_beta)%*%(vmat_theft_training_beta[,j])
    
    # pdf(y) yang digunakan disini adalah inverse gaussian 
    random_inv_gaussian=rinvgauss(nrow(data_training_Theft_subset),mean =1952.7,shape=909.91)
    
    y_theft_training_inv_gaussian[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean =1952.7,shape=909.91),0,1)
    
    # q disini merupakan pdf dari y|x1,x
    q_theft_training_inv_gaussian_beta[j]=(dinvgauss(random_inv_gaussian[j],mean =1952.7,shape=909.91 ))*exp(-0.5*(((y_theft_training_inv_gaussian[j]-b_theft_training_inv_gaussian_beta[j])^2/a_theft_training_beta)-(y_theft_training_inv_gaussian[j])^2))*(a_theft_training_beta)^(-0.5)
    
    # distribusi proposal 
    pl_training_inv_gaussian_theft_beta[j]=1*dinvgauss(random_inv_gaussian[j],mean = 2900,shape=770)  
    
    u=runif(1,0)
    if(u<q_theft_training_inv_gaussian_beta[j]/pl_training_inv_gaussian_theft_beta[j]){
      y_simull_training_inv_gaussian_theft_beta = c(y_simull_training_inv_gaussian_theft_beta,random_inv_gaussian)
      mean_training_inv_gaussian_theft_beta[j]=mean(y_simull_training_inv_gaussian_theft_beta)
      sampel_yang_diterima = sampel_yang_diterima + 1
    }
    total_sampel = total_sampel + 1
  }
  acceptance_rates_theft_beta[j] <- sampel_yang_diterima / total_sampel
  
}
sum(acceptance_rates_theft_beta==1)/nrow(data_training_Theft_subset)
plot(density(pl_training_inv_gaussian_theft_beta),col="blue", main="Plot Densitas Distribusi Target VS Proposal \n Inverse Gaussian-Beta Theft")
lines(density(q_theft_training_inv_gaussian_beta),col="red")

mean_training_inv_gaussian_theft_beta
plot(density(mean_training_inv_gaussian_theft_beta))

#cek normalitas
shapiro.test(mean_training_inv_gaussian_theft_beta)

# model testing inv gaussian dan beta 

v1_A5_A6_testing_beta=NULL
v2_A2_A4_testing_beta=NULL
v_mat_theft_testing_beta=matrix(NA,ncol=2,nrow = nrow(data_testing_Theft_subset))
b_theft_testing_inv_gaussian_beta=NULL
q_theft_testing_inv_gaussian_beta=NULL
pl_testing_inv_gaussian_theft_beta=NULL
y_simull_testing_inv_gaussian_theft_beta=NULL
y_theft_testing_inv_gaussian_beta=NULL
mean_testing_inv_gaussian_theft_beta=NULL
set.seed(100)
for(j in 1:nrow(data_testing_Theft_subset)){
  y_simull_testing_inv_gaussian_theft_beta=NULL
   for (i in 1:450){
    # ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_A5_A6_testing_beta[j]=qnorm(pbeta(data_testing_Theft_subset$A5_A6_beta_testing[j],shape1= alpha_A5_A6,shape2 = beta_A5_A6),0,1,lower.tail = TRUE)
    v2_A2_A4_testing_beta[j]=qnorm(pbeta(data_testing_Theft_subset$A2_A4_beta_testing[j],shape1 = alpha_A2_A4,shape2 = beta_A2_A4),0,1,lower.tail = TRUE)
  
    # matrix baris dari v*  
    v_mat_theft_testing_beta[j,1]<- v1_A5_A6_testing[j]
    v_mat_theft_testing_beta[j,2]<- v1_A5_A6_testing[j]
    vmat_theft_testing_beta=t(v_mat_theft_testing_beta)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a
    a_theft_testing_beta= 1-t(r_theft_training_inv_gaussian_beta)%*%inv(R2_theft_training_inv_gaussian_beta)%*%r_theft_training_inv_gaussian_beta
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
    b_theft_testing_inv_gaussian_beta[j]= t(r_theft_training_inv_gaussian_beta)%*%inv(R2_theft_training_inv_gaussian_beta)%*%(vmat_theft_testing_beta[,j])
    
    random_inv_gaussian=rinvgauss(nrow(data_testing_Theft_subset),mean =1952.7,shape=909.91)
    
    y_theft_testing_inv_gaussian[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean = 1952.7,shape=909.91),0,1)    
    
    # q disini merupakan pdf dari y|x1,x
    q_theft_testing_inv_gaussian_beta[j]=(dinvgauss(random_inv_gaussian[j],mean =1952.7,shape=909.91 ))*exp(-0.5*(((y_theft_testing_inv_gaussian[j]-b_theft_testing_inv_gaussian_beta[j])^2/a_theft_testing_beta)-(y_theft_testing_inv_gaussian[j])^2))*(a_theft_testing_beta)^(-0.5)
    
    # distribusi proposal 
    pl_testing_inv_gaussian_theft_beta[j]=1*dinvgauss(random_inv_gaussian[j],mean = 2900,shape=770)    
    
    u=runif(1,0)
    if(u< q_theft_testing_inv_gaussian_beta[j]/pl_testing_inv_gaussian_theft_beta[j]){
      y_simull_testing_inv_gaussian_theft_beta= c(y_simull_testing_inv_gaussian_theft_beta,random_inv_gaussian)
      mean_testing_inv_gaussian_theft_beta[j]=mean( y_simull_testing_inv_gaussian_theft_beta)
    }
  }
}  
mean_testing_inv_gaussian_theft_beta
plot(density( mean_testing_inv_gaussian_theft_beta),main="Plot Densitas Prediksi \n Copula Inverse Gaussian-Beta Theft")
# uji normalitas 
shapiro.test(mean_testing_inv_gaussian_theft_beta)

MSE_copula_theft_testing_inv_gaussian_beta=sum((data_testing_Theft_subset$Sev_Theft- mean_testing_inv_gaussian_theft_beta)^2)/nrow(data_testing_Theft_subset)
MSE_copula_theft_testing_inv_gaussian_beta

#-----------------------------------------------------------------------------------------------------------------------------------------------
#Frekuensi 
mod1_freq_pois_Theft=glm(Theft~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = poisson("log"),data = data_training)
summary(mod1_freq_pois_Theft)
mod2_freq_pois_Theft=stepAIC(mod1_freq_pois_Theft,direction = "both")
summary(mod2_freq_pois_Theft)

#predict 
y_hat_freq_Poisson_Theft=predict(mod2_freq_pois_Theft,data_testing_Theft_subset,type = "response")

# pure premium Theft
pure_prem_GammaPoisson_Theft=y_hat_Theft_Gamma*y_hat_freq_Poisson_Theft
pure_prem_GaussianPoisson_Theft=y_hat_Theft_Gaussian*y_hat_freq_Poisson_Theft
pure_prem_GaussianPoisson_Theft_log=y_hat_Theft_Gaussian_log*y_hat_freq_Poisson_Theft
pure_prem_InverseGaussianPoisson_Theft=y_hat_Theft_Inverse_Gaussian*y_hat_freq_Poisson_Theft
pure_prem_InverseGaussianPoisson_Theft_copula=mean_testing_inv_gaussian_theft*y_hat_freq_Poisson_Theft
pure_prem_InverseGaussianBeta_Theft_copula=mean_testing_inv_gaussian_theft_beta*y_hat_freq_Poisson_Theft


#MSE
S_Theft=data_testing_Theft_subset$Sev_Theft*data_testing_Theft_subset$Theft
MSE_Theft_GammaPoisson=sum((S_Theft-pure_prem_GammaPoisson_Theft)^2)/nrow(data_testing_Theft_subset)
MSE_Theft_GammaPoisson

MSE_Theft_GaussianPoisson=sum((S_Theft-pure_prem_GaussianPoisson_Theft)^2)/nrow(data_testing_Theft_subset)
MSE_Theft_GaussianPoisson

MSE_Theft_GaussianPoisson_log=sum((S_Theft-pure_prem_GaussianPoisson_Theft_log)^2)/nrow(data_testing_Theft_subset)
MSE_Theft_GaussianPoisson_log

MSE_Theft_InverseGaussianPoisson=sum((S_Theft-pure_prem_InverseGaussianPoisson_Theft)^2)/nrow(data_testing_Theft_subset)
MSE_Theft_InverseGaussianPoisson

MSE_Theft_InverseGaussianPoisson_copula=sum((S_Theft-pure_prem_InverseGaussianPoisson_Theft_copula)^2)/nrow(data_testing_Theft_subset)
MSE_Theft_InverseGaussianPoisson_copula

MSE_Theft_InverseGaussianBeta_copula=sum((S_Theft-pure_prem_InverseGaussianBeta_Theft_copula)^2)/nrow(data_testing_Theft_subset)
MSE_Theft_InverseGaussianBeta_copula

#Boxplot

Sev_Theft_testing=data.frame(cbind(data_testing_Theft_subset$Sev_Theft,y_hat_Theft_Gamma,y_hat_Theft_Gaussian,y_hat_Theft_Gaussian_log
                                        ,y_hat_Theft_Inverse_Gaussian,mean_testing_inv_gaussian_theft,mean_testing_inv_gaussian_theft_beta))
colnames(Sev_Theft_testing)<-c("sev_asli","yhat_Gamma","yhat_Gaussian","yhat_Gaussian_log","yhat_invGauss", 
                                "yhat_invGauss-trunNorm", "yhat_invGauss-Beta")

df_theft <- tidyr::gather(Sev_Theft_testing, key = "Model", value = "Predicted_Values")
mean_values_theft <- aggregate(Predicted_Values ~ Model, data = df_theft, FUN = mean)

ggplot(df_theft, aes(x = Model, y = Predicted_Values)) +
  geom_boxplot()+
  geom_point(data = mean_values_theft, aes(x = Model, y = Predicted_Values), col = "gray", pch = 19)+
  geom_text(data = mean_values_theft, aes(x = Model, y = Predicted_Values, label = round(Predicted_Values, 2)),
            vjust = -0.5, hjust = 0.5, size = 3, col = "black")+
  labs(title = "Boxplot Model Besar Klaim Theft", 
       x = "Models", y = "Predicted Values") +
  scale_y_continuous(limits = c(0,8000))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Boxplot Premi Murni Theft

premi_murni_theft=data.frame(cbind(S_Theft,pure_prem_GammaPoisson_Theft,pure_prem_GaussianPoisson_Theft, pure_prem_GaussianPoisson_Theft_log,pure_prem_InverseGaussianPoisson_Theft
                                   ,pure_prem_InverseGaussianPoisson_Theft_copula,pure_prem_InverseGaussianBeta_Theft_copula))
colnames(premi_murni_theft) <- c("Data", "Pois &\n Gamma", "Pois & \nNormal", "Pois & \nNormal Log","Pois & Inverse \nGaussian"
                                 ,"Pois & inverse\n Gaussian-Truncated \nGaussian","Pois & inverse\n Gaussian-Beta")
df_premi_murni_theft <- tidyr::gather(premi_murni_theft, key = "Model", value = "Predicted_Values")
df_premi_murni_theft$Model <- factor(df_premi_murni_theft$Model, levels = colnames(premi_murni_theft))
mean_premi_murni_theft <- aggregate(Predicted_Values ~ Model, data = df_premi_murni_theft, FUN = mean)

ggplot(df_premi_murni_theft, aes(x = Model, y = Predicted_Values)) +
  geom_boxplot()+
  geom_point(data = mean_premi_murni_theft, aes(x = Model, y = Predicted_Values), col = "gray", pch = 19)+
  geom_text(data = mean_premi_murni_theft, aes(x = Model, y = Predicted_Values, label = round(Predicted_Values, 4)),
            vjust = -0.5, hjust = 0.5, size = 3, col = "black")+
  labs(title = "Boxplot Premi Murni Theft", 
       x = "Models", y = "Predicted Values") +
  scale_y_continuous(limits = c(0,2000),expand = c(0.1, 0))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#################################################################################################################################################################################
# Sev_Total 
data_training_Total_subset=subset(data_training,Sev_Total>0)
data_testing_Total_subset=(data_testing)
summary(data_training_Total_subset)

# mengubah data katgorikal menjadi kontinu 
data_gender_total=mapvalues(data_training_Total_subset$DrivGender, from = c("F","M")
                          , to = c("1", "2"))
data_gender_total=data.frame(as.factor(data_gender_total))

frequency_table_gender_total <- table(data_gender_total)

PR_gender_total=NULL
for( i in 1:2){ 
  PR_gender_total[i]=frequency_table_gender_total[i]/(frequency_table_gender_total[1]+frequency_table_gender_total[2])
}
PR_gender_total

gender <- c("F","M")
Cumprob_gender_total=data.frame(cbind(gender,PR_gender_total))
Prob_gender_total=Cumprob_gender_total[order(Cumprob_gender_total$PR_gender_total,decreasing = TRUE),]
Prob_gender_total$PR_gender_total=as.numeric(Prob_gender_total$PR_gender_total)

# Truncated Gaussian 

a_M=0
b_M=Prob_gender_total$PR_gender_total[1]

a_F=Prob_gender_total$PR_gender_total[1]
b_F=Prob_gender_total$PR_gender_total[1]+Prob_gender_total$PR_gender_total[2]

set.seed(105)
M_trun_norm_training_total=rtruncnorm(nrow(data_training_Total_subset),a=a_M,b_M,mean = (a_M+b_M)/2,sd=(b_M-a_M)/6)
set.seed(106)
F_trun_norm_training_total=rtruncnorm(nrow(data_training_Total_subset),a=a_F,b=b_F,mean= (a_F+b_F)/2,sd=(b_F-a_F)/6)

# beta 
alpha_M=Prob_gender_total$PR_gender_total[1]*(((Prob_gender_total$PR_gender_total[1]*(1-Prob_gender_total$PR_gender_total[1]))/var(Prob_gender_total$PR_gender_total))-1)
beta_M=(1-Prob_gender_total$PR_gender_total[1])*(((Prob_gender_total$PR_gender_total[1]*(1-Prob_gender_total$PR_gender_total[1]))/var(Prob_gender_total$PR_gender_total))-1)

alpha_F=Prob_gender_total$PR_gender_total[2]*(((Prob_gender_total$PR_gender_total[2]*(1-Prob_gender_total$PR_gender_total[2]))/var(Prob_gender_total$PR_gender_total))-1)
beta_F=(1-Prob_gender_total$PR_gender_total[2])*(((Prob_gender_total$PR_gender_total[2]*(1-Prob_gender_total$PR_gender_total[2]))/var(Prob_gender_total$PR_gender_total))-1)

set.seed(105)
M_beta_training_total=rbeta(nrow(data_training_Total_subset),shape1 = alpha_M,shape2 = beta_M)
set.seed(106)
F_beta_training_total=rbeta(nrow(data_training_Total_subset),shape1 = alpha_F,shape2 = beta_F)

data_training_Total_subset=cbind(data_training_Total_subset,M_trun_norm_training_total,F_trun_norm_training_total,M_beta_training_total
                                 ,F_beta_training_total)


#family=Gamma(link="log")
set.seed(100)
mod1_Total=glm(Sev_Total~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = Gamma(link="log"),data = data_training_Total_subset)
summary(mod1_Total) 
step_mod1_Total=stepAIC(mod1_Total,direction = "both")
summary(step_mod1_Total)

#family=Gaussian(link="identity")
mod2_Total=glm(Sev_Total~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = gaussian(link="identity"),data = data_training_Total_subset)
summary(mod2_Total) 
step_mod2_Total=stepAIC(mod2_Total,direction = "both")
summary(step_mod2_Total)


#family=Gaussian(link="log")
mod2_Total_log=glm(Sev_Total~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Area+Channel,family = gaussian(link="log"),data = data_training_Total_subset)
summary(mod2_Total_log) #garage,region
step_mod2_Total_log=stepAIC(mod2_Total_log,direction = "both")
summary(step_mod2_Total_log)


#family=inverse.gaussian(link="log")
mod3_Total=glm(Sev_Total~VehAge+DrivAge+DrivGender+BonusMalus+PayFreq+VehPower+VehGas,family = inverse.gaussian(link="log"),data = data_training_Total_subset)
summary(mod3_Total) #+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel
step_mod3_Total=stepAIC(mod3_Total,direction = "both")
summary(step_mod3_Total)

#predict
y_hat_Total_Gamma=predict(step_mod1_Total,data_testing_Total_subset,type="response") 
MSE_Total_Gamma=sum((data_testing_Total_subset$Sev_Total-y_hat_Total_Gamma)^2)/nrow(data_testing_Total_subset)
MSE_Total_Gamma

y_hat_Total_Gaussian=predict(step_mod2_Total,data_testing_Total_subset,type="response") 
MSE_Total_Gaussian=sum((data_testing_Total_subset$Sev_Total-y_hat_Total_Gaussian)^2)/nrow(data_testing_Total_subset)
MSE_Total_Gaussian

y_hat_Total_Gaussian_Log=predict(step_mod2_Total_log,data_testing_Total_subset,type="response") 
MSE_Total_Gaussian_Log=sum((data_testing_Total_subset$Sev_Total-y_hat_Total_Gaussian_Log)^2)/nrow(data_testing_Total_subset)
MSE_Total_Gaussian_Log

y_hat_Total_inverse_Gaussian=predict(step_mod3_Total,data_testing_Total_subset,type="response") 
MSE_Total_inverse_Gaussian=sum((data_testing_Total_subset$Sev_Total-y_hat_Total_inverse_Gaussian)^2)/nrow(data_testing_Total_subset)
MSE_Total_inverse_Gaussian

# copula yang akan di bentuk hanya menggunakan Driv Gender M
u_training_total_inv_gaussian=pobs(data_training_Total_subset[c(27,28)])
fit.copula_training_total=fitCopula(normalCopula(dim = 2,dispstr = "un"),u_training_total_inv_gaussian,method="ml")
summary(fit.copula_training_total) 

R_total_training_inv_gaussian=matrix(1,nrow=2,ncol=2)
R_total_training_inv_gaussian[1,2]=fit.copula_training_total@estimate[[1]]
R_total_training_inv_gaussian[2,1]=fit.copula_training_total@estimate[[1]]

R2_total_training_inv_gaussian=R_total_training_inv_gaussian[1:1,1:1]
r_total_training_inv_gaussian=R_total_training_inv_gaussian[1:1,2]

v1_M_training=NULL
v_mat_total_training=matrix(NA,ncol=1,nrow = nrow(data_training_Total_subset))
b_total_training_inv_gaussian=NULL
q_total_training_inv_gaussian=NULL
pl_training_inv_gaussian_total=NULL
y_simull_training_inv_gaussian_total=NULL
y_total_training_inv_gaussian=NULL
mean_training_inv_gaussian_total=NULL
acceptance_rates_total=NULL
set.seed(100)
for(j in 1:nrow(data_training_Total_subset)){
  y_simull_training_inv_gaussian_total=NULL
  sampel_yang_diterima = 0 
  total_sampel = 0 
  for (i in 1:100){
    #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_M_training[j]=qnorm(ptruncnorm(data_training_Total_subset$M_trun_norm_training_total[j]
                                      ,a=a_M,b=b_M, mean = (a_M+b_M)/2, sd=(b_M-a_M)/6),0,1,lower.tail = TRUE)

    # matrix baris dari v*  
    v_mat_total_training[j,1]<-v1_M_training[j]
    vmat_total_training=t(v_mat_total_training)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a_theft_training_inv_gaussian  
    a_total_training= 1-t(r_total_training_inv_gaussian)%*%(R2_total_training_inv_gaussian)%*%r_total_training_inv_gaussian
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b_theft_training_inv_gaussian
    b_total_training_inv_gaussian[j]= t(r_total_training_inv_gaussian)%*%(R2_total_training_inv_gaussian)%*%(vmat_total_training[,j])
    
    # pdf(y) yang digunakan disini adalah inverse gaussian 
    random_inv_gaussian=rinvgauss(nrow(data_training_Total_subset),mean =1182.2,shape=52.449)
    
    y_total_training_inv_gaussian[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean =1182.2,shape=52.449),0,1)
    
    # q disini merupakan pdf dari y|x1,x
    q_total_training_inv_gaussian[j]=(dinvgauss(random_inv_gaussian[j],mean =1182.2,shape=52.449))*exp(-0.5*(((y_total_training_inv_gaussian[j]-b_total_training_inv_gaussian[j])^2/a_total_training)-(y_total_training_inv_gaussian[j])^2))*(a_total_training)^(-0.5)
    
    # distribusi proposal 
    pl_training_inv_gaussian_total[j]=1.0000001*dinvgauss(random_inv_gaussian[j],mean = 20000,shape=49)  
    
    u=runif(1,0)
    if(u<q_total_training_inv_gaussian[j]/pl_training_inv_gaussian_total[j]){
      y_simull_training_inv_gaussian_total = c(y_simull_training_inv_gaussian_total,random_inv_gaussian)
      mean_training_inv_gaussian_total[j]=mean(y_simull_training_inv_gaussian_total)
      sampel_yang_diterima = sampel_yang_diterima + 1
    }
    total_sampel = total_sampel + 1
  }
  acceptance_rates_total[j] <- sampel_yang_diterima / total_sampel
  
}
plot(density(q_total_training_inv_gaussian),col="red",main="Plot Densitas Distribusi Target VS Proposal \n Inverse Gaussian-Truncated Gaussian Total")
lines(density(pl_training_inv_gaussian_total),col="blue")

sum(acceptance_rates_total==1)/nrow(data_training_Total_subset)

mean_training_inv_gaussian_total
plot(density(mean_training_inv_gaussian_total))

#cek normalitas
ks.test(mean_training_inv_gaussian_total,"pnorm",mean(mean_training_inv_gaussian_total),sd(mean_training_inv_gaussian_total))

#Truncated Normal
set.seed(105)
M_trun_norm_testing_total=rtruncnorm(nrow(data_testing_Total_subset),a=a_M,b_M,mean = (a_M+b_M)/2,sd=(b_M-a_M)/6)
set.seed(106)
F_trun_norm_testing_total=rtruncnorm(nrow(data_testing_Total_subset),a=a_F,b=b_F,mean= (a_F+b_F)/2,sd=(b_F-a_F)/6)
#Beta
set.seed(105)
M_beta_testing_total=rbeta(nrow(data_testing_Total_subset),shape1 = alpha_M,shape2 = beta_M)
set.seed(106)
F_beta_testing_total=rbeta(nrow(data_testing_Total_subset),shape1 = alpha_F,shape2 = beta_F)


data_testing_Total_subset=cbind(data_testing_Theft_subset,M_trun_norm_testing_total,F_trun_norm_testing_total
                                ,M_beta_testing_total,F_beta_testing_total)
                                

# Model testing 
v1_M_testing=NULL
v_mat_total_testing=matrix(NA,ncol=1,nrow = nrow(data_testing_Total_subset))
b_total_testing_inv_gaussian=NULL
q_total_testing_inv_gaussian=NULL
pl_testing_inv_gaussian_total=NULL
y_simull_testing_inv_gaussian_total=NULL
y_total_testing_inv_gaussian=NULL
mean_testing_inv_gaussian_total=NULL
set.seed(100)
for(j in 1:nrow(data_testing_Total_subset)){
  y_simull_testing_inv_gaussian_total=NULL
  for (i in 1:450){
    # ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
    v1_M_testing[j]=qnorm(ptruncnorm(data_testing_Total_subset$M_trun_norm_testing_total[j],a=a_M,b=b_M, mean = (a_M+b_M)/2, sd=(b_M-a_M)/6),0,1,lower.tail = TRUE)
  
    # matrix baris dari v*  
    v_mat_total_testing[j,1]<- v1_M_testing[j]
    vmat_total_testing=t(v_mat_total_testing)
    
    # misalkan 1-r^t*R(n-1)^-1 r  adalah a
    a_total_testing= 1-t(r_total_training_inv_gaussian)%*%(R2_total_training_inv_gaussian)%*%r_total_training_inv_gaussian
    
    #misalkan t(r)*inverse(R_(n-1))* v* adalah b
    b_total_testing_inv_gaussian[j]= t(r_total_training_inv_gaussian)%*%(R2_total_training_inv_gaussian)%*%(vmat_total_testing[,j])
    
    random_inv_gaussian=rinvgauss(nrow(data_testing_Total_subset),mean =1182.2,shape=52.449)
    
    y_total_testing_inv_gaussian[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean =1182.2,shape=52.449),0,1)    
    
    # q disini merupakan pdf dari y|x1,x
    q_total_testing_inv_gaussian[j]=(dinvgauss(random_inv_gaussian[j],mean =1182.2,shape=52.449 ))*exp(-0.5*(((y_total_testing_inv_gaussian[j]-b_total_testing_inv_gaussian[j])^2/a_total_testing)-(y_total_testing_inv_gaussian[j])^2))*(a_total_testing)^(-0.5)
    
    # distribusi proposal 
    pl_testing_inv_gaussian_total[j]=1.0000001*dinvgauss(random_inv_gaussian[j],mean = 20000,shape=49)  
    
    u=runif(1,0)
    if(u< q_total_testing_inv_gaussian[j]/pl_testing_inv_gaussian_total[j]){
      y_simull_testing_inv_gaussian_total= c(y_simull_testing_inv_gaussian_total,random_inv_gaussian)
      mean_testing_inv_gaussian_total[j]=mean(y_simull_testing_inv_gaussian_total)
    }
  }
}  
mean_testing_inv_gaussian_total
plot(density( mean_testing_inv_gaussian_total),main="Plot Densitas Prediksi \n Copula Inverse Gaussian-Truncated Gaussian Total")
# uji normalitas 
shapiro.test(mean_testing_inv_gaussian_total)

MSE_copula_total_testing_inv_gaussian=sum((data_testing_Total_subset$Sev_Total- mean_testing_inv_gaussian_total)^2)/nrow(data_testing_Total_subset)
MSE_copula_total_testing_inv_gaussian

# Beta 
u_training_total_inv_gaussian_beta=pobs(data_training_Total_subset[c(27,30)])
fit.copula_training_total_beta=fitCopula(normalCopula(dim = 2,dispstr = "un"),u_training_total_inv_gaussian_beta,method="ml")
summary(fit.copula_training_total_beta) 

R_total_training_inv_gaussian_beta=matrix(1,nrow=2,ncol=2)
R_total_training_inv_gaussian_beta[1,2]=fit.copula_training_total_beta@estimate[[1]]
R_total_training_inv_gaussian_beta[2,1]=fit.copula_training_total_beta@estimate[[1]]

R2_total_training_inv_gaussian_beta=R_total_training_inv_gaussian_beta[1:1,1:1]
r_total_training_inv_gaussian_beta=R_total_training_inv_gaussian_beta[1:1,2]

v1_M_training_beta=NULL
v_mat_total_training_beta=matrix(NA,ncol=1,nrow = nrow(data_training_Total_subset))
b_total_training_inv_gaussian_beta=NULL
q_total_training_inv_gaussian_beta=NULL
pl_training_inv_gaussian_total_beta=NULL
y_simull_training_inv_gaussian_total_beta=NULL
y_total_training_inv_gaussian_beta=NULL
mean_training_inv_gaussian_total_beta=NULL
acceptance_rates_total_beta=NULL
set.seed(100)
for(j in 1:nrow(data_training_Total_subset)){
    y_simull_training_inv_gaussian_total_beta=NULL
    sampel_yang_diterima = 0 
    total_sampel = 0 
    for (i in 1:100){
  #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
  v1_M_training_beta[j]=qnorm(pbeta(data_training_Total_subset$M_beta_training_total[j]
                                    ,shape1 =alpha_M,shape2 =beta_M ),0,1,lower.tail = TRUE)
  
  # matrix baris dari v*  
  v_mat_total_training_beta[j,1]<-v1_M_training_beta[j]
  vmat_total_training_beta=t(v_mat_total_training_beta)
  
  # misalkan 1-r^t*R(n-1)^-1 r  adalah a_theft_training_inv_gaussian  
  a_total_training_beta= 1-t(r_total_training_inv_gaussian_beta)%*%(R2_total_training_inv_gaussian_beta)%*%r_total_training_inv_gaussian_beta
  
  #misalkan t(r)*inverse(R_(n-1))* v* adalah b_theft_training_inv_gaussian
  b_total_training_inv_gaussian_beta[j]= t(r_total_training_inv_gaussian_beta)%*%(R2_total_training_inv_gaussian_beta)%*%(vmat_total_training_beta[,j])
  
  # pdf(y) yang digunakan disini adalah inverse gaussian 
  random_inv_gaussian=rinvgauss(nrow(data_training_Total_subset),mean =1182.2,shape=52.449)
  
  y_total_training_inv_gaussian_beta[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean =1182.2,shape=52.449),0,1)
  
  # q disini merupakan pdf dari y|x1,x
  q_total_training_inv_gaussian_beta[j]=(dinvgauss(random_inv_gaussian[j],mean =1182.2,shape=52.449))*exp(-0.5*(((y_total_training_inv_gaussian_beta[j]-b_total_training_inv_gaussian_beta[j])^2/a_total_training_beta)-(y_total_training_inv_gaussian_beta[j])^2))*(a_total_training_beta)^(-0.5)
  
  # distribusi proposal 
  pl_training_inv_gaussian_total_beta[j]=1.0000001*dinvgauss(random_inv_gaussian[j],mean = 200000,shape=49)  
  
  u=runif(1,0)
  if(u<q_total_training_inv_gaussian_beta[j]/pl_training_inv_gaussian_total_beta[j]){
       y_simull_training_inv_gaussian_total_beta= c(y_simull_training_inv_gaussian_total_beta,random_inv_gaussian)
        mean_training_inv_gaussian_total_beta[j]=mean(y_simull_training_inv_gaussian_total_beta)
        sampel_yang_diterima = sampel_yang_diterima + 1
        }
        total_sampel = total_sampel + 1
      }
      acceptance_rates_total_beta[j] <- sampel_yang_diterima / total_sampel
    
  }
  plot(density(q_total_training_inv_gaussian_beta),col="red",main="Plot Densitas Distribusi Target VS Proposal \n Inverse Gaussian-Beta Total")
  lines(density(pl_training_inv_gaussian_total_beta),col="blue")
  
  sum(acceptance_rates_total_beta==1)/nrow(data_training_Total_subset)
  
  mean_training_inv_gaussian_total_beta
  plot(density(mean_training_inv_gaussian_total_beta))
  
# testing 
  
  v1_M_testing_beta=NULL
  v_mat_total_testing_beta=matrix(NA,ncol=1,nrow = nrow(data_testing_Total_subset))
  b_total_testing_inv_gaussian_beta=NULL
  q_total_testing_inv_gaussian_beta=NULL
  pl_testing_inv_gaussian_total_beta=NULL
  y_simull_testing_inv_gaussian_total_beta=NULL
  y_total_testing_inv_gaussian_beta=NULL
  mean_testing_inv_gaussian_total_beta=NULL
  set.seed(100)
  for(j in 1:nrow(data_testing_Total_subset)){
    y_simull_testing_inv_gaussian_total_beta=NULL
    for (i in 1:450){
      #ini untuk cari vi* dimana vi* adalah CDF inverse normal dari x1 dan x2
      v1_M_testing_beta[j]=qnorm(pbeta(data_testing_Total_subset$M_beta_testing_total[j]
                                        ,shape1 =alpha_M,shape2 =beta_M ),0,1,lower.tail = TRUE)
      
      # matrix baris dari v*  
      v_mat_total_testing_beta[j,1]<-v1_M_testing_beta[j]
      vmat_total_testing_beta=t(v_mat_total_testing_beta)
      
      # misalkan 1-r^t*R(n-1)^-1 r  adalah a_theft_training_inv_gaussian  
      a_total_testing_beta= 1-t(r_total_training_inv_gaussian_beta)%*%(R2_total_training_inv_gaussian_beta)%*%r_total_training_inv_gaussian_beta
      
      #misalkan t(r)*inverse(R_(n-1))* v* adalah b_theft_training_inv_gaussian
      b_total_testing_inv_gaussian_beta[j]= t(r_total_training_inv_gaussian_beta)%*%(R2_total_training_inv_gaussian_beta)%*%(vmat_total_testing_beta[,j])
      
      # pdf(y) yang digunakan disini adalah inverse gaussian 
      random_inv_gaussian=rinvgauss(nrow(data_testing_Total_subset),mean =1182.2,shape=52.449)
      
      y_total_testing_inv_gaussian_beta[j]=qnorm(pinvgauss(random_inv_gaussian[j],mean =1182.2,shape=52.449),0,1)
      
      # q disini merupakan pdf dari y|x1,x
      q_total_testing_inv_gaussian_beta[j]=(dinvgauss(random_inv_gaussian[j],mean =1182.2,shape=52.449))*exp(-0.5*(((y_total_testing_inv_gaussian_beta[j]-b_total_testing_inv_gaussian_beta[j])^2/a_total_testing_beta)-(y_total_testing_inv_gaussian_beta[j])^2))*(a_total_testing_beta)^(-0.5)
      
      # distribusi proposal 
      pl_testing_inv_gaussian_total_beta[j]=1.0000001*dinvgauss(random_inv_gaussian[j],mean = 200000,shape=49)  
      
      u=runif(1,0)
      if(u<q_total_testing_inv_gaussian_beta[j]/pl_testing_inv_gaussian_total_beta[j]){
        y_simull_testing_inv_gaussian_total_beta= c(y_simull_testing_inv_gaussian_total_beta,random_inv_gaussian)
        mean_testing_inv_gaussian_total_beta[j]=mean(y_simull_testing_inv_gaussian_total_beta)
        sampel_yang_diterima = sampel_yang_diterima + 1
      }
    }
  }
  mean_testing_inv_gaussian_total_beta
  plot(density( mean_testing_inv_gaussian_total_beta),main="Plot Densitas Prediksi \n Copula Inverse Gaussian-Beta Total")
  # uji normalitas 
  shapiro.test(mean_testing_inv_gaussian_total_beta)
  
  MSE_copula_total_testing_inv_gaussian_beta=sum((data_testing_Total_subset$Sev_Total- mean_testing_inv_gaussian_total_beta)^2)/nrow(data_testing_Total_subset)
  MSE_copula_total_testing_inv_gaussian_beta
  
  #Frekuensi 
  mod1_freq_pois_Total=glm(Total~DrivAge+DrivGender+BonusMalus+PayFreq+VehClass+VehAge+VehPower+VehGas+Garage+Area+Region+Channel,family = poisson("log"),data = data_training)
  summary(mod1_freq_pois_Total)
  mod2_freq_pois_Total=stepAIC(mod1_freq_pois_Total,direction = "both")
  summary(mod2_freq_pois_Total)
  
  #predict 
  y_hat_freq_Poisson_Total=predict(mod2_freq_pois_Total,data_testing_Total_subset,type = "response")
  
# Premi murni 
  pure_prem_GammaPoisson_Total=y_hat_Total_Gamma*y_hat_freq_Poisson_Total
  pure_prem_GaussianPoisson_Total=y_hat_Total_Gaussian*y_hat_freq_Poisson_Total
  pure_prem_GaussianPoisson_Total_log=y_hat_Total_Gaussian_Log*y_hat_freq_Poisson_Total
  pure_prem_InverseGaussianPoisson_Total=y_hat_Total_inverse_Gaussian*y_hat_freq_Poisson_Total
  pure_prem_InverseGaussianPoisson_Total_copula=mean_testing_inv_gaussian_total*y_hat_freq_Poisson_Total
  pure_prem_InverseGaussianBeta_Total_copula=mean_testing_inv_gaussian_total_beta*y_hat_freq_Poisson_Total
  
# MSE Premi Murni 
  S_Total=data_testing_Total_subset$Total*data_testing_Total_subset$Sev_Total
  MSE_Total_GammaPoisson=sum((S_Total-pure_prem_GammaPoisson_Total)^2)/nrow(data_testing_Total_subset)
  MSE_Total_GammaPoisson
  
  MSE_Total_GaussianPoisson=sum((S_Total-pure_prem_GaussianPoisson_Total)^2)/nrow(data_testing_Total_subset)
  MSE_Total_GaussianPoisson
  
  MSE_Total_GaussianPoisson_log=sum((S_Total-pure_prem_GaussianPoisson_Total_log)^2)/nrow(data_testing_Total_subset)
  MSE_Total_GaussianPoisson_log
  
  MSE_Total_InverseGaussianPoisson=sum((S_Total-pure_prem_InverseGaussianPoisson_Total)^2)/nrow(data_testing_Total_subset)
  MSE_Total_InverseGaussianPoisson
  
  MSE_Total_InverseGaussianPoisson_copula=sum((S_Total-pure_prem_InverseGaussianPoisson_Total_copula)^2)/nrow(data_testing_Total_subset)
  MSE_Total_InverseGaussianPoisson_copula
  
  MSE_Total_InverseGaussianBeta_copula=sum((S_Total-pure_prem_InverseGaussianBeta_Total_copula)^2)/nrow(data_testing_Total_subset)
  MSE_Total_InverseGaussianBeta_copula
  
  # Boxplot Premi Murni Total
  
  premi_murni_total=data.frame(cbind(S_Total,pure_prem_GammaPoisson_Total,pure_prem_GaussianPoisson_Total, pure_prem_GaussianPoisson_Total_log,pure_prem_InverseGaussianPoisson_Total
                                     ,pure_prem_InverseGaussianPoisson_Total_copula,pure_prem_InverseGaussianBeta_Total_copula))
  colnames(premi_murni_total) <- c("Data", "Pois &\n Gamma", "Pois & \nNormal", "Pois & \nNormal Log","Pois & Inverse \nGaussian"
                                   ,"Pois & inverse\n Gaussian-Truncated \nGaussian","Pois & inverse\n Gaussian-Beta")
  df_premi_murni_total <- tidyr::gather(premi_murni_total, key = "Model", value = "Predicted_Values")
  df_premi_murni_total$Model <- factor(df_premi_murni_total$Model, levels = colnames(premi_murni_total))
  mean_premi_murni_total <- aggregate(Predicted_Values ~ Model, data = df_premi_murni_total, FUN = mean)
  
  ggplot(df_premi_murni_total, aes(x = Model, y = Predicted_Values)) +
    geom_boxplot()+
    geom_point(data = mean_premi_murni_total, aes(x = Model, y = Predicted_Values), col = "gray", pch = 19)+
    geom_text(data = mean_premi_murni_total, aes(x = Model, y = Predicted_Values, label = round(Predicted_Values, 4)),
              vjust = -0.5, hjust = 0.5, size = 3, col = "black")+
    labs(title = "Boxplot Premi Murni Total", 
         x = "Models", y = "Predicted Values") +
    scale_y_continuous(limits = c(0,2000),expand = c(0.1, 0))+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
# Perbandingan data total dan dibagi beberapa jenis jaminan
  #GLM
  pure_prem_Total_GLM_individual=pure_prem_GammaPoisson+pure_prem_GaussianPoisson_TPL+pure_prem_GammaPoisson_damage+
    pure_prem_GaussianPoisson_other_log+pure_prem_GaussianPoisson_Theft_log+pure_prem_InverseGaussianPoisson_fire
  pure_prem_Total_Copula_Individual=pure_prem_InverseGaussianPoisson_copula_beta+pure_prem_copula_TPL+pure_prem_InvGaussPoisson_copula_damage+pure_prem_InverseGaussianPoisson_other_copula+
    pure_prem_InverseGaussianBeta_Theft_copula+pure_prem_copula_NormalPoisson_fire
#  pure_prem_Total_
# boxplot premi murni data total dan dibagi beberapa jenis jaminan 
  premi_murni_total_individual=data.frame(cbind(S_Total,pure_prem_GammaPoisson_Total,pure_prem_GaussianPoisson_Total, pure_prem_GaussianPoisson_Total_log,pure_prem_InverseGaussianPoisson_Total
                                     ,pure_prem_InverseGaussianPoisson_Total_copula,pure_prem_InverseGaussianBeta_Total_copula,pure_prem_Total_GLM_individual,pure_prem_Total_Copula_Individual))
  colnames(premi_murni_total_individual) <- c("Data", "Pois &\n Gamma", "Pois & \nNormal", "Pois & \nNormal Log","Pois & Inverse \nGaussian",
                                              "Pois & Inverse Gaussian \n- Truncated Gaussian \n Copula","Pois & Inverse Gaussian \n- Beta Copula","Total Univariat GLM", "Total Univariat Copula")
  df_premi_murni_total_individual <- tidyr::gather(premi_murni_total_individual, key = "Model", value = "Predicted_Values")
  df_premi_murni_total_individual$Model <- factor(df_premi_murni_total_individual$Model, levels = colnames(premi_murni_total_individual))
  mean_premi_murni_total_individual <- aggregate(Predicted_Values ~ Model, data = df_premi_murni_total_individual, FUN = mean)
  
  ggplot(df_premi_murni_total_individual, aes(x = Model, y = Predicted_Values)) +
    geom_boxplot()+
    geom_point(data = mean_premi_murni_total_individual, aes(x = Model, y = Predicted_Values), col = "gray", pch = 19)+
    geom_text(data = mean_premi_murni_total_individual, aes(x = Model, y = Predicted_Values, label = round(Predicted_Values, 4)),
              vjust = -0.5, hjust = 0.5, size = 3, col = "black")+
    labs(title = "Boxplot Premi Murni Total untuk Model Total Vs Total Univariat", 
         x = "Models", y = "Predicted Values") +
    scale_y_continuous(limits = c(0,2000),expand = c(0.1, 0))+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
