library(data.table)
library(tidyverse)
library(UWbe536)
setwd("C:/Users/arthur/Desktop/AU2019/Biost 536")
data <- read_rds("Project536-2019.rds")
#set up smoking status variable & BMI
data$BMI <- ((data$weight)^2) / data$height
data$smkst <- ifelse(data$packyrs>0 & data$yrsquit ==0, 2, ifelse(data$packyrs>0&data$yrsquit>0,1,0))
#blood test and blood pressure  indicator 
data$abldl <- ifelse(data$ldl > 150, 1, 0)
data$abglu <- ifelse(data$glu <70 | data$glu >105, 1, 0)
data$abalb <- ifelse(data$alb <2.4 | data$alb > 5.4, 1, 0)
data$abfib <- ifelse(data$fib <150 | data$fib >350, 1, 0)
data$abcrt <- ifelse(data$crt <0.7 | data$crt >1.3, 1, 0)
data$abwbc <- ifelse(data$wbc <4.5 | data$wbc >11, 1, 0)
data$abplt <- ifelse(data$plt <150 | data$plt >350, 1, 0)
data$hbp <- ifelse(data$sbp > 120 & data$dbp >80, 1, 0) 
data$badblood <-ifelse(as.numeric(apply(data[,25:30], 1, sum)) > 2, 1, 0)
# set up linear spline varibale 
data <- data %>% 
  mutate(s1 = dsst,
         s2 = (dsst-20)*(dsst>20),
         s3 = (dsst-40)*(dsst>40),
         s4 = (dsst-60)*(dsst>60),
         s5 = (dsst-80)*(dsst>80),
         dsst2 = dsst^2)
crmd.l <- glm(case~dsst,data=data, family= binomial)
crmd.lnq <- glm(case~dsst + dsst2,data =data, family = binomial)
crmd.sp <- glm(case~s1+s2+s3+s4+s5, data =data, family = binomial)
lincom(crmd.l)
lincom(crmd.lnq,c("40*dsst+4000*dsst2==0"))
lincom(crmd.sp,c("40*s1+40*s2+30*s3+10*s4==0"))
cfmd.l <- glm(case~dsst+age+male+nonwhite+factor(educ)+BMI+packyrs+factor(smkst)+gmalcoh+badblood,
              data=data,family = binomial)
cfmd.lnq <- glm(case~dsst+dsst2+age+male+nonwhite+factor(educ)+BMI+packyrs+factor(smkst)+gmalcoh+badblood,
                data=data,family=binomial)
cfmd.sp <- glm(case~s1+s2+s3+s4+s5+age+male+nonwhite+factor(educ)+BMI+packyrs+factor(smkst)+gmalcoh+badblood,
               data=data,family=binomial)
lincom(cfmd.l)
lincom(cfmd.lnq,c("40*dsst+4000*dsst2==0"))
lincom(cfmd.sp,c("40*s1+40*s2+30*s3+10*s4==0"))
emmd.l <- glm(case~dsst+age+male+nonwhite+factor(educ)+BMI+packyrs+factor(smkst)+gmalcoh+badblood+dsst:age+dsst:gmalcoh,
              data=data,family = binomial)
emmd.lnq <- glm(case~dsst+dsst2+age+male+nonwhite+factor(educ)+BMI+packyrs+factor(smkst)+gmalcoh+badblood+dsst:age+dsst:gmalcoh
                +dsst2:age+dsst2:gmalcoh,
                data=data,family=binomial)
emmd.sp <- glm(case~s1+s2+s3+s4+s5+age+male+nonwhite+factor(educ)+BMI+packyrs+factor(smkst)+gmalcoh+badblood
               +s1:age+s1:gmalcoh+s2:age+s2:gmalcoh
               +s3:age+s3:gmalcoh+s4:age+s4:gmalcoh
               +s5:age+s5:gmalcoh,
               data=data, family = binomial)
summary(emmd.sp)
lincom(emmd.l,c("dsst+dsst:age+dsst:gmalcoh==0"))
lincom(emmd.lnq,c("40*dsst+40*dsst:age+40*dsst:gmalcoh+4000*dsst2+4000*dsst2:age+4000*dsst2:gmalcoh==0"))
