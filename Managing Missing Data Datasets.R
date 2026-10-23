
#Load Packages
library(naniar)
library(ggplot2)
library(dplyr)
################################################################################


#Create Small Dataset
small_miss <- data.frame(ID=c("01","02","03","04","05","06","07","08","09","10"),
                         Y =c(1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10),
                         X1=c(10,9 ,8 ,7 ,6 ,5 ,4 ,3 ,2 ,1 ),
                         X2=c(20,NA,16,14,12,10,8 ,NA,4 ,2 ),
                         X3=c(NA,19,NA,NA,16,15,14,13,NA,NA),
                         X4=c(NA,NA,NA,NA,15,NA,NA,NA,19,20))
################################################################################


#Functions for detecting missing data
  #single variable
any_na(small_miss$X1)
any_na(small_miss$X2)

n_miss(small_miss$X1)
n_miss(small_miss$X2)

  #whole dataset
sapply(small_miss, function(x) sum(is.na(x)))
miss_var_summary(small_miss)
miss_case_summary(small_miss)

#Tables for detecting missing data
miss_var_table(small_miss)
miss_case_table(small_miss)

#Figures for detecting missing data
vis_miss(small_miss)
vis_miss(small_miss, cluster=TRUE)

gg_miss_var(small_miss)
################################################################################


#Create Example Dataset

#Type:       Human, Zombie, Werewolf
#Sex:        Male, Female
#Insomnia:   numerical
#Brain_wt:   numerical
#Region:    A, B, C, D
#Biomarker1: numerical
#Biomarker2: numerical
#Biomarker3: numerical

set.seed(1234)
hum_mB <-c(rnorm(20, mean=3.0,sd=0.5))
hum_fB <-c(rnorm(20, mean=2.5,sd=0.5))
zom_mB <-c(rnorm(20, mean=50, sd=5.0))
zom_fB <-c(rnorm(20, mean=45, sd=5.0))
wer_mB <-c(rnorm(20, mean=20, sd=7.0))
wer_fB <-c(rnorm(20, mean=12, sd=4.0))

rho_h <-  0.15
rho_z <-  0.20
rho_w <-  0.50

set.seed(1234)
hum_mI <-(rho_h * hum_mB) + sqrt(1-rho_h**2) * rnorm(n=20, mean=10, sd=2)
hum_fI <-(rho_h * hum_fB) + sqrt(1-rho_h**2) * rnorm(n=20, mean=12, sd=2)
zom_mI <-(rho_z * zom_mB) + sqrt(1-rho_z**2) * rnorm(n=20, mean=50, sd=4)
zom_fI <-(rho_z * zom_fB) + sqrt(1-rho_z**2) * rnorm(n=20, mean=55, sd=4)
wer_mI <-(rho_w * wer_mB) + sqrt(1-rho_w**2) * rnorm(n=20, mean=30, sd=5)
wer_fI <-(rho_w * wer_fB) + sqrt(1-rho_w**2) * rnorm(n=20, mean=28, sd=5)

#Full Dataset

brain_miss <-data.frame(Type  =c(rep("Human",40), rep("Zombie",40), rep("Werewolf",40)),
                     Sex      =c(rep("M",20), rep("F",20), rep("M",20), rep("F",20), rep("M",20), rep("F",20)),
                     Insomnia =c(hum_mI, hum_fI, zom_mI, zom_fI, wer_mI, wer_fI),
                     Brain_wt0=c(hum_mB, hum_fB, zom_mB, zom_fB, wer_mB, wer_fB),
                     Region   =c(rep(c("A","B","C","D"), 30)))
brain_miss <- brain_miss %>%
  mutate(Brain_wt =  case_when(Region== "A"  ~  Brain_wt0 * 1.05,
                               Region== "B"  ~  Brain_wt0 * 1.45,
                               Region== "C"  ~  Brain_wt0 * 0.80,
                               Region== "D"  ~  Brain_wt0 * 1.20))

brain_miss$Brain_wt <-round(brain_miss$Brain_wt,2)
brain_miss$Insomnia <-round(brain_miss$Insomnia, 2)

brain_miss <- subset(brain_miss, select = -Brain_wt0)

rho_b1 <-  0.6
rho_b2 <-  0.5
rho_b3 <-  0.4

set.seed(1234)
Biomarker1 <-(rho_b1 * brain_miss$Brain_wt) + sqrt(1-rho_b1**2) * rnorm(n=120, mean=120, sd=15)
Biomarker2 <-(rho_b2 * brain_miss$Brain_wt) + sqrt(1-rho_b2**2) * rnorm(n=120, mean=25, sd=4)
Biomarker3 <-(rho_b3 * brain_miss$Brain_wt) + sqrt(1-rho_b3**2) * rnorm(n=120, mean=80, sd=33)

brain_miss$Biomarker1 <- round(Biomarker1,2)
brain_miss$Biomarker2 <- round(Biomarker2,2)
brain_miss$Biomarker3 <- round(Biomarker3,2)

head(brain_miss)

#Output full datset
setwd("C:/Users/Mark.Williamson.2/Desktop/Williamson Data/R/R_data") #<-your working directory will be different
write.csv(brain_miss, file = "brain_miss_full.csv")





################################################################################