
#Load Packages
library(naniar)
library(ggplot2)
library(dplyr)
library(lme4)
library(mice)
################################################################################

#CHANGE AROUND INSOMNIA

#Load Datasets
setwd("C:/Users/Mark.Williamson.2/Desktop/Williamson Data/R/R_data") #<-your working directory will be different

brain_full <- read.csv("brain_miss_full.csv")

brain_MCAR <- read.csv("brain_miss_MCAR.csv")
brain_MAR  <- read.csv("brain_miss_MAR.csv")
brain_MNAR <- read.csv("brain_miss_MNAR.csv")

################################################################################

#Full Model

head(brain_full)

  #summary table
full_sum <- brain_full %>%
  group_by(Type,Sex) %>%
  summarise(mean_I_full  = mean(Insomnia),
              sd_I_full  = sd(Insomnia),
                  error  = qt(0.975,df=n()-1)*sd_I_full/sqrt(n()),
              ul_I_full  = mean_I_full + error,
              ll_I_full  = mean_I_full - error)
full_sum

  #graphs
full_g1 <- ggplot(data=brain_full, aes(y=Insomnia, x=Brain_wt, fill=Type))+
  geom_point()+
  geom_smooth(method="lm")+
  theme(legend.position = "bottom")
full_g1

full_g2 <-ggplot(data=full_sum, aes(y=mean_I_full, x=Sex))+
  geom_bar(aes(fill=Type), stat="identity", position=position_dodge(0.9)) +
  geom_errorbar(aes(ymin=ll_I_full, ymax=ul_I_full, group=Type), 
                width=0.1, position=position_dodge(0.9))+
  labs(title="Full Dataset", y="Mean Insomnia")+
  theme(legend.position = "bottom")
full_g2

  #statistical model
full_lm <- lm(Insomnia~ Type, data=brain_full)
summary(full_lm)

full_lmm <- glmer(Insomnia ~Brain_wt + Sex + Region + (1|Type),
                        data=brain_full)
summary(full_lmm)
################################################################################


#Missing Completely at Random (MCAR)

head(brain_MCAR)

  #summary table
MCAR_sum <- na.exclude(brain_MCAR) %>%
  group_by(Type,Sex) %>%
  summarise(mean_I_MCAR  = mean(Insomnia),
            sd_I_MCAR  = sd(Insomnia),
            error  = qt(0.975,df=n()-1)*sd_I_MCAR/sqrt(n()),
            ul_I_MCAR  = mean_I_MCAR + error,
            ll_I_MCAR  = mean_I_MCAR - error)
MCAR_sum

  #graphs
MCAR_g1 <- ggplot(data=brain_MCAR, aes(y=Insomnia, x=Brain_wt, fill=Type))+
  geom_point()+
  geom_smooth(method="lm")+
  theme(legend.position = "bottom")
MCAR_g1

MCAR_g2 <-ggplot(data=MCAR_sum, aes(y=mean_I_MCAR, x=Sex))+
  geom_bar(aes(fill=Type), stat="identity", position=position_dodge(0.9)) +
  geom_errorbar(aes(ymin=ll_I_MCAR, ymax=ul_I_MCAR, group=Type), 
                width=0.1, position=position_dodge(0.9))+
  labs(title="MCAR Dataset", y="Mean Insomnia")+
  theme(legend.position = "bottom")
MCAR_g2

  #statistical model -> DROPPED
MCAR_lm <- lm(Insomnia~ Type, data=brain_MCAR)
summary(MCAR_lm) #N=107

MCAR_lmm <- lmer(Insomnia~ Brain_wt + Sex + Region + (1|Type),
                  data=brain_MCAR)
summary(MCAR_lmm)
################################################################################


#Missing at Random (MAR)

head(brain_MAR)

  #summary table
MAR_sum <- na.exclude(brain_MAR) %>%
  group_by(Type,Sex) %>%
  summarise(mean_I_MAR  = mean(Insomnia),
            sd_I_MAR  = sd(Insomnia),
            error  = qt(0.975,df=n()-1)*sd_I_MAR/sqrt(n()),
            ul_I_MAR  = mean_I_MAR + error,
            ll_I_MAR  = mean_I_MAR - error)
MAR_sum

  #graphs
MAR_g1 <- ggplot(data=brain_MAR, aes(y=Insomnia, x=Brain_wt, fill=Type))+
  geom_point()+
  geom_smooth(method="lm")+
  theme(legend.position = "bottom")
MAR_g1

MAR_g2 <-ggplot(data=MAR_sum, aes(y=mean_I_MAR, x=Sex))+
  geom_bar(aes(fill=Type), stat="identity", position=position_dodge(0.9)) +
  geom_errorbar(aes(ymin=ll_I_MAR, ymax=ul_I_MAR, group=Type), 
                width=0.1, position=position_dodge(0.9))+
  labs(title="MAR Dataset", y="Mean Insomnia")+
  theme(legend.position = "bottom")
MAR_g2


  #statistical model -> DROPPED
MAR_lm <- lm(Insomnia~ Type, data=brain_MAR)
summary(MAR_lm) #N=107

MAR_lmm <- lmer(Insomnia~ Brain_wt + Sex + Region + (1|Type),
                 data=brain_MAR)
summary(MAR_lmm)

  #statistical model -> SIMPLE IMPUTATION

brain_MAR$Insomnia_SI <- impute_mean(brain_MAR$Insomnia)

MAR_Ig1 <- ggplot(data=brain_MAR, aes(y=Insomnia_SI, x=Brain_wt, fill=Type))+
  geom_point()+
  geom_smooth(method="lm")+
  theme(legend.position = "bottom")
MAR_Ig1

MAR_I_lm <- lm(Insomnia_SI~ Type, data=brain_MAR)
summary(MAR_I_lm) #N=107

MAR_I_lmm <- lmer(Insomnia_SI~ Brain_wt + Sex + Region + (1|Type),
                data=brain_MAR)
summary(MAR_I_lmm)
################################################################################


#Missing Not at Random (MNAR)

head(brain_MNAR)

  #summary table
MNAR_sum <- na.exclude(brain_MNAR) %>%
  group_by(Type,Sex) %>%
  summarise(mean_I_MNAR  = mean(Insomnia),
            sd_I_MNAR  = sd(Insomnia),
            error  = qt(0.975,df=n()-1)*sd_I_MNAR/sqrt(n()),
            ul_I_MNAR  = mean_I_MNAR + error,
            ll_I_MNAR  = mean_I_MNAR - error)
MNAR_sum

  #graphs
MNAR_g1 <- ggplot(data=brain_MNAR, aes(y=Insomnia, x=Brain_wt, fill=Type))+
  geom_point()+
  geom_smooth(method="lm")+
  theme(legend.position = "bottom")
MNAR_g1

MNAR_g2 <-ggplot(data=MNAR_sum, aes(y=mean_I_MNAR, x=Sex))+
  geom_bar(aes(fill=Type), stat="identity", position=position_dodge(0.9)) +
  geom_errorbar(aes(ymin=ll_I_MNAR, ymax=ul_I_MNAR, group=Type), 
                width=0.1, position=position_dodge(0.9))+
  labs(title="MNAR Dataset", y="Mean Insomnia")+
  theme(legend.position = "bottom")
MNAR_g2

  #statistical model -> DROPPED
MNAR_lm <- lm(Insomnia~ Type, data=brain_MNAR)
summary(MNAR_lm) #N=99

MNAR_lmm <- lmer(Insomnia~ Brain_wt + Sex + Region + (1|Type),
                data=brain_MNAR)
summary(MNAR_lmm)

  #statistical model -> SOPHISTICATED IMPUTATION
    #Variables: ID  Type Sex Insomnia Region Brain_wt Biomarker1 Biomarker2 Biomarker3

    #set up as factors as needed
brain_MNAR$Type <-as.factor(brain_MNAR$Type)
brain_MNAR$Sex <-as.factor(brain_MNAR$Sex)
brain_MNAR$Region <-as.factor(brain_MNAR$Region)

    #imputation step
init =mice(brain_MNAR, maxit=0)
meth=init$method
predM=init$predictorMatrix

    #set up design for imputation
predM[,1] <-c(rep(0, length(predM[1,])))
predM

    #skip non-missing
meth[c("ID")]=""
meth[c("Type")]=""
meth[c("Sex")]=""
meth[c("Region")]=""
meth[c("Biomarker1")]=""
meth[c("Biomarker2")]=""
meth[c("Biomarker3")]=""

    #set fill pattern for missing
meth[c("Insomnia")]="norm"
meth[c("Brain_wt")]="norm"

    #multiple imputation
set.seed(1234)
MNAR_impute <- mice(brain_MNAR, method=meth, predictorMatrix=predM, m=3)

    #get individual datasets
MNAR_impute_1 <-complete(MNAR_impute, 1)
MNAR_impute_2 <-complete(MNAR_impute, 2)
MNAR_impute_3 <-complete(MNAR_impute, 3)

    #check graph - MNAR_impute_1
MNAR_I_g1 <- ggplot(data=MNAR_impute_1, aes(y=Insomnia, x=Brain_wt, fill=Type))+
  geom_point()+
  geom_smooth(method="lm")+
  theme(legend.position = "bottom")
MNAR_I_g1

    #check models - MNAR_impute_1
MNAR_I_lm <- lm(Insomnia~ Type, data=MNAR_impute_1)
summary(MNAR_I_lm) #N=120

MNAR_I_lmm <- lmer(Insomnia~ Brain_wt + Sex + Region + (1|Type),
                 data=MNAR_impute_1)
summary(MNAR_I_lmm)
  