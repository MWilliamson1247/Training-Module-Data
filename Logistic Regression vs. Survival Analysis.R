
#Start
library(survival)
library(ggplot2)
library(dplyr)
library(ranger)
library(ggfortify)
library(kmed)
library(lme4)
library(ggsurvfit)


#Mock Graphs

km_trt_fit <- survfit(Surv(time, status) ~ trt, data=veteran)
autoplot(km_trt_fit)

# recode heart_disease into 2 classes
heart$heart_disease <- ifelse(heart$class == 0,
                            0,
                            1)
ggplot(data=heart, aes(x=age, y=heart_disease))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family = binomial)) 

#Dataset Creation

setwd("C:/Users/Mark.Williamson.2/Desktop/Williamson Data/R/R_data")

lds <- read.csv("LvS_dataset.csv")

#Popped is coded in a non-standard way
#Typically you will see 1=event, 0=censored. 
#We will re-code it

lds$Popped_f <-as.factor(lds$Popped)
table(lds$Popped_f)

lds$Color <-as.factor(lds$Color)
lds$Color <- relevel(lds$Color, ref = "Red")

head(lds)
length(lds$Trial) #120

lds %>%
  group_by(Popped) %>%
  summarise(mean  = mean(Size),
            sd    = sd(Size))




#LOGISTIC REGRESSION: SIMPLE CATEGORICAL
table(lds$Color, lds$Popped)

logistic_cat <-glm(Popped_f ~ Color, family=binomial(), data=lds)
summary(logistic_cat) #significant

exp_coef <- exp(coef(logistic_cat))
exp_ci   <- exp(confint(logistic_cat))
cbind(Odds_Ratio = exp_coef, exp_ci)

l_cat_graph <- data.frame(Label=c("Blue vs. Red", "Green vs. Red"),
                          OR   =c(0.3, 0.3),
                          LCL  =c(0.1, 0.1),
                          UCL  =c(0.8, 0.8))

ggplot(l_cat_graph, aes(x=OR, y=Label)) +
  geom_point(size=3) +
  geom_vline(xintercept=1, color="red")+
  geom_errorbar(aes(xmin=LCL, xmax=UCL), width=0.2) +
  labs(y="Color Comparisons", x="Odds Ratio")+
  scale_x_continuous(limits=c(0,2))+
  theme_bw()

#SURVIVAL ANALYSIS: SIMPLE CATEGORICAL

survival_cat <- coxph(Surv(Time, Popped) ~ Color, data=lds)
summary(survival_cat)

survfit2(Surv(Time, Popped) ~ Color, data=lds) |>
  ggsurvfit()

cox.zph(survival_cat) #significant, fits assumption


#LOGISTIC REGRESSION: SIMPLE NUMERICAL
logistic_num <-glm(Popped_f ~ Size, family=binomial(), data=lds)
summary(logistic_num) #Significant

ggplot(data=lds, aes(x=Size, y=Popped))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family = binomial)) 


#SURVIVAL ANALYSIS: SIMPLE NUMERICAL
survival_num <-coxph(Surv(Time, Popped) ~ Size, data=lds)
summary(survival_num) #Significant

cox.zph(survival_num)

#LOGISTIC REGRESSION: MULTIPLE PREDICTORS
logistic_mult <-glm(Popped_f ~ Color + String + Icon + Size + Height, 
                    family=binomial(), data=lds)
summary(logistic_mult) #Color, Size, significant

exp_coef <- exp(coef(logistic_mult))
exp_ci   <- exp(confint(logistic_mult))
cbind(Odds_Ratio = exp_coef, exp_ci)

#SURVIVAL ANALYSIS: MULTIPLE PREDICTORS
survival_mult <-coxph(Surv(Time, Popped) ~ Color + String + Icon + Size + Height,
                     data=lds)
summary(survival_mult) #Significant

cox.zph(survival_mult)

