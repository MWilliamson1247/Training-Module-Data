library(MASS)
library(ggplot2)
library(dplyr)

#refs: https://stirlingcodingclub.github.io/simulating_data/index.html
#      https://www.thomasward.com/simulating-correlated-data/  
#      https://r-coder.com/correlation-plot-r/
#      https://www.r-bloggers.com/2020/11/simpsons-paradox-and-misleading-statistical-inference/
#############################


#T-tests
set.seed(1234)
group_A <-rnorm(100, mean=0, sd=1)
group_B <-rnorm(100, mean=1, sd=1)

ds1 <- data.frame(group=c(rep("A",100), rep("B",100)),
                  value=c(group_A, group_B))
ggplot(data=ds1, aes(y=value, x=group, fill=group))+
  geom_boxplot()
#############################


#ANOVA
set.seed(1234)
group_C <-rnorm(100, mean=3, sd=3)
group_D <-rnorm(100, mean=-5, sd=1)

ds2 <- data.frame(group=c(rep("A",100), rep("B",100), rep("C",100), rep("D",100)),
                  value=c(group_A, group_B, group_C, group_D))
ggplot(data=ds2, aes(y=value, x=group, fill=group))+
  geom_boxplot()
#############################


#Correlation
set.seed(1234)
rho1 <-  0.6
rho2 <- -0.9
rho3 <-  0.05

x0  <- rnorm(n=100, mean=0, sd=1);
x1  <- (rho1 * x0) + sqrt(1 - rho1*rho1) * rnorm(n=100, mean=1, sd=1)
x2  <- (rho2 * x0) + sqrt(1 - rho2*rho2) * rnorm(n=100, mean=2, sd=1)
x3  <- (rho3 * x0) + sqrt(1 - rho3*rho3) * rnorm(n=100, mean=3, sd=1)

ds3 <-data.frame(x0=x0, x1=x1, x2=x2, x3=x3)

cor(ds3)
ggplot(data=ds3, aes(y=x0, x=x1))+
  geom_point()+ geom_smooth(method="lm")
ggplot(data=ds3, aes(y=x0, x=x2))+
  geom_point()+ geom_smooth(method="lm")
ggplot(data=ds3, aes(y=x0, x=x3))+
  geom_point()+ geom_smooth(method="lm")
#############################


#Regression
  #G1
set.seed(1234)
mu<-c(0,0)
sigma<-rbind(c(2,-0.5),c(-0.5,2) )
g1<-as.data.frame(mvrnorm(n=100, mu=mu, Sigma=sigma))
g1$group<-c("Group 1")

  #G2
mu<-c(2,2)
sigma<-rbind(c(2,-0.1),c(-0.1,8) )
g2<-as.data.frame(mvrnorm(n=100, mu=mu, Sigma=sigma))
g2$group<-c("Group 2")

  #G3
mu<-c(3,3)
sigma<-rbind(c(2,0.5),c(0.5,1) )
g3<-as.data.frame(mvrnorm(n=100, mu=mu, Sigma=sigma))
g3$group<-c("Group 3")

ds4 <-rbind(g1,g2,g3)
head(ds4)

ggplot(data=ds4, aes(x=V1, y=V2, group=group, col=group))+
  geom_point()+ 
  geom_smooth(method='lm', se=FALSE)+
  theme(legend.position = "top")
#############################


#RM ANOVA
set.seed(1234)
rho <-0.5

g1.t0 <-rnorm(n=100, mean=150, sd=20)
g1.t1 <-(rho * g1.t0) + sqrt(1 - rho*rho) * rnorm(n=100, mean=150, sd=20)  
g1.t2 <-(rho * g1.t1) + sqrt(1 - rho*rho) * rnorm(n=100, mean=150, sd=20)  

g2.t0 <-rnorm(n=100, mean=100, sd=20)
g2.t1 <- ((rho * g2.t0) + (1 - rho) * rnorm(n=100, mean=100, sd=20))
g2.t2 <- ((rho * g2.t1) + (1 - rho) * rnorm(n=100, mean=100, sd=20)) 

ds5 <-data.frame(Group=c(rep("A",300),
                         rep("B",300)),
                 Time=c(rep("0",100), rep("1",100), rep("2",100),
                        rep("0",100), rep("1",100), rep("2",100)),
                 Measure=c(g1.t0, g1.t1, g1.t2,
                           g2.t0, g2.t1, g2.t2))

ggplot(data=ds5, aes(x=Time, y=Measure, fill=Group))+
  geom_boxplot()

ds5_sum<-ds5 %>% 
  group_by(Group,Time) %>%       
  summarise(mean=mean(Measure),
            sd = sd(Measure))
print(ds5_sum)

ds5_g1 <-data.frame(g1.t0=g1.t0, g1.t1=g1.t1, g1.t2=g1.t2)
ds5_g2 <-data.frame(g2.t0=g2.t0, g2.t1=g2.t1, g2.t2=g2.t2)

cor(ds5_g1)
cor(ds5_g2)
cor(g1.t2, g2.t2)
#############################

#Counts
set.seed(1234)
status_A <- rbinom(n=100, size=1, prob=0.5)
status_B <- rbinom(n=100, size=1, prob=0.75)

ds6 <- data.frame(Group=c(rep("A",100), rep("B",100)),
                  Status =c(status_A, status_B))

table(ds6$Group, ds6$Status)

ds6_sum<-ds6 %>% 
  group_by(Group, Status) %>%       
  summarise(count=n())
print(ds6_sum)

ggplot(data=ds6_sum, aes(x=Group, y=count, fill=as.factor(Status)))+
  geom_bar(stat="identity") +
  theme(legend.position = "none")


