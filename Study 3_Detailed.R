library(tidyverse)
library(rstatix)
library(ggpubr)
library(ez)
library(DescTools)

options(scipen =999)

data<-read.csv(file.choose())

##Data preparation------------------------------------------------------------------------

View(data)
names(data)[1]<-"dur"
head(data)
####Independent Variable Coding
#Gender: 0 = Male, 1 =Female 
#Condition: 0 = Friend, 1 = Mating
#Similarity: 1 = Similar Targets, 0 =non Similar Targets 

#1 Convert to Factors
data$Gender<-factor(data$GENDER)
data$Condition<-factor(data$Condition)
data$Similarity<-factor(data$Sim)
data$id<-factor(data$id)
data$unique<-(data$HI2+data$HI5+data$HI6+data$HI7)/4
data$aff<-(data$A1+data$A2+data$A3+data$A4+data$A5+data$A6+data$A7+data$A8+data$E9)/9
data$compete<-(data$VC4+data$VI7)/2


head(data)
#2 Dependent Variables
#liking
#pa = happy / pleased
#na = frustrated / irritated
#totla = pa-na

data$liking<-rowMeans(data[c(10,11)])
data$pa<-rowMeans(data[c(13,17)])
data$na<-rowMeans(data[c(15,16)])
data$total<-data$pa-data$na

data$id<-c(1:168,1:168)


CronbachAlpha(data.frame(data$A1,data$A2,data$A3,data$A4,data$A5,data$A6,data$A7,data$A8,data$E9))
alpha(data[c(10,11)])
alpha(data[c(13,17)])
alpha(data[c(15,16)])

##-------------------------------------Summary------------------------------------------------


mean(data$Duration..in.seconds.)

#DEMOGRAPHIC
mean(data$age)
sd(data$age)

#SIM=1 SIMILAR / GOAL =1 MATING

##liking
sum1<-data %>%
  group_by(Similarity, Condition, Gender) %>%
  get_summary_stats(liking, type = "mean_sd")

sum1

##aggregated emotion
data %>%
  group_by(Similarity, Condition) %>%
  get_summary_stats(total, type = "mean_sd")

##pa
data %>%
  group_by(Similarity, Condition) %>%
  get_summary_stats(pa, type= "mean_sd")
##na
data %>%
  group_by(Similarity, Condition) %>%
  get_summary_stats(na, type = "mean_sd")

#Visualization
bxp1 <- ggboxplot(
  data, x = "Condition", y = "liking", 
  color = "Sim", palette = "jco")
bxp1


bxp2 <- ggboxplot(
  female, x = "Condition", y = "emotion", 
  color = "Sim", palette = "jco")
bxp2

bxp3 <- ggboxplot(
  male, x = "Condition", y = "emotion", 
  color = "Sim", palette = "jco")
bxp3


##MAIN ANALYSIS ----->

##--------------------------------DV: Liking------------------------------------------------
  

threeway1<-ezANOVA(data=data, dv=.(liking), wid=.(id), within=.(Similarity),
               between=.(Gender,Condition), type=3, detailed=T)

threeway1


##post-hoc: significant two-way interaction
twoway1<-ezANOVA(data=data, dv=.(liking), wid=.(id), within=.(Similarity),
             between=.(Condition), type=3, detailed=T)
twoway1

#When statistically significant three-way interaction ABSENT:
#you need to determine whether you have any statistically significant 
#two-way interaction from the ANOVA output. 

#A significant two-way interaction can be followed up by a simple main effect analysis, 
#which can be followed  up by simple pairwise comparisons if significant.


#Simple main effect
#effect of Similarity at each Condition
one.way <- data %>%
  group_by(Similarity) %>%
  anova_test(dv = na, wid = id, between = Condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way


#Pairwise comparisons
pwc <- data %>%
  group_by(Similarity) %>%
  pairwise_t_test(na ~ Condition, p.adjust.method = "bonferroni")
pwc
summary(pwc)



pwcpa <- data %>%
  group_by(Condition) %>%
  pairwise_t_test(
    pa ~ Similarity, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )  # Remove details
pwcpa

pwcna <- data %>%
  group_by(Condition) %>%
  pairwise_t_test(
    na ~ Similarity, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )  # Remove details
pwcna

pwc2 <- data %>%
  group_by(Condition) %>%
  pairwise_t_test(
    liking ~ Similarity, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )  # Remove details
pwc2

DAT2<-data[c(1:120,150:168,169:288,318:336),]



22.43535/ (22.43535+275.1889 )
26.22127 / ( 26.22127+ 192.1887)
15.47127 /(15.47127+ 192.1887)
##--------------------------------DV: Anticipated-Emotion----------------------------------------


#pa
twoway_pa<-ezANOVA(data=data, dv=.(pa), wid=.(id), within=.(Similarity),
              between=.(Condition), type=3, detailed=T)

twoway_pa

#na
twoway_na<-ezANOVA(data=data, dv=.(na), wid=.(id), within=.(Similarity),
                 between=.(Condition), type=3, detailed=T)

twoway_na

#total (reported)
threeway_total<-ezANOVA(data=data, dv=.(total), wid=.(id), within=.(Similarity),
                   between=.(Gender,Condition), type=3, detailed=T)

threeway_total

twoway1<-ezANOVA(data=data, dv=.(total), wid=.(id), within=.(Similarity),
                 between=.(Condition), type=3, detailed=T)

twoway1

26.81360/ (26.81360+276.4721 )
54.33908/ (54.33908+186.6579 )
18.16349/ (18.16349+186.6579 )


52.331578/ (52.331578+391.7927)
9.470832/ ( 9.470832+191.1177)
20.238689/ (20.238689+191.1177)

#post-hoc


one.way<- data %>%
  group_by(Similarity) %>%
  anova_test(dv = total, wid = id, between = Condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way


one.way<- data %>%
  group_by(Condition) %>%
  anova_test(dv = total, wid = id, within = Similarity) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

pwce <- data %>%
  group_by(Condition) %>%
  pairwise_t_test(total~ Similarity, p.adjust.method = "bonferroni")
pwce
summary(pwce)

pwce <- data %>%
  group_by(Condition) %>%
  pairwise_t_test(na~ Similarity, p.adjust.method = "bonferroni")
pwce
summary(pwce)

pwce <- data %>%
  group_by(Condition) %>%
  pairwise_t_test(pa~ Similarity, p.adjust.method = "bonferroni")
pwce
summary(pwce)



pwc2e <- data %>%
  group_by(Condition) %>%
  pairwise_t_test(
    total ~ Similarity, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )  # Remove details
pwc2e

#----------------------------Moderator: Trait Affiliation------------------------------------------------


a<-lm(liking~Similarity*aff,data = data.mating)
summary(a)

library(emmeans)
library(lme4)
d2<-data[1:168,]
d3<-data[169:336,]

mean(d2$AGE_1)
sd(d2$AGE_1)

d2$AGE_1[104]<-23

sum(d2$GENDER)-168

d2$likdiff<-d2$liking-d3$liking
d2$padiff<-d2$pa-d3$pa
d2$nadiff<-d2$na-d3$na
d2$totaldiff<-d2$total-d3$total

#---------------------liking------------------------
model1<-lm(nadiff~aff*Condition,data = d2)
summary(model1)


model1<-lm(liking~aff*Similarity,data = data.mating)
summary(model1)

model1<-lmer(liking~aff*Similarity+(1|id),data = data.aff)
summary(model1)
confint(model1)



library(lmerTest)


model1<-lmer(liking~aff*Similarity+(1|id),REML = F,data = data.mating)
summary(model1)

mylist <- list(aff=c(1,7), Similarity=c("1","0"))

emmip(model1, Similarity~aff, engine = get_emm_option("graphics.engine"),at=data.mating, CIs =T)

#---------------------PA----------------------------

model2<-lm(padiff~Condition*aff,data = data.aff)
summary(model2)

mylist <- list(aff=c(1,7), Condition=c("1","0"))

emmip(model2, Condition~aff, at=mylist, CIs =T)


#---------------------NA----------------------------

model3<-lm(na~Condition*aff,data = d2)
summary(model3)
model3b<-lm(na~Condition*aff,data = d3)
summary(model3b)

mylist <- list(aff=c(1,7), Condition=c("1","0"))

emmip(model3, Condition~aff, at=mylist, CIs =T)


#---------------------total-------------------------

model4<-lm(total~Condition*aff,data = d2)
summary(model4)

model4b<-lm(total~Condition*aff,data = d3)
summary(model4b)

mylist <- list(aff=c(1,7), Sim=c("1","0"))
par(mfrow = c(2, 2))
emmip(model1, Sim~aff, at=mylist, CIs =T)
emmip(model4b, Condition~aff, at=mylist, CIs =T)

model1<-lmer(liking~aff*Sim+(1|id),REML = F,data = data.mating)
summary(model1)
anova(model1)
confint(model1)
model2<-lmer(liking~aff*Sim+(1|id),REML = F,data = data.aff)
summary(model2)
confint(model2)
anova(model1)
ef3 <- effect(term="aff:Sim", xlevels= list(Sim=c(0, 1)), mod=model1)
efdata3<-as.data.frame(ef3) #convert the effects list to a data frame
efdata3 #print effects data frame
efdata3$Similarity<-factor(efdata3$Sim, labels = c("non-similar", "similar")) # Make our values into factors

ggplot(efdata3, aes(x=aff, y=fit, color = Similarity)) + ylim(1,7)+
  geom_line(size=1.2,aes(linetype =Similarity)) +
  scale_color_manual(values=c('dodgerblue','firebrick3'))+
  labs( x= "Agreeableness", y="General liking") + theme_classic() + theme(text=element_text(size=12))
library(lmerTest)

View(data.AFF)


ef3 <- effect(term="aff:Sim", xlevels= list(Sim=c(0, 1)), mod=model2)
efdata3<-as.data.frame(ef3) #convert the effects list to a data frame
efdata3 #print effects data frame
efdata3$Similarity<-factor(efdata3$Sim, labels = c("non-similar", "similar")) # Make our values into factors

ggplot(efdata3, aes(x=aff, y=fit, color = Similarity)) + ylim(1,7)+
  geom_line(size=1.2,aes(linetype =Similarity)) +
  scale_color_manual(values=c('dodgerblue','firebrick3'))+
  labs(title = "mate-seeking condition", x= "Agreeableness", y="General liking") + theme_classic() + theme(text=element_text(size=12))


View(data.mating)



#### Plots and tables for our models #### 

library(nlme)
library(effects)
library(ggplot2)
library(psych)

# Two-way interaction plot
model1
# Create effects
ef1 <- effect(term="aff*Similarity", xlevels= list(Similarity=c(0, 1)), mod=model1)
efdata1<-as.data.frame(ef1) #convert the effects list to a data frame
efdata1 #print effects data frame
efdata1$'Similarity of targets'<-factor(efdata1$Similarity, labels = c("dissimilar", "similar")) # Make our values into factors

# Now the two-way interaction plot!
a<-ggplot(efdata1, aes(x=aff, y=fit, color = Similarity)) + ylim(1,7)+
  geom_line(size=1.2,aes(linetype =Similarity)) +
  scale_color_manual(values=c('dodgerblue','firebrick3'))+
  labs(x= "Agreeableness", y="General liking") + theme_classic() + theme(text=element_text(size=12))


model1<-lm(liking~aff*Similarity,data = data.mating)
summary(model1)


ef2 <- effect(term="aff*Similarity", xlevels= list(Similarity=c(0, 1)), mod=model2)
efdata2<-as.data.frame(ef2) #convert the effects list to a data frame
efdata2 #print effects data frame
efdata2$Similarity<-factor(efdata2$Similarity, labels = c("non-similar", "similar")) # Make our values into factors

# Now the two-way interaction plot!
b<-ggplot(efdata2, aes(x=aff, y=fit, color = Similarity)) + ylim(1,7)+
  geom_line(size=1.2,aes(linetype =Similarity)) +
  scale_color_manual(values=c('dodgerblue','firebrick3'))+
  labs(title = "control condition", x= "Agreeableness", y="General liking") + theme_classic() + theme(text=element_text(size=12))


model2<-lm(liking~aff*Similarity,data = data.aff)
summary(model2)
a+b

library(patchwork)

par(mfrow=c(2,2))
library(jtools)

simple_slopes(model1, aff, Similarity, mod2 = NULL, modxvals = NULL,
           mod2vals = NULL, centered = NULL, standardize = FALSE,
           cond.int = FALSE, johnson_neyman = TRUE, jnplot = FALSE,
           jnalpha = 0.05, robust = FALSE, robust.type = "HC3", digits = 3,
           n.sd = 1)




library(survey)

probe_interaction(model1, pred = aff, modx = Condition, cond.int = TRUE,
                  interval = TRUE,  jnplot = TRUE)




library(interactions)
sim_slopes(model1, pred = aff, modx = Similarity)
library(lmerTest)

library(interactions)
sim_slopes(model2, pred = aff, modx = Similarity)





##--------------------------------Effectsize------------------------------------------------


#effect size: partial eta squared

#Liking
72.53469565/(297.17524+ 72.53469565)
4.63218379/ (91.77528+ 4.63218379)
25.71949906 / (91.77528  + 25.71949906)


#Total Emotion
109.85 /( 324.9679+ 109.85)
19.16 /(19.16+850.3303)
6.21621988 /(6.21621988+324.9679)


#liking
cohens_d(
  Similarity ~ liking,
  data = data.aff,
  correction = FALSE,
  pooled_sd = TRUE,
  paired = T,
  ci = 0.95
)

data.mating<-data[data$Condition ==1,]
data.aff<-data[data$Condition ==0,]
data.aff %>% cohens_d(liking ~ Similarity, paired = TRUE)
data.mating %>% cohens_d(liking ~ Similarity, paired = TRUE)
data.aff %>% cohens_d(pa ~ Similarity, paired = TRUE)
data.mating %>% cohens_d(pa ~ Similarity, paired = TRUE)
data.aff %>% cohens_d(na ~ Similarity, paired = TRUE)
data.mating %>% cohens_d(na ~ Similarity, paired = TRUE)


#---------------------------------------------------#----
  

devtools::install_github("doomlab/MeMoBootR")

1

library(MeMoBootR)

data.mating$Similarity<-as.factor(data.mating$Similarity)

mod<-moderation1(y = "liking",
                 m ="Similarity",
                 x = "aff",
                 df = data.mating)

summary(mod$model1)
summary(mod$model1low)
summary(mod$model1high)
summary(mod$interpretation)
mod$graphslopes
