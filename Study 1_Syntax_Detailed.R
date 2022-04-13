library(tidyverse)
library(rstatix)
library(ggpubr)
library(ez)

options(scipen =999)

data<-read.csv(file.choose())

##Data preparation------------------------------------------------------------------------

View(data)
names(data)[1]<-"Gender"


####Independent Variable Coding
#Gender: 0 = Male, 1 =Female 
#Condition: 0 = Friend, 1 = Mating
#Similarity: 1 = Similar Targets, 0 =non Similar Targets 

#1 Convert to Factors
data$Gender<-factor(data$Gender)
data$Condition<-factor(data$Condition)
data$Similarity<-factor(data$Similarity)
data$id<-factor(data$id)

#2 Dependent Variables
#liking
#pa = happy / pleased
#na = frustrated / irritated
#totla = pa-na

data$liking<-rowMeans(data[4:7])
data$pa<-rowMeans(data[8:9])
data$na<-rowMeans(data[10:11])
data$total<-data$pa-data$na

##-------------------------------------Summary------------------------------------------------

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
  get_summary_stats(emotion, type = "mean_sd")

##pa
data %>%
  group_by(Similarity, Condition) %>%
  get_summary_stats(pa, type = "mean_sd")
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


##post-hoc

one.way<- data %>%
  group_by(Similarity) %>%
  anova_test(dv = liking, wid = id, between = Condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way


pwc <- data %>%
  group_by(Condition) %>%
  pairwise_t_test(liking ~ Similarity, p.adjust.method = "bonferroni")
pwc
summary(pwc)

pwc2 <- data %>%
  group_by(Condition) %>%
  pairwise_t_test(
    liking ~ Similarity, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )  # Remove details
pwc2


##--------------------------------DV: Anticipated-Emotion----------------------------------------


#pa
threeway_pa<-ezANOVA(data=data, dv=.(pa), wid=.(id), within=.(Similarity),
               between=.(Gender,Condition), type=3, detailed=T)

threeway_pa

twoway_pa<-ezANOVA(data=data, dv=.(pa), wid=.(id), within=.(Similarity),
              between=.(Condition), type=3, detailed=T)

twoway_pa

#na
threeway_na<-ezANOVA(data=data, dv=.(na), wid=.(id), within=.(Similarity),
                   between=.(Gender,Condition), type=3, detailed=T)

threeway_na

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

###post-hoc


one.way<- data %>%
  group_by(Similarity) %>%
  anova_test(dv = total, wid = id, between = Condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way



pwc2 <- data %>%
  group_by(Condition) %>%
  pairwise_t_test(
    na ~ Similarity, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )  # Remove details
pwc2

options(scipen=999)


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


#pa
100.92 / (100.92 +393.8989)
21.099414592 /(21.099414592+ 137.8490)

#na
100.9228660 / (100.9228660+393.8989 )
21.0994145 / (21.0994145 +137.8490)

#liking
cohens_d(
  Similarity ~ liking,
  data = data,
  correction = FALSE,
  pooled_sd = TRUE,
  paired = T,
  ci = 0.95
)

data.mating<-data[data$Condition ==1,]

data.aff %>% cohens_d(liking ~ Similarity, paired = TRUE)
data.mating %>% cohens_d(liking ~ Similarity, paired = TRUE)
data.aff %>% cohens_d(na~ Similarity, paired = TRUE)
data.mating %>% cohens_d(na ~ Similarity, paired = TRUE)

