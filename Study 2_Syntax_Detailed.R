  
data<-read.csv(file.choose())
#Data preparation

View(data)
names(data)[1]<-"Gender"

d<-data[data$Gender==1,]

#Gender: 0 = Male, 1 =Female 
#Condition: 0 = Friend, 1 = Mating
#Sim 1 = Sim, 0 =non Sim 

#FACTORS / IV
head(data)
data$Gender<-factor(data$Gender)
data$condition<-factor(data$condition)
data$Sim<-factor(data$Sim)
data$id<-factor(data$id)

head(data)
#DEPENDENT VARIABLES
#liking, pa, na, emotions
data$liking<-rowMeans(data[5:8])
data$pa<-rowMeans(data[9:10])
data$na<-rowMeans(data[11:12])
data$emotion<-data$pa-data$na

#MODERATORS
#appearance, mate-value, appearance
data$vig<-rowMeans(data[17:28])
data$mv<-rowMeans(data[32:37])
data$hum<-rowMeans(data[29:31])


#DEMOGRAPHIC
mean(data$age)
sd(data$age)

View(data.mating)
a<-lm(data.mating$liking~data.mating$Sim*data.mating$mv)
summary(a)

##########Summary Stats##############
sum1<-data %>%
  group_by(Sim, condition, Gender) %>%
  get_summary_stats(pa, type = "mean_sd")

sum1

data<-data[data$AC==7,]

#SIM=1 SIMILAR / GOAL =1 MATING
##liking
data %>%
  group_by(Sim, condition) %>%
  get_summary_stats(na, type = "mean_sd")
##aggregated emotions
data %>%
  group_by(Sim, condition) %>%
  get_summary_stats(emotion, type = "mean_sd")
##pa
data %>%
  group_by(Sim, condition) %>%
  get_summary_stats(pa, type = "mean_sd")
##na
data %>%
  group_by(Sim, condition) %>%
  get_summary_stats(na, type = "mean_sd")

#Visualization
bxp <- ggboxplot(
  data, x = "Condition", y = "liking", 
  color = "Sim", palette = "jco")
bxp


bxp <- ggboxplot(
  female, x = "Condition", y = "emotion", 
  color = "Sim", palette = "jco")
bxp

bxp <- ggboxplot(
  male, x = "Condition", y = "emotion", 
  color = "Sim", palette = "jco")
bxp



data.aff <- data[which(data$condition==0),]
data.mating <- data[which(data$condition==1),]
data.young<-data[which(data$age < 60),]

data.mating.sim <- data.mating[which(data.mating$Similarity==1),]

######main: liking######

library(ez)
threeway<-ezANOVA(data=data, dv=.(liking), wid=.(id), within=.(Sim),
                  between=.(Gender,condition), type=3, detailed=T)

threeway


twoway<-ezANOVA(data=data, dv=.(liking), wid=.(id), within=.(Sim),
                between=.(condition), type=3, detailed=T)
twoway


t.test(data.aff$liking~data.aff$Similarity, paired =T)
t.test(data.mating$liking~data.mating$Similarity, paired =T)

options(scipen =999)

##post-hoc
a0<-data.aff$emotion[which(data.aff$Sim == 0)]
a1<-data.aff$emotion[which(data.aff$Sim == 1)]
cor.test(a0,a1)

b0<-data.mating$emotion[which(data.mating$Sim == 0)]
b1<-data.mating$emotion[which(data.mating$Sim == 1)]
cor.test(b0,b1)



c0<-data.aff$liking[which(data.aff$Sim == 0)]
c1<-data.aff$liking[which(data.aff$Sim == 1)]
cor.test(c0,c1)

one.way<- data %>%
  group_by(sim) %>%
  anova_test(dv = liking, wid = id, between = Condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way


pwc <- data %>%
  group_by(condition) %>%
  pairwise_t_test(pa ~ Sim, p.adjust.method = "bonferroni")
pwc
summary(pwc)

pwc2 <- data %>%
  group_by(condition) %>%
  pairwise_t_test(
    liking ~ Sim, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )  # Remove details
pwc2
options(Scipen = 999)

##########emotion###############
library(ez)
three<-ezANOVA(data=data, dv=.(pa), wid=.(id), within=.(Sim),
               between=.(Gender,condition), type=3, detailed=T)

three

three2<-ezANOVA(data=data, dv=.(pa), wid=.(id), within=.(Sim),
               between=.(Gender,condition), type=3, detailed=T)

three2


twoe<-ezANOVA(data=data, dv=.(na), wid=.(id), within=.(Sim),
              between=.(condition), type=3, detailed=T)
twoe


pwce <- data %>%
  group_by(condition) %>%
  pairwise_t_test(emotion~ Sim, p.adjust.method = "bonferroni")
pwce
summary(pwce)

pwc2e <- data %>%
  group_by(condition) %>%
  pairwise_t_test(
    pa~ Sim, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )  # Remove details
pwc2e


pwc2e <- data %>%
  group_by(condition) %>%
  pairwise_t_test(
    na~ Sim, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )  # Remove details
pwc2e


t.test(data.aff$liking~data.aff$Similarity, paired =T)
t.test(data.mating$liking~data.mating$Similarity, paired =T)

options(scipen =999)

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

pwce <- data %>%
  group_by(Condition) %>%
  pairwise_t_test(
    emotion ~ Similarity, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )  # Remove details
pwce

##effect size for analysis1##
79.3224968/( 79.3224968+ 251.09286)
13.52548404/ (   13.52548404 + 68.84413)
10.37466541/ (   10.37466541 +68.84413 )


na
203.8581602038/ (488.9280+203.8581602038)
5.5176256011/  (5.5176256011+107.7839)
16.3850219901/ (  16.3850219901+ 107.7839)
pa
121.19802935/(121.19802935+387.72587)
21.01590896 /(21.01590896 +84.42182)
library(effectsize)
data.aff$Similarity<-as.factor(data.aff$Similarity)
data.mating$Similarity<-as.factor(data.mating$Similarity)

cohen.d(data.aff$liking, data.aff$Similarity,alpha=.05)
cohen.d(exp$thought, exp$condition,alpha=.05)

cohens_d(
  Similarity ~ liking,
  data = data.aff,
  correction = FALSE,
  pooled_sd = TRUE,
  paired = T,
  ci = 0.95
)



data.mating<-data[data$condition ==1,]
data.aff<-data[data$condition ==0,]
data.aff %>% cohens_d(liking ~ Sim, paired = TRUE)
data.mating %>% cohens_d(liking ~ Sim, paired = TRUE)
data.aff %>% cohens_d(pa ~ Sim, paired = TRUE)
data.mating %>% cohens_d(pa ~ Sim, paired = TRUE)
data.aff %>% cohens_d(na ~ Sim, paired = TRUE)
data.mating %>% cohens_d(na ~ Sim, paired = TRUE)

library(psych)
alpha(data[11:12], keys=NULL,cumulative=FALSE, title=NULL, max=10,na.rm = TRUE,
      check.keys=FALSE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,
      n.obs=NULL,impute=NULL
)
data$liking<-rowMeans(data[5:8])
data$pa<-rowMeans(data[9:10])
data$na<-rowMeans(data[11:12])
data$emotion<-data$pa-data$na