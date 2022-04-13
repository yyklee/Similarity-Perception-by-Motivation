library(tidyverse)
library(rstatix)
library(ggpubr)


data<-read.csv(file.choose())
#Data preparation

View(data)
names(data)[1]<-"Gender"
#Gender: 0 = Male, 1 =Female 
#Condition: 0 = Friend, 1 = Mating
#Sim 1 = Sim, 0 =non Sim 

#FACTORS / IV
data$Gender<-factor(data$Gender)
data$Condition<-factor(data$Condition)
data$Similarity<-factor(data$Similarity)
data$id<-factor(data$id)

#DEPENDENT VARIABLES
#liking, pa, na, emotions
data$liking<-rowMeans(data[4:7])
data$pa<-rowMeans(data[8:9])
data$na<-rowMeans(data[10:11])
data$emotion<-data$pa-data$na

#MODERATORS
#appearance, mate-value, appearance
data$vig<-rowMeans(data[16:27])
data$mv<-rowMeans(data[31:36])
data$app<-rowMeans(data[28:30])

#DEMOGRAPHIC
mean(data$age)
sd(data$age)

##########Summary Stats##############

#SIM=1 SIMILAR / GOAL =1 MATING
data %>%
  group_by(Similarity, Condition) %>%
  get_summary_stats(liking, type = "full")


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



data.aff <- data[which(data$Condition==0),]
data.mating <- data[which(data$Condition==1),]

library(ez)
three<-ezANOVA(data=data, dv=.(liking), wid=.(id), within=.(Similarity),
                       between=.(Gender,Condition), type=3, detailed=T)

three


two<-ezANOVA(data=data, dv=.(liking), wid=.(id), within=.(Similarity),
           between=.(Condition), type=3, detailed=T)
two


t.test(data.aff$liking~data.aff$Similarity, paired =T)
t.test(data.mating$liking~data.mating$Similarity, paired =T)

options(scipen =999)

