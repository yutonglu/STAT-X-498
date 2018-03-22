library(readxl)
library(jtools)
library(ggplot2)
library(dplyr)
library(broom)
library(tidyr)
#We are using Ordered Categorical Response
#Therefore, we are going to look at Proportional Odds Linear Regression
f <- file.choose()
#Assign Wolvarine Data
dataset <- read_excel(f,skip=1)
#Columns 2 and 3 are our only predictors
EBPAS <- dataset[,c(1,2,3,which(substr(names(dataset),1,2)=="EB"))]
ATSA <- dataset[,c(1,2,3,which(substr(names(dataset),1,2)=="SA"))]
Culture <- dataset[,c(1,2,3,which(substr(names(dataset),1,2)=="OC"))]
Functioning <- dataset[,c(1,2,3,which(substr(names(dataset),1,2)=="OF"))]
#EBPAS Cleaning and EDA
EBPAS <- gather(EBPAS,key="Question",value="Likert",4:18)
Subscale <- c()
for(x in EBPAS$Question){
  q <- substr(x,4,5)
  if(q%in%c(11,12,13)){
    Subscale <- c(Subscale,"Requirement")
  } else if(q%in%c(9,10,14,15)){
    Subscale <- c(Subscale,"Appeal")
  } else if(q%in%c(1,2,4,8)){
    Subscale <- c(Subscale,"Openness")
  } else if(q%in%c(3,5,6,7)){
    Subscale <- c(Subscale,"Divergence")
  }
}
EBPAS$Subscale <- Subscale
ggplot(data=EBPAS,aes(x=Subscale,y=Likert,fill=Subscale))+geom_boxplot()+
  labs(title="Boxplot of Subscale Scores",y="Score")+guides(fill=FALSE)
ggplot(data=EBPAS,aes(x=Subscale,y=Likert,fill=as.factor(Time)))+geom_boxplot()+
  labs(title="Boxplot of Subscale Scores In Each Time",y="Score",fill="Time")
ggplot(data=EBPAS,aes(x=Subscale,y=Likert,fill=as.factor(DQ_1Site)))+
  geom_boxplot()+labs(title="Boxplot of Subscale Scores in Each Site",fill="Site",y="Score")

#Interaction Plot
ggplot(data=EBPAS,aes(x=Time,y=Likert,col=as.factor(DQ_1Site)))+
  geom_smooth(method="lm",se=FALSE)+facet_wrap(~Subscale)+
  labs(col="Site",title="Interaction Plot Between Time and Site", y="Score")
###Conclusions: There are some outliers, we should include an Interaction Term
###In these Subscales between Time and Site


