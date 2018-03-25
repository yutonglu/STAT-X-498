library(readxl)
library(jtools)
library(ggplot2)
library(dplyr)
library(broom)
library(tidyr)
library(stringr)
#We are using Ordered Categorical Response
#Therefore, we are going to look at Proportional Odds Linear Regression
f <- file.choose()
#Assign Wolvarine Data
dataset <- read_excel(f,skip=1)
#Columns 2 and 3 are our only predictors
dataset <- dataset[which(dataset$DQ_1Site!=3),]
###Using Averages Data
averages_data <- dataset[,c(1:3,which(str_sub(names(dataset),start=-2)=="le"),
                            which(substr(names(dataset),16,18)=="Fun"))]
#Boxplots
ebpas_req <- ggplot(data=averages_data,aes(y=`EBPAS: Requirement Subscale`))
ebpas_appeal <- ggplot(data=averages_data,aes(y=`EBPAS: Appeal Subscale`))
ebpas_openness <- ggplot(data=averages_data,aes(y=`EBPAS: Openness Subscale`))
ebpas_divergence <- ggplot(data=averages_data,aes(y=`EBPAS: Divergence Subscale`))

atsa_judgement <- ggplot(data=averages_data,aes(y=`ATSA: Benefits over Clinical Judgement Scale`))
atsa_quality <- ggplot(data=averages_data,aes(y=`ATSA: Psychometric Quality Subscale`))
atsa_practical <- ggplot(data=averages_data,aes(y=`ATSA: Practiciality Subscale`))

cult_team <- ggplot(data=averages_data,aes(y=`Organizational Clulture: Teamwork Subscale`))
cult_morale <- ggplot(data=averages_data,aes(y=`Organizational Culture: Climate/Morale Subscale`))
cult_info <- ggplot(data=averages_data,aes(y=`Organizational Culture Information Subscale`))
cult_res <- ggplot(data=averages_data,aes(y=`Organizational Culture: Supervision Subscale`))
cult_meet <- ggplot(data=averages_data,aes(y=`Organzational Culture: Meeting Subscale` ))

fun_change <- ggplot(data=averages_data,aes(y=`Organizational Functioning: Motivation for Change`))
fun_res <- ggplot(data=averages_data,aes(y=`Organizational Functioning: Resources`))
fun_staff <- ggplot(data=averages_data,aes(y=`Organizational Functioning: Staff Attributes`))
fun_org <- ggplot(data=averages_data,aes(y=`Organizational Functioning: Organizational Climate`))
fun_job <- ggplot(data=averages_data,aes(y=`Organizational Functioning: Job Attitudes`))
fun_work <- ggplot(data=averages_data,aes(y=`Organizational Functioning: Workplace Practices`))
fun_satis <- ggplot(data=averages_data,aes(y=`Organizational Functioning: Training Satisfaction`))
fun_exp <- ggplot(data=averages_data,aes(y=`Organizational Functioning: Training Exposure and Utilization`))

#Heatmap
map_heat <- ggplot(data=averages_data,aes(x=as.factor(Time),y=as.factor(DQ_1Site)))

ebpas_req+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of EBPAS: Requirement Subscale")
ggplot(data=averages_data,aes(x=`EBPAS: Requirement Subscale`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of EBPAS: Requirement Subscale")
map_heat+geom_tile(aes(fill=`EBPAS: Requirement Subscale`))+scale_fill_gradient(low="black",high="cyan")
ebpas_req+geom_smooth(aes(x=Time,col=as.factor(DQ_1Site)),se=FALSE)
summary(averages_data$`EBPAS: Requirement Subscale`)

ebpas_openness+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of EBPAS: Openness Subscale")
ggplot(data=averages_data,aes(x=`EBPAS: Openness Subscale`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of EBPAS: Openness Subscale")
map_heat+geom_tile(aes(fill=`EBPAS: Openness Subscale`))+scale_fill_gradient(low="black",high="cyan")
ebpas_openness+geom_smooth(aes(x=Time,col=as.factor(DQ_1Site)),se=FALSE)
summary(averages_data$`EBPAS: Openness Subscale`)

ebpas_divergence+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of EBPAS: Divergence Subscale")
ggplot(data=averages_data,aes(x=`EBPAS: Divergence Subscale`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of EBPAS: Divergence Subscale")
map_heat+geom_tile(aes(fill=`EBPAS: Divergence Subscale`))+scale_fill_gradient(low="black",high="cyan")
ebpas_divergence+geom_smooth(aes(x=Time,col=as.factor(DQ_1Site)),se=FALSE)
summary(averages_data$`EBPAS: Divergence Subscale`)

ebpas_appeal+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of EBPAS: Appeal Subscale")
ggplot(data=averages_data,aes(x=`EBPAS: Appeal Subscale`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of EBPAS: Appeal Subscale")
map_heat+geom_tile(aes(fill=`EBPAS: Appeal Subscale`))+scale_fill_gradient(low="black",high="cyan")
ebpas_appeal+geom_smooth(aes(x=Time,col=as.factor(DQ_1Site)),se=FALSE)
summary(averages_data$`EBPAS: Appeal Subscale`)

atsa_judgement+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of ATSA: Benefits over Clinical Judgement Scale")
ggplot(data=averages_data,aes(x=`ATSA: Benefits over Clinical Judgement Scale`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of ATSA: Benefits over Clinical Judgement Scale")
summary(averages_data$`ATSA: Benefits over Clinical Judgement Scale`)

atsa_quality+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of ATSA: Psychometric Quality Scale")
ggplot(data=averages_data,aes(x=`ATSA: Psychometric Quality Subscale`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of ATSA: Psychometric Quality Scale")
summary(averages_data$`ATSA: Psychometric Quality Subscale`)

atsa_practical+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of ATSA: Practicality Subscale")
ggplot(data=averages_data,aes(x=`ATSA: Practiciality Subscale`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of ATSA: Practicality Subscale")
summary(averages_data$`ATSA: Practiciality Subscale`)

cult_team+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of Org Culture: Teamwork Subscale")
ggplot(data=averages_data,aes(x=`Organizational Clulture: Teamwork Subscale`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of Org Culture: Teamwork Subscale")
summary(averages_data$`Organizational Clulture: Teamwork Subscale`)

cult_morale+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of Org Culture: Climate/Morale Subscale")
ggplot(data=averages_data,aes(x=`Organizational Culture: Climate/Morale Subscale`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of Org Culture: Climate/Morale Subscale")
summary(averages_data$`Organizational Culture: Climate/Morale Subscale`)

cult_info+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of Org Culture: Information Subscale")
ggplot(data=averages_data,aes(x=`Organizational Culture Information Subscale`))+geom_histogram(fill="red",binwidth=0.5)
summary(averages_data$`Organizational Culture Information Subscale`)

cult_res+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of Org Culture: Supervision Subscale")
ggplot(data=averages_data,aes(x=`Organizational Culture: Supervision Subscale`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of Org Culture: Supervision Subscale")
summary(averages_data$`Organizational Culture: Supervision Subscale`)

cult_meet+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of Org Culture: Meeting Subscale")
ggplot(data=averages_data,aes(x=`Organzational Culture: Meeting Subscale`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of Org Culture: Meeting Subscale")
summary(averages_data$`Organzational Culture: Meeting Subscale`)

fun_change+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of Org Function: Motivation Subscale")
ggplot(data=averages_data,aes(x=`Organizational Functioning: Motivation for Change`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of Org Function: Motivation Subscale")
summary(averages_data$`Organizational Functioning: Motivation for Change`)

fun_res+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of Org Function: Resources Subscale")
ggplot(data=averages_data,aes(x=`Organizational Functioning: Resources`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of Org Function: Resources Subscale")
summary(averages_data$`Organizational Functioning: Resources`)

fun_staff+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of Org Function: Staff Subscale")
ggplot(data=averages_data,aes(x=`Organizational Functioning: Staff Attributes`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of Org Function: Staff Subscale")
summary(averages_data$`Organizational Functioning: Staff Attributes`)

fun_org+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of Org Function: Climate Subscale")
ggplot(data=averages_data,aes(x=`Organizational Functioning: Organizational Climate`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of Org Function: Climate Subscale")
summary(averages_data$`Organizational Functioning: Organizational Climate`)

fun_job+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of Org Function: Job Subscale")
ggplot(data=averages_data,aes(x=`Organizational Functioning: Job Attitudes`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of Org Function: Job Subscale")
summary(averages_data$`Organizational Functioning: Job Attitudes`)

fun_work+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of Org Function: Practices Subscale")
ggplot(data=averages_data,aes(x=`Organizational Functioning: Workplace Practices`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of Org Function: Practices Subscale")
summary(averages_data$`Organizational Functioning: Workplace Practices`)

fun_satis+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of Org Function: Satisfaction Subscale")
ggplot(data=averages_data,aes(x=`Organizational Functioning: Training Satisfaction`))+geom_histogram(fill="red",binwidth=1)+labs(title="Histogram of Org Function: Satisfaction Subscale")
summary(averages_data$`Organizational Functioning: Training Satisfaction`)

fun_exp+geom_boxplot(aes(x=as.factor(DQ_1Site),fill=as.factor(Time)))+labs(x="Site",fill="Time",title="Boxplot of Org Function: Training Subscale")
ggplot(data=averages_data,aes(x=`Organizational Functioning: Training Exposure and Utilization`))+geom_histogram(fill="red",binwidth=0.5)+labs(title="Histogram of Org Function: Training Subscale")
summary(averages_data$`Organizational Functioning: Training Exposure and Utilization`)
