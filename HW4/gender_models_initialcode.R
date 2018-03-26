library(foreign)
library(ggplot2)
f <- file.choose()
#Choose GLRB_Diss_Gender_Tall_Data.sav
genderdata <- read.spss(f,to.data.frame=TRUE)
#Questions:

#1. Will transphobia affect
#responses to advertisements when
#participants read news stories
#about models' transgender
#identities?

#2. Will there be differences in
#attention, arousal, and negative
#affect to advertisements featuring
#hard to categorize models compared
#to advertisements featuring easy
#to categorize models?

#3.Will transphobia affect
#responses to advertisements
#featuring hard to categorize
#models?

#4. Will there be differences in 
#attention, arousal, and negative 
#affect to advertisements 
#featuring transgender models 
#compared to advertisements 
#featuring cisgender models?

#5. Will transphobia affect
#responses to advertisements
#featuring transgender models?

###This implies the important
###variables:
###Transphobia: IV- Continuous
###Attention (Heart Rate): DV- Continuous
###Arousal (Sweat): DV- Continuous
###Negative Effect (Facial EMG): DV- Continuous
###Transgender or Cis: IV- Categorical
###Androgynous or Not: IV- Categorical
###Identity Story or Not: IV- Categorical

#1.Does a model have a nonzero
#coefficient for transphobia
#for a model only concerned with
#those who read a transgender
#identity story?

#2. Does a model have a nonzero
#coefficient for adrogynous
#or not

#3.Does a model have a nonzero
#coefficient for transphobia for
#a model only concerned with 
#adrogynous models?

#4. Does a model have a nonzero
#coefficient for transgender or
#cis model?

#5. Does a model only concerned
#with transgender models have
#a nonzero coefficient for
#transphobia?

###Present raw data
interest_data <- genderdata[,c(1,2,4,5,6,7,11,12,13,14,16,18,20,21,25,38)]

###Data Visualization

###Question 1- Only for those who read identity story, transphobia vs response
ggplot(subset(interest_data,News_Identity=="Identity"),
       aes(x=Transphobia_Log,y=EDA_Change))+geom_point()+
  geom_smooth()+
  labs(title="Log Transphobia Against Change in EDA",
       x="Log Transphobia",y="Change in EDA")

ggplot(subset(interest_data,News_Identity=="Identity"),
       aes(x=Transphobia_Log,y=EMG_Change1000))+geom_point()+
  geom_smooth()+
  labs(title="Log Transphobia Against Change in EMG*1000",
       x="Log Transphobia",y="Change in EMG*1000")

ggplot(subset(interest_data,News_Identity=="Identity"),
       aes(x=Transphobia_Log,y=HR_Change))+geom_point()+
  geom_smooth()+
  labs(title="Log Transphobia Against Change in HR",
       x="Log Transphobia",y="Change in HR")
###We fit a gam model and find that all of these changes are around 0 in all
###DV's

#2 and 4 Does a model have a nonzero
#coefficient for adrogynous
#or not
ggplot(interest_data,aes(x=Ease_Categorization,y=Actual_Identity))+
  geom_tile(aes(fill = HR_Change)) + scale_fill_gradient(low = "black", 
                                                        high = "cyan")+
  labs(title="Change in Heart Rate Heatmap",
       x="Ease of Categorization",y="Actual Identity")
###We see that the ones that are transgender and Hard to Identify have the
###lowest change in Heart Rate while the transgender and easy to identify
###models have the highest Heart Rate Change.
###Those of cisgender models seem to have relatively the same change
ggplot(interest_data,aes(x=Ease_Categorization,y=Actual_Identity))+
  geom_tile(aes(fill = EDA_Change)) + scale_fill_gradient(low = "black", 
                                                         high = "cyan")+
  labs(title="Change in EDA Heatmap",
       x="Ease of Categorization",y="Actual Identity")
###We see absolutely no change in regards to these categories
ggplot(interest_data,aes(x=Ease_Categorization,y=Actual_Identity))+
  geom_tile(aes(fill = EMG_Change1000)) + scale_fill_gradient(low = "black", 
                                                          high = "cyan")+
  labs(title="Change in EMG*1000 Heatmap",
       x="Ease of Categorization",y="Actual Identity")
###Again, we see no change between these groups

###Another Method of Visualization
ggplot(interest_data,aes(x=Actual_Identity,y=EMG_Change1000))+
  facet_wrap(~Ease_Categorization)+geom_boxplot()+
  labs(title="Boxplot of Change in EMG*1000",
       y="Change in EMG*1000",x="Actual Identity")
ggplot(interest_data,aes(x=Actual_Identity,y=EDA_Change))+
  facet_wrap(~Ease_Categorization)+geom_boxplot()+
  labs(title="Boxplot of Change in EDA",
       y="Change in EDA",x="Actual Identity")
ggplot(interest_data,aes(x=Actual_Identity,y=HR_Change))+
  facet_wrap(~Ease_Categorization)+geom_boxplot()+
  labs(title="Boxplot of Change in Heart Rate",
       y="Change in Heart Rate",x="Actual Identity")
###The boxplots show less of a relationship than the heatmap

#3
ggplot(subset(interest_data,Ease_Categorization=="Hard (Androgynous)"),
       aes(x=Transphobia_Log,y=EDA_Change))+geom_point()+
  geom_smooth()+
  labs(title="Log Transphobia Against Change in EDA",
       x="Log Transphobia",y="Change in EDA")

ggplot(subset(interest_data,Ease_Categorization=="Hard (Androgynous)"),
       aes(x=Transphobia_Log,y=EMG_Change1000))+geom_point()+
  geom_smooth()+
  labs(title="Log Transphobia Against Change in EMG*1000",
       x="Log Transphobia",y="Change in EMG*1000")

ggplot(subset(interest_data,Ease_Categorization=="Hard (Androgynous)"),
       aes(x=Transphobia_Log,y=HR_Change))+geom_point()+
  geom_smooth()+
  labs(title="Log Transphobia Against Change in HR",
       x="Log Transphobia",y="Change in HR")

###Each of these variables seem to have a gam line close to 0.

#5
ggplot(subset(interest_data,Actual_Identity=="Transgender"),
       aes(x=Transphobia_Log,y=EDA_Change))+geom_point()+
  geom_smooth()+
  labs(title="Log Transphobia Against Change in EDA",
       x="Log Transphobia",y="Change in EDA")

ggplot(subset(interest_data,Actual_Identity=="Transgender"),
       aes(x=Transphobia_Log,y=EMG_Change1000))+geom_point()+
  geom_smooth()+
  labs(title="Log Transphobia Against Change in EMG*1000",
       x="Log Transphobia",y="Change in EMG*1000")

ggplot(subset(interest_data,Actual_Identity=="Transgender"),
       aes(x=Transphobia_Log,y=HR_Change))+geom_point()+
  geom_smooth()+
  labs(title="Log Transphobia Against Change in HR",
       x="Log Transphobia",y="Change in HR")
###Much like the previous graphs, these have a GAM line surrounding 0.