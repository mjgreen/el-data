library(readxl)
library(tidyverse)
library(report)
dd=read_excel("el_data.xlsx") %>% 
  mutate(pp=seq_along(Age)) %>% 
  rename(riskiness=`C Total`) %>% 
  rename(urgency = `UrgTot.`) %>% 
  rename(premed=`PremTot.`) %>% 
  rename(persev=`PersTot.`) %>% 
  rename(sensation_seeking=`SenTot.`) %>% 
  relocate(pp, riskiness, urgency, premed, persev, sensation_seeking) %>% 
  mutate(pp=as_factor(pp))
  
a=dd %>% select(1:6) %>% 
  pivot_longer(cols=3:6, names_to="predictor", values_to="score") %>% 
  mutate(predictor=as_factor(predictor))


ggplot(a, aes(y=riskiness, x=score))+
  facet_grid(~predictor)+
  #stat_summary(geom='line')+
  #stat_summary(fun='mean')+
  #geom_smooth(method='lm', color='red')+
  geom_smooth()+
  stat_summary()

ggplot(a %>% filter(predictor == "urgency"), 
       aes(y=riskiness, x=score))+
  #facet_wrap(~pp)+
  stat_summary()
  #geom_smooth(method='lm')
  
model1 <- lm(data=dd, riskiness ~ urgency)
model2 <- lm(data=dd, riskiness ~ urgency + premed+persev+sensation_seeking)


report(model1)
