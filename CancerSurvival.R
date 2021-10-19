
library(tidyverse)
library(Stat2Data)
library(agricolae)

 #data 
 data(CancerSurvival) 
 
 #calculating mean &SD 
 CancerSurvival %>% 
   group_by(Organ)%>% 
   summarise(mean(Survival),
             sd(Survival))
 
 #plotting
 ggplot(CancerSurvival) +
   geom_boxplot(aes(Organ,Survival))
 
 
 #anova test: is there signficatnt differnce?  
 a1 <- aov(Survival~Organ,data=CancerSurvival)
summary.aov(a1)

# signd 

#No adjected P-value
with(CancerSurvival,pairwise.t.test(Survival,Organ, 
                             p.adjust.method = "none"))

#adjcted P-value with bonferroni
with(CancerSurvival,pairwise.t.test(Survival,Organ, 
                              p.adjust.method = "bonferroni"))

#adjcted P-value with Benjamini-Hochberg

with(CancerSurvival,pairwise.t.test(Survival,Organ, 
                              p.adjust.method = "BH"))

#Other methods
#Tukey's  honestly significant difference(HSD)
TukeyHSD(a1)

#fisher's least significant difference(LSD)

model.tables(a1)
print(LSD.test(a1,"Organ"))

?CancerSurvival
