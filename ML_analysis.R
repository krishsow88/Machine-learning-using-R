
#Analysis of machine learning tools

library('tidyverse') 
library('leaflet')
library('ggmap')
library('GGally')
library('viridis')
library('plotly')
library('IRdisplay')
library('ggrepel')
library('cowplot')

str(multichoice)
head(multichoice)

theme1 <- theme_bw()+theme(text = element_text(size=20),
                           legend.position = 'none', plot.title = element_text(hjust = 0.5))

options(repr.plot.width=12, repr.plot.height=6)

multichoice[-1,] %>% group_by(Q1)%>%summarise(Count = length(Q1))%>%
  mutate(pct = prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Q1, -pct), y = pct, fill = Q1)) + 
  geom_bar(stat = 'identity') + scale_fill_brewer(palette="Set1",  na.value = "gray")+
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.5,
            vjust = -0.5, size =5)+ theme1+  xlab("") + ylab("Percent")+
  ggtitle("Respondents Gender")

options(repr.plot.width=12, repr.plot.height=8)

multichoice$Q2 <- factor(multichoice$Q2, 
                         level = c("18-21","22-24","25-29",
                                   "30-34","35-39","40-44",
                                   "45-49","50-54","55-59",
                                   "60-69","70-79","80+"))

multichoice %>% 
  group_by(Q1,Q2)%>%
  filter(Q1 == "Female"| Q1== "Male")%>%
  summarise(Count = length(Q2))%>%
  mutate(pct = prop.table(Count)*100)%>%
  ggplot(aes(x = Q2, y = pct, fill = pct)) + 
  geom_bar(stat = 'identity') + scale_fill_viridis(direction = -1)+
  facet_grid(Q1~.)+
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.5,
            vjust = -0.1, size =4)+ theme1+  xlab("") + ylab("Percent")+
  ggtitle("Age Comparison")

options(repr.plot.width=12, repr.plot.height=10)

#Replacing the long titles, for making the plots convinient
multichoice$Q3 <- 
  str_replace(multichoice$Q3, "United Kingdom of Great Britain and Northern Ireland","UK & NI" )

multichoice$Q3 <- 
  str_replace(multichoice$Q3, "I do not wish to disclose my location","Won't disclose" )

multichoice$Q3 <- 
  str_replace(multichoice$Q3, "Iran, Islamic Republic of...","Iran" )

multichoice$Q3 <- 
  str_replace(multichoice$Q3, "United States of America","USA" )

#Setting the theme for the plots
theme2 <- theme_bw()+theme(text = element_text(size=15),
                           axis.text.x = element_text(angle = 0, hjust = 1))+
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))

multichoice[-1,] %>% 
  group_by(Q1,Q3)%>%
  filter(Q1 == "Female"| Q1== "Male")%>%
  summarise(Count = length(Q3))%>%
  mutate(pct = prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Q3,pct), y = pct, fill = pct)) + 
  geom_bar(stat = 'identity') + scale_fill_viridis(direction= -1)+
  facet_wrap(Q1~.)+
  theme2+  xlab("") + ylab("Percent")+coord_flip()+
  ggtitle("Country")

theme2.1 <- theme_bw()+theme(text = element_text(size=15),
                             axis.text.x = element_text(angle = 30, hjust = 1))+
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))


multichoice[] %>% 
  group_by(Q1,Q2,Q3)%>%
  filter(Q1 == "Female"| Q1== "Male")%>%
  summarise(Count = length(Q2))%>%
  mutate(pct = prop.table(Count)*100)%>%
  ggplot(aes(x = Q2, y = Q3, fill = pct)) + 
  geom_tile() + scale_fill_viridis(direction= -1)+
  facet_wrap(Q1~.)+
  theme2.1+  xlab("") + ylab(" ")+
  ggtitle("Country-Age-Gender Comparison")

multichoice %>% 
  group_by(Q1,Q4)%>%
  filter(Q1 == "Female"|Q1=="Male")%>%
  filter(!is.na(Q4))%>%
  summarise(Count = length(Q4))%>%
  mutate(pct = prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Q4, -pct), y = pct, fill = Q4)) + 
  geom_bar(stat = 'identity') + scale_fill_brewer(palette="Set1",  na.value = "gray")+
  facet_grid(Q1~.)+
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.5,vjust = -0.5, size =4)+   
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+theme2+ 
  xlab("") + ylab("Percent")+
  ggtitle("Education")

options(repr.plot.width=12, repr.plot.height=12)

multichoice[] %>% 
  group_by(Q1,Q3,Q4)%>%
  filter(Q1 == "Female"| Q1== "Male")%>%
  filter(!is.na(Q4))%>%
  summarise(Count = length(Q3))%>%
  mutate(pct = prop.table(Count)*100)%>%
  ggplot(aes(x = Q4, y = Q3, fill = pct)) + 
  geom_tile() + scale_fill_viridis(direction= -1)+
  facet_wrap(Q1~.)+
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 20))+             
  theme2.1+  xlab("") + ylab(" ")+
  ggtitle("Country-Education-Gender Comparison")

theme3 <- theme_bw()+theme(text = element_text(size=13),
                           axis.text.y = element_text(angle = 0, hjust = 1))+
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))

multichoice %>% 
  group_by(Q1,Q5)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  filter(!is.na(Q5))%>%
  summarise(Count = length(Q5))%>%
  mutate(pct = prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Q5, -pct), y = pct, fill = pct)) + 
  geom_bar(stat = 'identity') + scale_fill_viridis(direction = -1)+
  facet_grid(Q1~.)+
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.5,vjust = -0.5, size =4)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme3+xlab("") + ylab("Percent")+
  ggtitle("Undergraduate Major")

multichoice %>% 
  group_by(Q1, Q6)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  filter(!is.na(Q6))%>%
  summarise(Count = length(Q6))%>%
  mutate(pct = prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Q6, pct), y = pct, fill = pct)) + 
  geom_bar(stat = 'identity') + scale_fill_viridis(direction= -1)+
  facet_wrap(Q1~.)+coord_flip()+
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = -0.1,vjust = -0.5, size =3, angle = 0)+   
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme3+ xlab("") + ylab("Percent")+
  ggtitle("Current Role")

theme4 <- theme_bw()+theme(text = element_text(size=13),
                           axis.text.x = element_text(angle = 30, hjust = 1))+
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))

multichoice %>% 
  group_by(Q1,Q7)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  filter(!is.na(Q7))%>%
  summarise(Count = length(Q7))%>%
  mutate(pct = prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Q7, -pct), y = pct, fill = pct)) + 
  geom_bar(stat = 'identity') + scale_fill_viridis(direction= -1)+
  facet_grid(Q1~.)+
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.5,vjust = -0.5, size =4)+   
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme4+xlab("") + ylab("Percent")+
  ggtitle("Current Industry")

multichoice$Q8 <- factor(multichoice$Q8, level = c("0-1","1-2","2-3",
                                                   "3-4","4-5","5-10",
                                                   "10-15","15-20","20-25",
                                                   "25-30","30+"))

multichoice %>% 
  group_by(Q1,Q8)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  filter(!is.na(Q8))%>%
  summarise(Count = length(Q8))%>%
  mutate(pct = prop.table(Count)*100)%>%
  ggplot(aes(x = Q8, y = pct, fill = Q8)) + 
  geom_bar(stat = 'identity') + scale_fill_brewer(palette="Set3",  na.value = "gray")+
  facet_grid(Q1~.)+
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.5,vjust = -0.5, size =5)+   
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme1+xlab("") + ylab("Percent")+
  ggtitle("Experience in Current Role")


options(repr.plot.width=12, repr.plot.height=8)

theme2.2 <- theme_bw()+theme(text = element_text(size=15),
                             axis.text.x = element_text(angle = 30, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))


multichoice$Q9 <- 
  str_replace(multichoice$Q9, 
              "I do not wish to disclose my approximate yearly compensation",
              "Won't disclose" )

multichoice$Q9 <- factor(multichoice$Q9, 
                         level = c("Won't disclose",
                                   "0-10,000","10-20,000","20-30,000","30-40,000",
                                   "40-50,000","50-60,000","60-70,000","70-80,000",
                                   "80-90,000","90-100,000","100-125,000",
                                   "125-150,000","150-200,000","200-250,000",
                                   "250-300,000","300-400,000", "400-500,000","500,000+"))


multichoice %>% 
  group_by(Q1,Q9)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  filter(!is.na(Q9))%>%
  summarise(Count = length(Q9))%>%
  mutate(pct = prop.table(Count)*100)%>%
  ggplot(aes(x = Q9, y = pct, fill = Q1)) + 
  geom_col( position = "dodge") + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+
  theme2.2+ xlab("Salary Ranges") + ylab("Percent")+
  ggtitle("Yearly Salary")

options(repr.plot.width=12, repr.plot.height=12)

theme2.3 <- theme_bw()+theme(text = element_text(size=10),
                             axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position = "top",plot.title = element_text(hjust = 0.5))


multichoice[] %>% 
  group_by(Q1,Q3,Q9)%>%
  filter(Q1 == "Female"| Q1== "Male")%>%
  filter(!is.na(Q3)) %>% filter(!is.na(Q9))%>%
  filter(Q9 != "Won't disclose") %>%
  filter(Q3 != "Won't disclose" & Q3 != "Other")%>%
  summarise(Count = length(Q1))%>%
  mutate(pct = prop.table(Count)*100)%>%
  ggplot(aes(x = Q9, y= pct, fill = Q1)) + 
  geom_col(position = "dodge") + #scale_fill_viridis(direction= -1)+
  facet_wrap(Q3~.)+
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 20))+             
  theme2.3+  xlab("") + ylab(" ")+#coord_flip()+
  ggtitle("Country-Education-Gender Comparison")

options(repr.plot.width=12, repr.plot.height=10)

multichoice %>% 
  group_by(Q1,Q10)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  filter(!is.na(Q10))%>%
  summarise(Count = length(Q10))%>%
  mutate(pct = prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Q10, -pct), y = pct, fill = pct)) + 
  geom_bar(stat = 'identity') + scale_fill_viridis(direction= -1)+
  facet_grid(Q1~.)+
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.01,vjust = 0.05, size =5)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme1+ xlab("") + ylab("Percent")+
  ggtitle("Employer uses ML or not")


multichoice %>% 
  select(Q1,Q11_Part_1,Q11_Part_2, Q11_Part_3,Q11_Part_4,Q11_Part_5,Q11_Part_6,Q11_Part_7)%>%
  filter(Q1 == "Female"|Q1=="Male")%>%
  gather(2:8, key = "questions", value = "Function")%>%
  group_by(Q1,Function)%>%
  filter(!is.na(Function))%>%
  summarise(Count = length(Function))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Function, -percent), y = percent, fill = Function)) + 
  geom_bar(stat = 'identity') + scale_fill_brewer(palette="Set1",  na.value = "gray")+
  facet_grid(Q1~.)+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.01, size =5)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme1+ xlab("") + ylab("Percent")+
  ggtitle("Day to Day function?")

multichoice %>% 
  group_by(Q1, Q12_MULTIPLE_CHOICE)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  filter(!is.na(Q12_MULTIPLE_CHOICE))%>%
  summarise(Count = length(Q12_MULTIPLE_CHOICE))%>%
  mutate(pct = prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Q12_MULTIPLE_CHOICE, -pct), y = pct, fill = Q12_MULTIPLE_CHOICE)) + 
  geom_bar(stat = 'identity') + scale_fill_brewer(palette="Set1")+
  facet_grid(Q1~.)+
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.01,vjust = 0.01, size =5)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme1+ xlab("") + ylab("Percent")+
  ggtitle("Choice of Tools for Analysis")

multichoice %>% 
  select(Q1,30:45)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  gather(2:16, key = "questions", value = "Function")%>%
  group_by(Q1,Function)%>%
  filter(!is.na(Function))%>%
  summarise(Count = length(Function))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Function, -percent), y = percent, fill = percent)) + 
  geom_bar(stat = 'identity') + scale_fill_viridis(direction = -1)+
  facet_grid(Q1~.)+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.01, size =5)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme2+ xlab("") + ylab("Percent")+
  ggtitle("IDE's used at school or work")

multichoice %>% 
  select(Q1,46:57)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  gather(2:11, key = "questions", value = "Function")%>%
  group_by(Q1,Function)%>%
  filter(!is.na(Function))%>%
  summarise(Count = length(Function))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Function, -percent), y = percent, fill = percent)) + 
  geom_bar(stat = 'identity') + scale_fill_viridis(direction = -1)+
  facet_grid(Q1~.)+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.01, size =5)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme2+ xlab("") + ylab("Percent")+
  ggtitle("Hosted Notebooks used at school or work")

multichoice %>% 
  select(Q1,59:65)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  gather(2:7, key = "questions", value = "Function")%>%
  group_by(Q1,Function)%>%
  filter(!is.na(Function))%>%
  summarise(Count = length(Function))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Function, -percent), y = percent, fill = percent)) + 
  geom_bar(stat = 'identity') + scale_fill_viridis(direction = -1)+
  facet_grid(Q1~.)+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.01, size =5)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme2+ xlab("") + ylab("Percent")+
  ggtitle("Cloud services used in the last 5 years")
options(repr.plot.width=10, repr.plot.height=6)


theme5 <- theme_bw()+theme(text = element_text(size=15),
                           axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = 'top', plot.title = element_text(hjust = 0.5))

multichoice %>% 
  select(Q1,66:84)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  gather(2:18, key = "questions", value = "Programming_Language")%>%
  group_by(Q1,Programming_Language)%>%
  filter(!is.na(Programming_Language))%>%
  filter(!is.na(Q1))%>%
  summarise(Count = length(Programming_Language))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Programming_Language,-percent), y = percent, fill = Q1)) + 
  geom_col( position = "dodge") + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+
  # geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.1, size =3)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme5+ xlab("") + ylab("Percent")+
  ggtitle("Programming_Language used on a Regular Basis")

multichoice %>% 
  select(Q1,Q17)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  #gather(2:18, key = "questions", value = "Programming_Language")%>%
  group_by(Q1,Q17)%>%
  filter(!is.na(Q17))%>%
  #filter(!is.na(Q1))%>%
  summarise(Count = length(Q17))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Q17,-percent), y = percent, fill = Q1)) + 
  geom_col( position = "dodge") + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+
  #geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.1, size =3)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme5+ xlab("") + ylab("Percent")+
  ggtitle("Specific Programming language used most often")

multichoice %>% 
  select(Q1,Q18)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  #gather(2:18, key = "questions", value = "Programming_Language")%>%
  group_by(Q1,Q18)%>%
  filter(!is.na(Q18))%>%
  #filter(!is.na(Q1))%>%
  summarise(Count = length(Q18))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Q18,-percent), y = percent, fill = Q1)) + 
  geom_col( position = "dodge") + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+
  #geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.1, size =3)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme5+ xlab("") + ylab("Percent")+
  ggtitle("Recommended Programming language")

multichoice %>% 
  filter(Q1 == "Female"|Q1 == "Male")%>%
  filter(Q18 == "Python"|Q18 == "R")%>%
  group_by(Q1,Q6,Q18)%>%
  filter(!is.na(Q18))%>%
  summarise(Count = length(Q18))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x =Q18 , y = Q6, fill = percent)) + 
  geom_tile() + scale_fill_viridis(direction= -1)+
  facet_wrap(Q1~.)+
  theme2.1+  xlab("") + ylab(" ")+
  ggtitle("Profession-Programming Language-Gender Comparison")

multichoice %>% 
  filter(Q1 == "Female"|Q1 == "Male")%>%
  filter(Q18 == "Python"|Q18 == "R")%>%
  group_by(Q1,Q5,Q18)%>%
  filter(!is.na(Q18))%>%
  filter(!is.na(Q5))%>%
  summarise(Count = length(Q18))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x =Q18 , y = Q5, fill = percent)) + 
  geom_tile() + scale_fill_viridis(direction= -1)+
  facet_wrap(Q1~.)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 40))+
  theme2.1+  xlab("") + ylab(" ")+
  ggtitle("Industry-Programming Language-Gender Comparison")

multichoice %>% 
  select(Q1,Q19_Part_1:Q19_Part_19)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  gather(2:19, key = "questions", value = "ML")%>%
  group_by(Q1,ML)%>%
  filter(!is.na(ML))%>%
  filter(!is.na(Q1))%>%
  summarise(Count = length(ML))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(ML,-percent), y = percent, fill = Q1)) + 
  geom_col( position = "dodge") + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+
  #geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.1, size =3)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme5+ xlab("") + ylab("Percent")+
  ggtitle("ML framework used in last 5 years")

multichoice %>% 
  select(Q1,Q20)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  group_by(Q1,Q20)%>%
  filter(!is.na(Q20))%>%
  #filter(!is.na(Q1))%>%
  summarise(Count = length(Q20))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Q20,-percent), y = percent, fill = Q1)) + 
  geom_col( position = "dodge") + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+
  #geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.1, size =3)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme5+ xlab("") + ylab("Percent")+
  ggtitle("ML framework used most often")

multichoice %>% 
  select(Q1,Q21_Part_1:Q21_Part_13)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  gather(2:13, key = "questions", value = "viz")%>%
  group_by(Q1,viz)%>%
  filter(!is.na(viz))%>%
  filter(!is.na(Q1))%>%
  filter(viz != "None")%>%
  summarise(Count = length(viz))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(viz,-percent), y = percent, fill = Q1)) + 
  geom_col( position = "dodge") + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+
  #geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.1, size =3)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme5+ xlab("") + ylab("Percent")+
  ggtitle("Visualization Libraries used in last 5 years")

multichoice %>% 
  select(Q1,Q22)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  group_by(Q1,Q22)%>%
  filter(!is.na(Q22))%>%
  summarise(Count = length(Q22))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Q22,-percent), y = percent, fill = Q1)) + 
  geom_col( position = "dodge") + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme5+ xlab("") + ylab("Percent")+
  ggtitle("Visualization Libraries used most often")

multichoice$Q23 <- factor(multichoice$Q23, level = c("0% of my time",
                                                     "1% to 25% of my time",
                                                     "25% to 49% of my time",
                                                     "50% to 74% of my time",
                                                     "75% to 99% of my time",
                                                     "100% of my time"))


multichoice %>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  group_by(Q1,Q23)%>%
  filter(!is.na(Q23))%>%
  summarise(Count = length(Q23))%>%
  mutate(Percent = prop.table(Count)*100)%>%
  ggplot(aes(Q23, Percent, fill = Q1))+
  geom_col(position ="dodge")+ 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme5+ xlab("") + ylab("Percent")+
  ggtitle("Time spent actively coding at work or school")

theme6 <- theme_bw()+theme(text = element_text(size=15),
                           axis.text.x = element_text(angle = 0, hjust = 1))+
  theme(legend.position = 'top', plot.title = element_text(hjust = 0.5))


multichoice$Q24 <- factor(multichoice$Q24, 
                          level = c("I have never written code and I do not want to learn",
                                    "I have never written code but I want to learn",
                                    "< 1 year","1-2 years","3-5 years","5-10 years",
                                    "10-20 years","20-30 years","30-40 years", "40+ years"))




multichoice %>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  group_by(Q1,Q24)%>%
  filter(!is.na(Q24))%>%
  summarise(Count = length(Q24))%>%
  mutate(Percent = prop.table(Count)*100)%>%
  ggplot(aes(Q24, Percent, fill = Q1))+
  geom_col(position ="fill")+ 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme6+ xlab("") + ylab("Percent")+
  ggtitle("Experience in coding to analyze data")

options(repr.plot.width=12, repr.plot.height=8)

theme7 <- theme_bw()+theme(text = element_text(size=15),
                           axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))




multichoice %>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  group_by(Q1,Q6, Q24)%>%
  filter(!is.na(Q24))%>%
  filter(!is.na(Q6))%>%
  summarise(Count = length(Q24))%>%
  mutate(Percent = prop.table(Count)*100)%>%
  ggplot(aes(x =Q24 , y = Q6, fill = Percent)) + 
  geom_tile() + scale_fill_viridis(direction= -1)+
  facet_wrap(Q1~.)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))+    
  theme7+  xlab("") + ylab(" ")+
  ggtitle("Experience in coding for analysis-Current Role-Gender Comparison")

multichoice$Q25 <- factor(multichoice$Q25, 
                          level = c("I have never studied machine learning and I do not plan to",
                                    "I have never studied machine learning but plan to learn in the future",
                                    "< 1 year","1-2 years","2-3 years","3-4 years",
                                    "4-5 years","5-10 years","10-15 years"))


multichoice %>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  group_by(Q1,Q25)%>%
  filter(!is.na(Q25))%>%
  summarise(Count = length(Q25))%>%
  mutate(Percent = prop.table(Count)*100)%>%
  ggplot(aes(Q25, Percent, fill = Q1))+
  geom_col(position ="dodge")+ 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme6+ xlab("") + ylab("Percent")+
  ggtitle("ML Method used in School or Work")


multichoice$Q26 <- factor(multichoice$Q26,level = c("Definitely not", "Probably not",
                                                    "Maybe","Probably yes","Definitely yes"))



multichoice %>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  group_by(Q1,Q26)%>%
  filter(!is.na(Q26))%>%
  summarise(Count = length(Q26))%>%
  mutate(Percent = prop.table(Count)*100)%>%
  ggplot(aes(Q26, Percent, fill = Q1))+
  geom_col(position ="dodge")+ 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  theme6+ xlab("") + ylab("Percent")+
  ggtitle("Consider themselves a Data Scientist?")

multichoice %>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  group_by(Q1,Q7, Q26)%>%
  filter(!is.na(Q26))%>%
  filter(!is.na(Q7))%>%
  summarise(Count = length(Q26))%>%
  mutate(Percent = prop.table(Count)*100)%>%
  ggplot(aes(x =Q26 , y = Q7, fill = Percent)) + 
  geom_tile() + scale_fill_viridis(direction= -1)+
  facet_wrap(Q1~.)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))+    
  theme7+  xlab("") + ylab(" ")+
  ggtitle("Current Industry-Consider Data Scientist-Gender Comparison")

multichoice %>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  group_by(Q1,Q5, Q26)%>%
  filter(!is.na(Q26))%>%
  filter(!is.na(Q5))%>%
  summarise(Count = length(Q26))%>%
  mutate(Percent = prop.table(Count)*100)%>%
  ggplot(aes(x =Q26 , y = Q5, fill = Percent)) + 
  geom_tile() + scale_fill_viridis(direction= -1)+
  facet_wrap(Q1~.)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 30))+    
  theme7+  xlab("") + ylab(" ")+
  ggtitle("Undergrad Major-Consider Data Scientist-Gender Comparison")

multichoice %>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  group_by(Q1,Q9, Q26)%>%
  filter(!is.na(Q26))%>%
  filter(!is.na(Q9))%>%
  summarise(Count = length(Q26))%>%
  mutate(Percent = prop.table(Count)*100)%>%
  ggplot(aes(x =Q26 , y = Q9, fill = Percent)) + 
  geom_tile() + scale_fill_viridis(direction= -1)+
  facet_wrap(Q1~.)+
  scale_y_discrete(labels = function(y) str_wrap(y, width = 30))+    
  theme7+  xlab("") + ylab(" ")+
  ggtitle("Yearly Compensation-Consider Data Scientist-Gender Comparison")

multichoice %>% 
  select(Q1,Q27_Part_1:Q27_Part_20)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  gather(2:21, key = "questions", value = "cloud")%>%
  group_by(Q1,cloud)%>%
  filter(!is.na(cloud))%>%
  #filter(!is.na(Q1))%>%
  summarise(Count = length(cloud))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(cloud,-percent), y = percent, fill = Q1)) + 
  geom_col( position = "dodge") + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+
  #geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.1, size =3)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
  theme5+ xlab("") + ylab("Percent")+
  ggtitle("Cloud Computing products used at work or school in the last 5 years ")


options(repr.plot.width=12, repr.plot.height=12)

multichoice %>% 
  select(Q1,Q28_Part_1:Q28_Part_43)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  gather(2:44, key = "questions", value = "ML_Products")%>%
  group_by(Q1,ML_Products)%>%
  filter(!is.na(ML_Products))%>%
  #filter(!is.na(Q1))%>%
  summarise(Count = length(ML_Products))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(ML_Products,percent), y = percent, fill = Q1)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+coord_flip()+
  #geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.1, size =3)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40))+
  theme5+ xlab("") + ylab("Percent")+
  ggtitle("ML_Products used at work or school in the last 5 years ")

multichoice %>% 
  select(Q1,Q29_Part_1:Q29_Part_28)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  gather(2:29, key = "questions", value = "RDB_Products")%>%
  group_by(Q1,RDB_Products)%>%
  filter(!is.na(RDB_Products))%>%
  #filter(!is.na(Q1))%>%
  summarise(Count = length(RDB_Products))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(RDB_Products,percent), y = percent, fill = Q1)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+coord_flip()+
  #geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.1, size =3)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40))+
  theme5+ xlab("") + ylab("Percent")+
  ggtitle("Relational Database Products used at work or school in the last 5 years ")

multichoice %>% 
  select(Q1,Q30_Part_1:Q30_Part_25)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  gather(2:26, key = "questions", value = "BigData_Products")%>%
  group_by(Q1,BigData_Products)%>%
  filter(!is.na(BigData_Products))%>%
  #filter(!is.na(Q1))%>%
  summarise(Count = length(BigData_Products))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(BigData_Products,percent), y = percent, fill = Q1)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+coord_flip()+
  #geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.1, size =3)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40))+
  theme5+ xlab("") + ylab("Percent")+
  ggtitle("Big data and Analytics products used at work or school in the last 5 years ")

multichoice %>% 
  select(Q1,Q31_Part_1:Q31_Part_12)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  gather(2:13, key = "questions", value = "DataType")%>%
  group_by(Q1,DataType)%>%
  filter(!is.na(DataType))%>%
  summarise(Count = length(DataType))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(DataType,-percent), y = percent, fill = Q1)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+#coord_flip()+
  #geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.1, size =3)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40))+
  theme5+ xlab("") + ylab("Percent")+
  ggtitle("Data Types used most at work or school")


multichoice %>% 
  select(Q1,Q32)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  group_by(Q1,Q32)%>%
  filter(!is.na(Q32))%>%
  summarise(Count = length(Q32))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(Q32,-percent), y = percent, fill = Q1)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+#coord_flip()+
  #geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.1, size =3)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40))+
  theme5+ xlab("") + ylab("Percent")+
  ggtitle("Data Types used most at work or school")

multichoice %>% 
  select(Q1,Q33_Part_1:Q33_Part_11)%>%
  filter(Q1 == "Female"|Q1 == "Male")%>%
  gather(2:12, key = "questions", value = "DataSource")%>%
  group_by(Q1,DataSource)%>%
  filter(!is.na(DataSource))%>%
  summarise(Count = length(DataSource))%>%
  mutate(percent =  prop.table(Count)*100)%>%
  ggplot(aes(x = reorder(DataSource,-percent), y = percent, fill = Q1)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))+#coord_flip()+
  #geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.5,vjust = 0.1, size =3)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))+
  theme5+ xlab("") + ylab("Percent")+
  ggtitle("Data Source used for Public Data")

















