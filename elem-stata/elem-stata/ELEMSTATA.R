library(dplyr)
library(ggplot2)
setwd("C:/Users/John Oliver Amurao/Documents/elem-stata")
expenses <- read.csv(file="expenses.csv")

#View(expenses)
head(expenses)
str(expenses)
summary(expenses)

factor(table(expenses$daily_allowance))
factor(table(expenses$ave_amount_spend))
factor(table(expenses$id))
factor(table(expenses$Gender))

factor(table(expenses$id))


#Sections
Sections <- as.factor(m_resp_section$id)
Sections

sections <- as.factor(f_resp_section$id)
sections

#Female Respondents by section
f_resp_section <- select(expenses,-1,-2) %>% filter(expenses$Gender == "Female") %>%
   arrange(id)
f_resp_section

summary(as.factor(f_resp_section$id))
ggplot(f_resp_section, aes(x=as.factor(f_resp_section$id), fill=sections) )+
  geom_bar()+
  labs(x="Sections",y="Frequency",title="Female respondents by Section")

#Male Respondents by Section
m_resp_section <- select(expenses, -1,-2) %>% filter(expenses$Gender=="Male") %>%
  arrange(id)
m_resp_section

#plot(as.factor(m_resp_section$id))
summary(as.factor(m_resp_section$id))
 


ggplot(m_resp_section, aes(as.factor(m_resp_section$id), fill=Sections)) +
  geom_bar()+
  labs(x="Section",y="Frequency",title="Male Respondents per Section")



#female_section <- filter(expenses, expenses$Gender == "Female") %>% arrange(id)
#female_section

  #Female Expenses
  female_exp <- filter(expenses, Gender =="Female") %>% arrange(daily_allowance)
  #female_exp
  #head(female_exp)
  #hist(female_exp$daily_allowance)
  female_exp
  ggplot(female_exp, aes(x=female_exp$daily_allowance, fill=ave_amount_spend))+
  geom_histogram(position="dodge",bins="30",binwidth = 150)+
  labs(title="Female Expenses", x="Allowance", y="Frequency")  
  
  
  #Male Expenses
  male_exp <- filter(expenses, Gender =="Male") %>% arrange(ave_amount_spend)
  
  male_exp 
  summarise(male_exp,total = sum(male_exp$daily_allowance)) 
  summarise(female_exp, total = sum(female_exp$daily_allowance)) 
  #head(male_exp)
  #hist(male_exp$daily_allowance)
  ggplot(male_exp, aes(x=male_exp$daily_allowance, fill=ave_amount_spend)) + 
  geom_histogram( position="dodge", bins=20, binwidth=50)+
  labs(title="Male Expenses", x="Allowance", y="Frequency")

  #Expenses via section
  
  #Male
  Male_s_exp <- filter(expenses, Gender == "Male") %>% arrange(id,ave_amount_spend)
  Male_s_exp
  #Histogram
  ggplot(Male_s_exp, aes(x=Male_s_exp$id, fill=ave_amount_spend)) + 
    #geom_histogram(fill="white", alpha=0.3, position="identity", bins=30)+
    geom_bar(position="dodge")+
    coord_flip()+
    labs(title="Average amount Spent on food by Male EM Students", x="Sections", y="Frequency")
  
  #Female
  Female_s_exp <- filter(expenses, Gender == "Female") %>% arrange(id, ave_amount_spend)
  Female_s_exp
  #Histogram
  ggplot(Female_s_exp, aes(x=Female_s_exp$id, fill=ave_amount_spend)) + 
    #geom_histogram(fill="white", alpha=0.3, position="identity", bins=30)+
    geom_bar(alpha=0.5, position="dodge")+
    coord_flip()+
    labs(title="Average amount Spent on Food by Female EM Students", x="Sections", y="Frequency")
    
  
  #Male vs Female
  #Respondents
  ggplot(expenses, aes(x=Gender, fill=Gender))+
    geom_bar(position="stack",stat="count")+
    labs(title="EM Respondents",x="Gender",y="Frequency")
  
  #expenses
  ggplot(expenses, aes(x=ave_amount_spend, fill=Gender))+
    geom_bar(position="dodge",stat="count")+
    labs(title="Average Amount Spent in food on a daily basis",x="Average Amount Spent",y="Frequency")
  
  #Average amount spend by Section
  ggplot(expenses, aes(x=id, fill=ave_amount_spend))+
    geom_bar(position="dodge",stat="count")+
    coord_flip()+
    labs(title="Average Amount Spent on food on a daily basis per Section",x="Section",y="Frequency")

  #Average amount spend by section simple graph and plotting
    
  plot(expenses$Gender)
  hist(expenses$id)
  plot(expenses$ave_amount_spend)
  hist(expenses$daily_allowance)
