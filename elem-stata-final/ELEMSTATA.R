#install.packages("dplyr")
#install.packages("ggplot2)

#install.packages("moments")

library(dplyr)
library(ggplot2)

library(moments)

setwd("C:/Users/John Oliver Amurao/Documents/elem-stata")
expenses <- read.csv(file="expenses.csv")

#View(expenses)
#Summary
head(expenses)
str(expenses)
summary(expenses)

factor(table(expenses$daily_allowance))
factor(table(expenses$ave_amount_spend))
factor(table(expenses$id))
factor(table(expenses$Gender))


  #Research questions
  #Profile of Respondents by Gender, Section
  summary(expenses$Gender) #Gender
  summary(as.factor(expenses$id)) #Section
  
  #Female Respondents per Section 
  fem_per_section <- filter(expenses, Gender=="Female") %>% arrange(id)
  summary(as.factor(fem_per_section$id))  
  
  #Male Respondents per Section
  mal_per_section <- filter(expenses, Gender=="Male") %>% arrange(id)
  summary(as.factor(mal_per_section$id))
  
  #MCT
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  #Male
  #mean
  mean(summary(as.factor(mal_per_section$id)))
  #median
  median(summary(as.factor(mal_per_section$id)))
  getmode(summary(as.factor(mal_per_section$id)))
  
  #Female
  #Mean
  mean(summary(as.factor(fem_per_section$id)))
  #Median
  median(summary(as.factor(fem_per_section$id)))
  #Mode
  getmode(summary(as.factor(fem_per_section$id)))
  
  #Measures of Validity
  #Male
  #Standard Deviation
  sd(summary(as.factor(mal_per_section$id)))
  #Range
  range(summary(as.factor(mal_per_section$id)))
  #IQR
  IQR(summary(as.factor(mal_per_section$id)))

  #Female
  #Standard Deviation
  sd(summary(as.factor(fem_per_section$id)))
  #Range
  range(summary(as.factor(fem_per_section$id)))
  #IQR
  IQR(summary(as.factor(fem_per_section$id)))

  #Measures of skewness and kurtosis
  
  #Skewness Female
  skewness(summary(as.factor(fem_per_section$id)))
  #Kurtosis Female
  kurtosis(summary(as.factor(fem_per_section$id)))

  #Skewness Male
  skewness(summary(as.factor(mal_per_section$id)))
  #Kurtosis Male
  kurtosis(summary(as.factor(fem_per_section$id)))
  
  #Other Statistics
  
  #Female Max
  max(summary(as.factor(fem_per_section$id)))
  #Female Min
  min(summary(as.factor(fem_per_section$id)))  
  
  #Male Max
  max(summary(as.factor(mal_per_section$id)))
  #Male Min
  min(summary(as.factor(mal_per_section$id)))

  
  #Male x Female Respondents
  ggplot(expenses, aes(x=Gender, fill=Gender))+
    geom_bar(position="stack",stat="count")+
    labs(title="EM Respondents by Gender",x="Gender",y="Frequency")
  
  
  #Female Respondents by section
  f_resp_section <- select(expenses,-1,-2) %>% filter(expenses$Gender == "Female") %>%
    arrange(id)
  f_resp_section
  
  summary(as.factor(f_resp_section$id))
  ggplot(f_resp_section, aes(x=as.factor(f_resp_section$id), fill=sections) )+
    geom_bar()+
    labs(x="Sections",y="Frequency",title="Female EM respondents by Section")
  
  #Male Respondents by Section
  m_resp_section <- select(expenses, -1,-2) %>% filter(expenses$Gender=="Male") %>%
    arrange(id)
  m_resp_section
  
  #plot(as.factor(m_resp_section$id))
  summary(as.factor(m_resp_section$id))
  
  ggplot(m_resp_section, aes(as.factor(m_resp_section$id), fill=Sections)) +
    geom_bar()+
    labs(x="Section",y="Frequency",title="Male EM Respondents per Section")
  
  Sections <- as.factor(m_resp_section$id)
  Sections
  
  sections <- as.factor(f_resp_section$id)
  sections
  
  #Respondents per section
  Section <- as.factor(expenses$id)
  ggplot(expenses, aes(x=Section, fill=Section) ) +
    geom_bar()+
    labs(x="Section",y="Frequency",title="EM Respondents per Section")
  
  #What is the average amount that an EM student spends on food on a daily basis
  average_amount <- select(expenses,2:6) %>% arrange(id)
  average_amount  
  
  Amount_Spent <-average_amount$ave_amount_spend
  
  table(Amount_Spent)
  
  ggplot(data=average_amount, aes(x=average_amount$ave_amount_spend, fill=Amount_Spent) )+
    geom_bar()+
    labs(x="Amount Spent",y="Frequency",title="Average Amount spent in food ona daily basis of EM Respondents")
    
  ggplot(expenses, aes(x=ave_amount_spend, fill=Gender))+
    geom_bar(position="dodge",stat="count")+
    labs(title="Average Amount Spent in food on a daily basis of EM Respondents (by Gender)",x="Average Amount Spent",y="Frequency")
  
  female_exp <- filter(expenses, Gender =="Female") %>% arrange(daily_allowance)
  #female_exp
  #head(female_exp)
  #hist(female_exp$daily_allowance)
  female_exp
  ggplot(female_exp, aes(x=female_exp$daily_allowance, fill=ave_amount_spend))+
    geom_histogram(position="dodge",bins="30",binwidth = 150)+
    labs(title="Average Food Expenses of Female EM Respondents", x="Allowance", y="Frequency")  
  male_exp <- filter(expenses, Gender =="Male") %>% arrange(ave_amount_spend)
  
  male_exp 
  summarise(male_exp,total = sum(male_exp$daily_allowance)) 
  summarise(female_exp, total = sum(female_exp$daily_allowance)) 
  #head(male_exp)
  #hist(male_exp$daily_allowance)
  ggplot(male_exp, aes(x=male_exp$daily_allowance, fill=ave_amount_spend)) + 
    geom_histogram( position="dodge", bins=20, binwidth=50)+
    labs(title="Average Food Expenses of Male EM Respondents", x="Allowance", y="Frequency")
  
  
  Male_s_exp <- filter(expenses, Gender == "Male") %>% arrange(id,ave_amount_spend)
  Male_s_exp
  #Histogram
  ggplot(Male_s_exp, aes(x=Male_s_exp$id, fill=ave_amount_spend)) + 
    geom_bar(position="dodge")+
    coord_flip()+
    labs(title="Average amount Spent on food by Male EM Respondents per Section", x="Sections", y="Frequency")
  
  #Female
  Female_s_exp <- filter(expenses, Gender == "Female") %>% arrange(id, ave_amount_spend)
  Female_s_exp
  #Histogram
  ggplot(Female_s_exp, aes(x=Female_s_exp$id, fill=ave_amount_spend)) + 
    geom_bar(position="dodge")+
    coord_flip()+
    labs(title="Average amount Spent on Food by Female EM Respondents per Section", x="Sections", y="Frequency")
  
  #Average amount spend by Section
  aver_amount_spend_per_sec <- select(expenses,-1) %>% arrange(id)
  
  aver_amount_spend_per_sec
  
  ggplot(aver_amount_spend_per_sec, aes(x=id, fill=ave_amount_spend))+
    geom_bar(position="dodge",stat="count")+
    coord_flip()+
    labs(title="Average Amount Spent on food on a daily basis per Section",x="Section",y="Frequency")
  
  #What is the average allowance of an EM student? (Daily)
  average_allowance <- select(expenses, 2:6) %>% arrange(id) %>% group_by(id,daily_allowance)
  average_allowance
  
  Allowance <- as.factor(average_allowance$daily_allowance)
  #Counts per Category
  table(Allowance)
  summary(average_allowance$daily_allowance)

  ggplot(data=average_allowance, aes(x=average_allowance$daily_allowance, fill=Allowance))+
    geom_histogram(binwidth = 50)+
    labs(x="Allowance",y="Frequency",title="Daliy Allowance of EM Respondents")

  

    fem_average_allowance <- select(expenses,-1,-2) %>%filter(expenses$Gender == "Female") %>% arrange(id,daily_allowance)
    fem_average_allowance
    
    ID <- as.factor(fem_average_allowance$id)
   
     ggplot(data=fem_average_allowance, aes(x=fem_average_allowance$daily_allowance, fill=ID))+
    geom_histogram(position="dodge", bins=30,binwidth = 50)+
    labs(x="Daily Allowance",y="Frequency",title="Daily Allowance of Female EM Respondents by Section")
    
    mal_average_allowance <- select(expenses, -1,-2) %>% filter(expenses$Gender=="Male")%>% arrange(id,daily_allowance)
    mal_average_allowance
    Section_ID <- as.factor(mal_average_allowance$id)
    
    ggplot(data=mal_average_allowance, aes(x=mal_average_allowance$daily_allowance, fill=Section_ID))+
      geom_histogram(position="dodge", bins=30,binwidth = 75)+
      labs(x="Daily Allowance",y="Frequency",title="Daily Allowance of Male EM Respondents by Section")
  
    ggplot(data=expenses, aes(x=expenses$daily_allowance, fill=Gender))+
      geom_bar(position="dodge")+
      labs(x="Daily Allowance",y="Frequency",title="Daily Allowance of EM Respondents by Gender")
  
    m_allow <- select(expenses, daily_allowance)%>% filter(expenses$Gender=="Male")
    f_allow <- select(expenses, daily_allowance)%>%filter(expenses$Gender=="Female")
     
    m_allow #Male Allowance
    f_allow #Female Allowance
    
    #t.test(m_allow,f_allow, conf.level = 0.95)  
    
    #MCT
    #Male
    summary(m_allow$daily_allowance)
    #Mean
    mean(m_allow$daily_allowance)
    #Median
    median(m_allow$daily_allowance)
    #Mode
    getmode(m_allow$daily_allowance)
    
    #Measures of Variability
    #Standard Deviation Male
    sd(m_allow$daily_allowance)
    #Range Male
    range(m_allow$daily_allowance)
    #IQR Male
    IQR(m_allow$daily_allowance)
    
    #Measures of skewness and kurtosis
    #Skewness Male
    skewness(m_allow$daily_allowance)
    #Kurtosis Male
    kurtosis(m_allow$daily_allowance)
    
    #Other Statistics
    #Max
    max(m_allow$daily_allowance)
    #Min
    min(m_allow$daily_allowance) 
    
    #Female
    #MCT
    #Female
    summary(f_allow$daily_allowance)
    #Mean
    mean(f_allow$daily_allowance)
    #Median
    median(f_allow$daily_allowance)
    #Mode
    getmode(f_allow$daily_allowance)
    
    #Measures of Variability
    #Standard Deviation Female
    sd(f_allow$daily_allowance)
    #Range Female
    range(f_allow$daily_allowance)
    #IQR Female
    IQR(f_allow$daily_allowance)
    
    #Measures of skewness and kurtosis
    #Skewness Female
    skewness(f_allow$daily_allowance)
    #Kurtosis Female
    kurtosis(f_allow$daily_allowance)
    
    #Other Statistics
    #Max
    max(f_allow$daily_allowance)
    #Min
    min(f_allow$daily_allowance) 
    
    #Plotting Skewness and Kurtosis of Allowance (Female)
    qqnorm(f_allow$daily_allowance)
    qqline(f_allow$daily_allowance)
    
    #Plotting Skewness and kurtosis of Allowance (Male)
    qqnorm(m_allow$daily_allowance)
    qqline(m_allow$daily_allowance)
    