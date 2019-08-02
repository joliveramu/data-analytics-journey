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
factor(table(expenses$ID.number))
factor(table(expenses$Gender))


  #Female Expenses
  female_exp <- select(-1:2) %>% filter(expenses, Gender =="Female") %>% arrange(daily_allowance)
  #female_exp
  #head(female_exp)
  #hist(female_exp$daily_allowance)
  female_exp
  ggplot(female_exp, aes(x=female_exp$daily_allowance, colour=ave_amount_spend))+
  geom_histogram(fill="white", alpha=0.3, position="identity", bins=30)+
  labs(title="Female Expenses", x="Allowance", y="Frequency")  
  
  
  #Male Expenses
  male_exp <- filter(expenses, Gender =="Male") %>% arrange(ave_amount_spend)
  
  male_exp 
  summarise(male_exp,total = sum(male_exp$daily_allowance)) 
  summarise(female_exp, total = sum(female_exp$daily_allowance)) 
  #head(male_exp)
  #hist(male_exp$daily_allowance)
  ggplot(male_exp, aes(x=male_exp$daily_allowance, colour=ave_amount_spend)) + 
  geom_histogram(fill="white", alpha=0.3, position="identity", bins=30)+
  labs(title="Male Expenses", x="Allowance", y="Frequency")

  #Expenses via section
  
  #Male
  Male_s_exp <- filter(expenses, Gender == "Male") %>% arrange(id,ave_amount_spend)
  Male_s_exp
  #Histogram
  ggplot(Male_s_exp, aes(x=Male_s_exp$id, colour=ave_amount_spend)) + 
    #geom_histogram(fill="white", alpha=0.3, position="identity", bins=30)+
    geom_bar(fill="white", position="dodge")+
    coord_flip()+
    labs(title="Average amount Spent by Male EM Students", x="Sections", y="Frequency")
  
  #Female
  Female_s_exp <- filter(expenses, Gender == "Female") %>% arrange(id, ave_amount_spend)
  Female_s_exp
  #Histogram
  ggplot(Female_s_exp, aes(x=Female_s_exp$id, colour=ave_amount_spend)) + 
    #geom_histogram(fill="white", alpha=0.3, position="identity", bins=30)+
    geom_bar(fill="white",alpha=0.3, position="dodge")+
    coord_flip()+
    labs(title="Average amount Spent by Female EM Students", x="Sections", y="Frequency")
  
#Female Expenses
#expenses %>%
	#filter(expenses, expenses$ave_amount_spend == "201-300", expenses$Gender == "Female") %>%
	#filter(expenses, expenses$ave_amount_spend == "100-200", expenses$Gender == "Female") 
	#filter(expenses, expenses$ave_amount_spend == "301-400", expenses$Gender == "Female")
	#filter(expenses, expenses$ave_amount_spend == "401-500", expenses$Gender == "Female")

#Male Expenses

	#filter(expenses, expenses$ave_amount_spend == "100-200", expenses$Gender == "Male") 
	#filter(expenses, expenses$ave_amount_spend == "201-300", expenses$Gender == "Male")
	#filter(expenses, expenses$ave_amount_spend == "301-400", expenses$Gender == "Male")
  #expenses %>%
  #select(expenses,expenses$Name:expenses$Name) %>%
	#filter(expenses, expenses$ave_amount_spend == "401-500", expenses$Gender == "Male")
