setwd("/home/suman/Documents/data science")
rd<-read.csv("bike_buyers.csv",TRUE,",")

#overview of datasets 
str(rd)
summary(rd)
class(rd)

View(rd)
head(rd)

#univariate analysis 
#discrete variables and continuous variables 
#discrete - categorical (eg: education , occupation)
#continuous - not categorised (eg: income , children)

#central tendency of continuous variable (income)
summary(rd$Income)
boxplot(rd$Income , border = "blue" ,col = "grey" , main = "Income central tendency" , ylab = "incomes")

#spread of income 
hist(rd$Income , xlab = "Income" , main = "Spread of Income")
#spread of income (using density curve)
plot(density(rd$Income) , main = "Density curve of income")

#discrete variable (education)
#categorical data 
summary(rd$Education)
plot(rd$Education , main = "No of bike_buyers from different education field " , col = "orange")

#discrete variable (marital status)
#categorical data
summary(rd$Marital.Status)
plot(rd$Marital.Status , main = "No of bike_buyers Married and Single" , col = "pink")

#continuous variable (children)
summary(rd$Children)
#factoring this column to make it categorical data
rd$Children<-factor(rd$Children)
summary(rd$Children)                            #labels (now a factor)
plot(rd$Children , main = "No of customers having Children and how many" , xlab = "Labels(no. of children)" , col = "purple")

library(dplyr)
bought<-filter(rd , Purchased.Bike=="Yes")
#those who have bought the bikes 
View(bought)

#No. of bikes Purchased 
plot(rd$Purchased.Bike , main = "No of Purchased Bikes" , col = "blue")
pie(table(rd$Purchased.Bike) ,  main = "Purchased Bike")

#No. of children buyers(who have bought the bikes) are having
plot(bought$Children , main = "No. of Children of Purchased bike_buyers" , xlab = "no of children" , col = "violet")

#Singles have bought more bikes than married ones 
plot(bought$Marital.Status , main = "Marital Status of Purchased bike_buyers" , col = "green")

#Bachelors are the most buyers 
plot(bought$Education , main = "Stream of Purchased bike_buyers" , col = "red")

#Professionals are most of the buyers 
plot(bought$Occupation , main = "Occupation of Purchased bike_buyers" , col = "blue")

#dataset contains more male bike_buyers
plot(rd$Gender , main = "Gender of bike_customers" , col = "grey")
#And males have bought more than females (slight difference)
plot(bought$Gender , main = "Gender of bike_buyers" , col = "magenta")

#pie charts 
pie(table(rd$Marital.Status) , main = "Married Vs Single")
pie(table(rd$Gender) , main = "Female Vs Male")
pie(table(rd$Home.Owner) , main = "No of Home Owners")

#Home owners are more buyers 
plot(bought$Home.Owner , main = "No. of home owners buyers" , col = "yellow")

#Most of the buyers are of age group 40-50
plot(bought$Age , main = "Age of buyers" , col = "pink" , type = "l" , ylab = "Age")

#Distance travelled 
summary(rd$Commute.Distance)
#difference in graphs can be seen 
plot(rd$Commute.Distance , main = "Distance travelled" , col = "violet")
#Most bikes travel 0-1 miles 
plot(bought$Commute.Distance , main = "Distance travelled by bought bikes" , col = "yellow")


#multivariate analysis 

#relationship between discrete(categorical data) variable and continuous variable 
#comparing education to income (using different FUN)
by(rd$Income , rd$Education , summary)                   
by(rd$Income , rd$Education , mean)
by(rd$Income , rd$Education , median)

boxplot(rd$Income~rd$Education , notch = TRUE , main = "Income distribution among different education levels" , col = "orange")
#density curves showing income distribution in different educations levels 
library(sm)                                       #for summary information 

sm.density.compare(rd$Income , rd$Education , xlab = "Income")
#creating labels
edu_label<-factor(rd$Education , labels = c("Bachelors" , "Graduate Degree" , "High School" , "Partial College" , "Partial High School"))

colfill<-c(2:(2+length(edu_label)))
legend(locator(1), levels(edu_label) , fill = colfill)

#relationship between categorical and categorical data 
#creates a table between two categorical data 
xtabs( ~Education+Purchased.Bike , rd)
#width and height is used to compare 
#Bachelors are more as compared to Partial High School 
plot(xtabs( ~Education+Purchased.Bike , rd) , main = "Bike_buyers and Education" , col = "magenta")

#similarly for Occupation 
xtabs( ~Occupation+Purchased.Bike , rd)
#width and height is used to compare 
#Professionals are more as compared to Manual 
plot(xtabs( ~Occupation+Purchased.Bike , rd) , main = "Bike_buyers and Occupation" , col = "blue")

#extraction of some important data from dataset 

#correlation
cor(rd[ ,c(4,12)])
#graph
pairs(rd[ , -c(1,2,3,6,7,8,9,10,11,13)])
#covariance
is.numeric(rd$Income)
is.numeric(rd$Children)
cov(rd[ ,c(4,12)])

#relationship between 3 variable together 

#No. of customers who have bought/not bought bikes from different regions 
aggregate(Age ~ Purchased.Bike + Region , data = rd , FUN = "length")

#Customers who are having bikes/cars according to age 
aggregate(Cars ~ Purchased.Bike + Age , data = rd , FUN = "length")


library(tidyverse)
#Male have more income than female , very less females have 5 children 
#scatterplot 
ggplot(rd , aes(x=Children , y=Income , color = Gender)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = 'lm')

#Most married customers are in Europe of age group 60-80 
ggplot(rd , aes(x=Age , y=Home.Owner , color = Marital.Status)) +
  geom_line() +
  facet_grid(~Region)

#Miles Travelled 
#barplot 
ggplot(rd , aes(x=Commute.Distance)) +
  geom_bar() 

#graph showing relationship between Income & Occupation 
#No. of females/males involved in which occupation and the income they get  
#plus which gender and which education they are on 
ggplot(rd , aes(x=Income , y=Occupation , color = Gender)) +
  geom_point() +
  facet_grid(~Education)

#frequency_plot
#Which Occupation is paid higher
ggplot(rd , mapping = aes(x = Income )) + 
  geom_freqpoly(mapping = aes(colour = Occupation), binwidth = 500)

#diffrence between boxplot func. and ggplot boxplot is we can have extra features in ggplot boxplot 
#like changing the colors of outliers 
#we can see people of age upto 70 crosses 10+ miles 
#Old people crossing age 80 travel only 0-1 miles 
ggplot(rd , aes(x=Commute.Distance , y=Age)) +
  geom_boxplot(outlier.colour = "orange" , notch = TRUE , color = "purple")

#qplot 
#income distribution between male & female 
qplot(Income , data = rd, geom = "histogram",
      fill = Gender , main = "Income Distribution between Male & Female")

#Equivalent distance travelled in all regions 
#both are categorical data 
qplot(Region , Commute.Distance , data = rd , geom = c("point" , "line") , color = Region ,
      main = "Distance travelled in different regions")

#density curve 
#More Management workers have 4 children , very less Clericals have 5 children 
qplot(Children, data = rd , geom = "density",
      color = Occupation , linetype = Occupation , main = "No. of children based on Occupation")












