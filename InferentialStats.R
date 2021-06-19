# DESCRIPTIVE STATISTICS 
## Descriptive statistics is also known as summary statistics.
## It is a representation of what the data set is about, a summary.
## There are a number of different functions in R to get summary statistics.
## We are going to use pastecs package for descriptive statistics here.  

## Import data file 
my_data <- read.csv(file.choose()) # use salary.csv 

## Print the first 5 rows
head(my_data, 5) # see Employee and Salary columns

## Install pastecs package for descriptive statistics
install.packages("pastecs")

## Load library
library(pastecs)

## Calculate summary statistics 
descrip <- stat.desc(my_data$Salary) # Use Salary variable 
round(descrip, 2) # Assume normal distribution 


# Z SCORE
## Z score is the probability of a score occurring within the normal distribution.
## No built-in function for Z score calculation in R.
## Therefore, we need to calculate the needed numbers using formula.
## Using the same salary.csv file, we are going to calculate Z-scores.

## Employee salary standard deviation
sal_sd <- sd(my_data$Salary)*sqrt(length(my_data$Salary)-1)/(length(my_data$Salary))
sal_sd # stdev = 2.03

## Employee salary mean
sal_mean <- mean(my_data$Salary)
sal_mean # mean = 72.5

## Z score calculation
Z_score <- (my_data$Salary - sal_mean )/ sal_sd
Z_score # Since there are 16 observations, see 16 Z-scores

## Combine the calculated Z score into the data set 
New_my_data <- cbind(my_data, Z_score) #cbind: combine
head(New_my_data, 5) # add Z_score column to my_data

## Export data
write.csv(New_my_data,"new_salary.csv") # new file under Files tab

## Find percentage of employees who earn more than the mean salary. 
## pnorm is a function for normal distribution, p stands for probability. 

## First, define per_less_mean, percentage less than mean salary
per_less_mean <- pnorm(72, sal_mean, sal_sd) # mean = 72
per_less_mean # show percent earning less than mean salary = 0.40

## Then, per_more_mean is the difference between 1 and per_less_mean
per_more_mean <- 1 - per_less_mean 
per_more_mean # show percent earning greater than mean salary = 0.60


# Z-TEST
## Z-Test:used to determine whether two populations are similar or not
## H0 = two populations are similar
## H1 = two populations are not similar

## Import the first data set 
salary1 <- read.csv(file.choose()) # use salary1.csv

## Calculate mean of Salary variable from salary1.csv
mean(salary1$salary) # mean of salary1 = 23.54
 
## Now, import the second data set 
salary2 <- read.csv(file.choose()) # use salary2.csv

## Calculate variance of Salary variable from salary2.csv
var(salary2$salary) # var = 14.20

## Once again since there's no built-in Z test function in R, 
## We need to create the function.  We will use the mean from salary1 and 
## variance from salary2 to see if the two data sets are similar or not
## a = vector of values
## mu = population means
## var = population variance 
## zeta = z.test "number" 
z.test = function(a,mu,var){
  zeta=(mean(a)-mu)/(sqrt(var/length(a)))
  return(zeta)} 

# Calculate z statistics
a<- salary2$`salary`
z <- z.test(a,23.5475,14.19435) 
z # show numeric value of z = 0.84

# Calculate the p-value
p_value <- 2*pnorm(-abs(z))
p_value # show numeric value of p-value = 0.40

# z = 0.84
# p-value = 0.40
# Default alpha = 0.05, p-value is greater than 0.05, so means of both data sets
# are the same and are not statistically significantly different from 
# each other.


# T-TEST
## t-test is similar to Z-test, but differ in sample size (N)
## If sample size is greater than 30, use Z-test.  If not, use t-test
## t-test is used to see whether two groups are similar or not. 

## One Sample t-Test
## Two sided one sample t-Test: determines whether the sample mean is statistically
## different from a known or hypothesized population mean

## Read data 
cars_data <- read.csv(file.choose()) # use cars.csv
View(cars_data) # Use View to see the data in new tab.

# Assume normal distribution, set mu = 30000 (population mean)
# H0 = mean of Price is not greater than mu (means are equal)
# H1 = mean of Price is greater than mu (different)
t.test(cars_data$Price,mu=30000,alternative="two.sided") 
# p-value = 0.107, which bigger than 0.05
# Accept H0 which indicates that mean of Price is not greater than mu

## Independent t-Test 
## Independent t-test is used to test whether there is a statistically significant 
## difference between the means from two groups

## Import data 
customer_data <- read.csv(file.choose()) # use customers.csv 
View(customer_data)

## Before t-test, run F-test to determine variance between two groups.
res.ftest <- var.test(Age ~ Gender, data = customer_data) # Gender: male or female
res.ftest
## p = 0.9218 is greater than 0.05, which indicates no differences between the variances
## between two groups.  Therefore, assume equal variances.  

## Now, we can run the Independent t-test.  
## Set mu (population mean) equal 0 (mean2 - mean1 = 0)
## var.eq= T assumes equal variances, change to F when assumes unequal variances
## var.eq is used to tell the program whether variables have
## equal variances or not
## H0 = two groups are the same
## H1 = two groups are different
t.test(customer_data$Age~customer_data$Gender,mu=0,alt="two.sided",var.eq=T)
## p = 0.6677 is greater than 0.05, which indicates no differences between two groups.
## Therefore, assume that the two groups are the same.  

## Paired T-TEST
## Paired t-test determines whether a group who exhibit the same "behavior" 
## change over periods of time.
## H0 = Behavior is the same
## H1 = Behavior is different

## Import data 
spend_data <- read.csv(file.choose()) # use spending.csv
View(spend_data)

## Set up paired test
## spend_data$`Month1` and `Month2`are the variables used
## mu = 0 (mean2 - mean1 = 0)
## alt = the alternative hypothesis. Allowed value is one of “two.sided” (default), “greater” or “less
## paired = logical value specifying that we want to compute a paired t-test
t.test(spend_data$`Month1`, spend_data$`Month2`,mu=0,alt = "two.sided",paired = T)
## p-value = 1.217e-06, less than 0.05 so reject H0 and assume that the behavior 
## is different between Month1 and Month2.  


## One-Way ANOVA analysis of variance()
## One way ANOVA tests whether the the means from different groups are equal or not
## H0 = the means are the same
## H1 = the means are different

## Import data 
age_anova <- read.csv(file.choose()) # use customers.csv
View(age_anova)

## ANOVA
## Age~Gender: Specify attribute (Age) and Gender (groups)
## data: specify name of the data set (age_anova)
ANOVA1 <- aov(Age~Gender,data=age_anova)
ANOVA1 
summary(ANOVA1) # show ANOVA result
## p-value = 0.668, greater than 0.05 so conclude that there the two means are equal.
## Note that t test and one way Anova are similar, but use t-test for two groups, 
## and use ANOVA for three or more groups.  


# CHI-SQUARE TEST 
## Chi-square test determines the level of association between two categorical variables

## Import file
pay_data <- read.csv(file.choose()) # use paycard.csv
View(pay_data) # Two categorical variables: Gender and Card
## Gender: male (m) or female (f)
## Card: Yes (y) for pay with card and No (n) for not pay with card

## Construct frequency table using the two categorical variables
freq_table <- table(pay_data$Gender, pay_data$Card)
freq_table # Show the frequency table

## Chi-square test
## Find whether there's relationship between gender and pay with card
## HO = these two variables are related to each other
## H1 = these two variables are not related to each other
## freq_table is used for the test 
## correct: indicate whether to apply correction when working with 2x2 table
## use True (T) if no correction is done, use False (F) if need correction
CHI <- chisq.test(freq_table, correct = T) 
CHI

## Return Attributes
## Attributes return object whose attributes to be accessed.
attributes(CHI)

## Expected chi-square table
CHI$expected
# p value = 0.3687, so can't reject H0, this suggests that variables 
# are related to each other