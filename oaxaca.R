###############################################################
##### How to Audit Your Gender Pay Gap:
##### An Employers Guide Using R
#####
##### by Andrew Chamberlain, Ph.D.
##### Glassdoor, Inc.
#####
##### March 2017
#####
##### Contact:
#####   Web: www.glassdoor.com/research
#####   Email: economics@glassdoor.com
###############################################################

# Clear workspace. 
rm(list=ls())

# Load R libraries.
library(stargazer)
library(broom)
library(dplyr)

# Turn off scientific notation. 
options(scipen=999) 

# Load data. 
data <- read.csv("C:\\Users\\ambatipudir\\Desktop\\oaxaca.csv", header = TRUE) # N = 1000 total observations. 
colnames(data) <- c("Employee","Job_Title","Job_Grade","Gender_Reduced","Gender","Full_Part_Time_Reduced","Full_Part_Time","Total_Compensation","Years_of_Experience","Performance_Score_Reduced","Performance_Rating","Location","Special_Qualifications","Time_Traveling","Shifts")

#############################
# Data Cleaning and Prep.
#############################

# Create five employee age bins.
data$age_bin <- 0
data$age_bin <- ifelse(data$age < 25, 1, data$age_bin) # Below age 25
data$age_bin <- ifelse(data$age >= 25 & data$age < 35, 2, data$age_bin) # Age 25-34.
data$age_bin <- ifelse(data$age >= 35 & data$age < 45, 3, data$age_bin) # Age 35-44.
data$age_bin <- ifelse(data$age >= 45 & data$age < 55, 4, data$age_bin) # Age 45-54.
data$age_bin <- ifelse(data$age >= 55, 5, data$age_bin) # Age 55+.

# Take natural logarithm of compensation variables (for percentage pay gap interpretation in regressions).
data$log_total_compensation <- log(data$Total_Compensation, base = exp(1)) # Total comp.

# Check the structure of the imported data.
str(data)

# Cast all categorial variable as factors for the regression analysis. 
data$Job_Title <- as.factor(data$Job_Title)
data$Job_Grade <- as.factor(data$Job_Grade)
data$Gender <- as.factor(data$Gender)
data$Full_Part_Time <- as.factor(data$Full_Part_Time)
data$Performance_Rating <- as.factor(data$Performance_Rating)
data$Location <- as.factor(data$Location)
data$Special_Qualifications <- as.factor(data$Special_Qualifications)
data$Shifts <- as.factor(data$Shifts)
data$Time_Traveling <- as.factor(data$Time_Traveling)

# https://medium.com/@MarekHlavac/package-oaxaca-for-blinder-oaxaca-decomposition-in-r-part-2-features-and-example-12222e9a886d
library(VIF)
library(car)

library(oaxaca)

job_results <- oaxaca(log_total_compensation ~ Job_Title + age_bin + Full_Part_Time + Years_of_Experience + Performance_Rating + Location + Special_Qualifications + Shifts | Gender_Reduced, data = data)
summary.oaxaca(job_results)

lm_job_results <- lm(log_total_compensation ~ Job_Grade + Full_Part_Time + Years_of_Experience + Performance_Score_Reduced + Location + Special_Qualifications + Shifts| Gender_Reduced, data = data)
summary.oaxaca(lm_job_results)
vif(lm_job_results)

job_results$threefold$overall

plot(job_results, decomposition = "twofold", group.weight = -1)

plot(job_results, decomposition = "threefold", group.weight = -1)

job_results$twofold$overall
job_results$twofold$variables

plot.oaxaca(job_results, decomposition = "twofold", group.weight = -1,
            unexplained.split = FALSE) 

plot.oaxaca(job_results, decomposition = "twofold", group.weight = -1,
            unexplained.split = TRUE, components = c("unexplained A(Male)", "unexplained B(Female)"))

variables <- c('Years_of_Experience','Performance_RatingGood')
columns <- c('coef(unexplained A)', 'coef(unexplained B)')
job_results$twofold$variables[[1]][variables, columns]

library(GGally)
ggpairs(data$Job_Title, data$Job_Grade, data$Gender_Reduced, data$Gender, data$Full_Part_Time_Reduced, data$Full_Part_Time, data$Total_Compensation, data$Years_of_Experience, data$Performance_Score_Reduced, data$Performance_Rating, data$Location, data$Special_Qualifications, data$Time_Traveling, data$Shifts)

