HospitalCosts <- read_excel("C:/Users/karab/Downloads/HospitalCosts.csv")
getwd()
View(HospitalCosts)
summary(HospitalCosts)
hist(HospitalCosts$AGE)
summary(as.factor(HospitalCosts$AGE))
aggregate( TOTCHG ~ AGE, FUN = sum, data =HospitalCosts)
max(aggregate(TOTCHG ~ AGE, FUN = sum, data =( HospitalCosts) ))
which.max(summary(as.factor(HospitalCosts$APRDRG)))
          
# Based on the output we can see that Age wise Hospital Visit and Expenses
# Maximum Hospital Visit - 0-1 yrs age group : 307
# Maximum Expenditure - 0-1 yrs age group : 678118          
          
diagnosiscost <- aggregate(TOTCHG ~ APRDRG, FUN = sum, data =HospitalCosts)

diagnosiscost
diagnosiscost[which.max(diagnosiscost$TOTCHG),]
summary(as.factor(HospitalCosts$RACE))
head(HospitalCosts)

# Based on the output we can see the list of Expenditure based on the Diagnosis and treatment.
# TOTCHG - Hospital discharge costs
# APRDRG - All Patient Refined Diagnosis Related Groups
# 640 (All patient refined Diagnosis Related Gp) has the maximum expenditure - 437978

(HospitalCosts<-na.omit(HospitalCosts))          
(HospitalCosts$RACE<-as.factor(HospitalCosts$RACE))
(HospitalCosts$RACE<-as.factor(HospitalCosts$RACE))
model <- aov(TOTCHG ~ RACE, data =HospitalCosts )
model
summary(model)
summary(HospitalCosts$RACE)

# The Residual Value (deviation of the observed value) is very high specifying that there is no relation between the race of patient and the hospital cost.
# From the summary we can also see that the data has 484 patients of Race 1 out of the 500 entries. 
# This will affect the results of ANOVA as well, since the number of observations is very much skewed. 
# Hence we can conclude that there is no race wise cost biasness in the observed data.

model1 <- lm(TOTCHG ~ AGE + FEMALE, data = HospitalCosts)
(HospitalCosts$FEMALE<-as.factor(HospitalCosts$FEMALE))
model1 <- lm(TOTCHG ~ AGE + FEMALE, data =HospitalCosts)
summary(model1)
summary(HospitalCosts$FEMALE)

# Age is a very important factor in the hospital costs as seen by the significance levels and p-values.
# The gender also seems to have an impact.
# There is an equal number of male and female patients. 
# Based the negative coefficient we can conclude that females incur lesser cost than males. 

head(HospitalCosts)
HospitalCosts$RACE<-as.factor(HospitalCosts$RACE)
model2 <- lm(TOTCHG ~ AGE + FEMALE + RACE, data =HospitalCosts )
summary(model2)
model3 <- lm(TOTCHG ~ ., data =HospitalCosts )
summary(model3)

#The significance codes are almost null for all the variables, except for the intercept. 
# The p-value high which signifies that there is no linear relationship between the given variables. 
# Hence we cannot predict the length of stay of the patients based on the age, gender, and race.


#################################################################################################################################


# Analysis
 
# Based on the output we can see that the Age and Length of stay affects the total Hospital cost. 
# Cost is directly proportional to the Length i.e. higher the Length of stay of patients will result to higher hospital cost. 
# As per the output we can see that with an increase of 1 day stay, the hospital cost will increase by 742.

