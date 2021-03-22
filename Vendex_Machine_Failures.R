#Import the relevant datasets 
library(tidyverse)
library(data.table)
library(dplyr)

t_data = fread(file = './transactional_data.csv',
               sep = ',',
               header = TRUE)
t_data = setDT(t_data)
t_data
summary(t_data)

m_fail = fread(file = './machine_failures.csv',
               sep = ',',
               header = TRUE)
m_fail = setDT(m_fail)
m_fail
summary(m_fail)

#Create the relevant variables for the model

#1

#Merge transactional data with machine failures

df = merge(t_data, m_fail, by = c('machine', 'timestamp'), all.x = T)

#Substitue NA values with "0"

df = df[is.na(failure), failure:=0]
df = subset(df, select = c('machine', 'timestamp','date', 'product_name', 'failure', 'column.x'))
df = setnames(df, 'column.x', 'column')

#2

#order transactions by machine and date

df = setorderv(df, cols= c('machine', 'date'))

#create variable "last_vend" (timestamp of last sale)

df = df[, last_vend:=shift(timestamp, n = 1L), by = machine]

#3

#create variable "deltahours" (for every transaction contains the hours which passed since the last sale)

df = df[, deltahours:= difftime(timestamp, last_vend, units = 'hours'), by = machine]

#4 

#create "machine_daily_average" (contains average daily sales per machine)

#count transactions per machine (unique)

trans_per_m = count(df,machine)

#count unique days each machine has been active

days_active = df[, length(unique(date)), machine]

#set data table to merge to the original one with all the necessary transformations (aux1 = auxiliary data table)

aux1 = merge(trans_per_m, days_active, by = 'machine')
aux1 = setDT(aux1)
aux1 = aux1[, machine_daily_average:= n/V1]
aux1 = subset(aux1, select = c('machine', 'machine_daily_average'))

#merge to original data table

df = merge(df, aux1, by = 'machine')

df = subset(df, select = c('machine', 'timestamp', 'date', 'product_name', 'failure', 
                           'column', 'last_vend', 'deltahours', 'machine_daily_average'))

#5

#create new variable called "delta" (normalized "deltahours")

df = df[, delta:= .(deltahours/(24/machine_daily_average))]


#Creating the model

#6 

#select machines for train and test sets

machines = unique(df$machine)
m_train = sample(machines, round(0.7*length(machines),0), replace = F)
m_test = setdiff(machines, m_train)

#create train and test sets

df %>% filter(machine %in% m_train) -> train_set
train_set
df %>% filter(machine %in% m_test) -> test_set
test_set

#model

m = glm(failure ~ delta, data = train_set, family = 'binomial')
summary(m)
m$coefficients
m_fitted = predict(m, newdata = train_set, type = 'response')

#Insight: the value for the intercept is equal to -6.91 while the value for the coefficient
#associated with delta is equal to 0.56

m_test = glm(failure ~ delta, data = test_set, family = 'binomial')
summary(m_test)
m_test$coefficients
m_fitted_test = predict(m, newdata = test_set, type = 'response')

#Insight: the value for the intercept is equal to -6.9 while the value for the coefficient 
#associated with delta is equal to 0.56

#questions

#a

roc(response=train_set$failure, predictor = m_fitted,
    auc = TRUE, plot = TRUE, col = 'red', 
    legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Positive Percentage")

roc(response=test_set$failure, predictor = m_fitted_test,
    auc = TRUE, plot = TRUE, col = 'purple',
    legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Positive Percentage")


#the value of the AUC for the model built on the train set is equal to 0.9191, while 
#the value for the AUC built on the test set is equal to 0.9219

#b 

#plot the function of probability of failure vs delta

m_int = m$coefficients['(Intercept)']
m_delta_coeff = m$coefficients['delta']

x = train_set$delta
curve(1/(1 + exp(-(m_int + m_delta_coeff*x))), from = 0, to = 30, xlab = 'delta', ylab = 'Logistic Function', col = 'red')

#c

#1

f = function(x){-0.6 + 1 / (1 + exp(-(m_int + m_delta_coeff*x)))}
delta_threshold_0.6 = uniroot(f, c(0,20))$root  # 13.04

f = function(x){-0.8 + 1 / (1 + exp(-(m_int + m_delta_coeff*x)))}
delta_threshold_0.8 = uniroot(f, c(0,20))$root  # 14.79

#the threshold for 60% is equal to 13.04 hours while the threshold for 80% is equal to 14.79 hours.

#2

#count number of unique days

length(unique(train_set$date)) #90
length(unique(test_set$date)) #90

#number of med-risk alarm per day

med_per_day = length(df[delta > delta_threshold_0.6]$delta)/90
med_per_day #40.8
high_per_day = length(df[delta > delta_threshold_0.8]$delta)/90
high_per_day #27.46

#3

#get observations for which we expect high-risk and med-risk alarm to be activated

high_risk_alarms = df[delta > delta_threshold_0.8]
med_risk_alarms = df[delta > delta_threshold_0.6]

#find % of false alarms per level of priority

High_False_Alarms = length(high_risk_alarms[failure == 0]$failure)
High_False_Alarms #214
High_False_Alarms_Perc = (High_False_Alarms/length(high_risk_alarms$column))*100
High_False_Alarms_Perc #the percentage of false alarms for the high-risk priority is equal to 8.66%

Med_False_Alarms = length(med_risk_alarms[failure == 0]$failure)
Med_False_Alarms #730
Med_False_Alarms_Perc = (Med_False_Alarms/length(med_risk_alarms$column))*100
Med_False_Alarms_Perc #the percentage of false alarms for the med-risk priority is equal to 19.88%

#d

#for med-risk alarms 

df1 = df[delta > delta_threshold_0.6]
df1 = df1[, threshold_hours:= (delta_threshold_0.6*(24/machine_daily_average))]
df1 = df1[, delta_fixed:= (threshold_hours+1.5)*(1/(24/machine_daily_average))]
df1 = df1[, won_sales:= failure*(delta - delta_fixed)]
df1 = df1[, additional_revenues:=won_sales*1.7]
times_operator_sent = length(df1[failure == 0]$failure)
times_operator_sent
total_cost = Med_False_Alarms*10
total_add_rev = sum(df1$additional_revenues)
profit_increase = total_add_rev - total_cost
profit_increase
perc_profit_med_risk = (profit_increase/(1.7*length(df$column)))*100
perc_profit_med_risk

#the percentage increase in profit by activating our system only on medium-risk alarms would be 
#equal to 1.96% with respect to the profit made on all transactions in the given period of 90 days

#for high-risk alarms

df2 = df[delta > high_risk_alarms]
df2 = df2[, threshold_hours:= (high_risk_alarms*(24/machine_daily_average))]
df2 = df2[, delta_fixed:= (threshold_hours+1.5)*(1/(24/machine_daily_average))]
df2 = df2[, won_sales:= failure*(delta - delta_fixed)]
df2 = df2[, additional_revenues:=won_sales*1.7]
df2
times_operator_sent = length(df2[failure == 0]$failure)
times_operator_sent
total_cost = High_False_Alarms*10
total_add_rev = sum(df2$additional_revenues)
profit_increase = total_add_rev - total_cost
profit_increase
perc_profit_high_risk = (profit_increase/(1.7*length(df$column)))*100
perc_profit_high_risk

#the percentage increase in profit by activating our system only on high-risk alarms would be 
#equal to 2.13% with respect to the profit made on all transactions in the given period of 90 days