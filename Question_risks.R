#Example: 
#Load libraries####
library(decisionSupport)
library(tidyverse)

#Define the "Make variables" function####
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

#read in the Input table 
Question_input<-read.csv2("Question_v1.csv")

#Compute test values with make_variables####
make_variables(as.estimate(Question_input))

#Decision function####

  #number of frost nights per year (during blossom time)
  
  harmful_frost_event<-chance_event(frost_risk, value_if = round(vv(n_nights_frost, var_CV = var_cv, n = n_years)), value_if_not = rep(0, n_years), n = n_years )
  
  #share of the protection in harmful frost nights on all nights with protection (usually calculated)
  share_harmful_of_protection<-c(1,1,0.6,0.7,1,1,1,0.6,0.7,1,1,1,0.6,0.7,1,1,1,0.6,0.7,1)
  
  #risk 1
  risk_1_list <- vector(mode = "list", length = n_years)
  for (i in 1:n_years){
    if  (harmful_frost_event[i]!=0){
      risk_1_list[[i]]<-chance_event(risk_1, n= harmful_frost_event[i])}else{
        risk_1_list[[i]]<-0}}
  risk_1_matrix<-t(sapply(risk_1_list, `length<-`, max(2,max(harmful_frost_event)))) #max(2,...) the 2 ensures to get an matrix also in years without frost
  risk_1_matrix_na_0<-risk_1_matrix %>% replace(is.na(.),0)
  
  #risk 2
  risk_2_list <- vector(mode = "list", length = n_years)
  for (i in 1:n_years){
    if  (harmful_frost_event[i]!=0){
      risk_2_list[[i]]<-chance_event(risk_2, n= harmful_frost_event[i])}else{
        risk_2_list[[i]]<-0}}
  risk_2_matrix<-t(sapply(risk_2_list, `length<-`, max(2,max(harmful_frost_event)))) #max(2,...) the 2 ensures to get an matrix also in years without frost
  risk_2_matrix_na_0<-risk_2_matrix %>% replace(is.na(.),0)
  
  #prot stop missing something
  
  #the number of stops is usually calculated from the availability, this is just to make it easier in the example
  n_protection_stop_example<-chance_event(stop_risk, value_if = n_stops, value_if_not = 0, n=n_years)
  
  #number of stop and harmful events
  stop_harmful_example<-rep(NA, n_years)
  for(i in 1: n_years){
    if(harmful_frost_event[i] != 0 & n_protection_stop_example[i] > 0){
      stop_harmful_example[i]<-min(sum(chance_event(share_harmful_of_protection[i], n= n_protection_stop_example[i])), harmful_frost_event[i])} else{
        stop_harmful_example[i]<-0}}
  
  protection_stop_example_list<- vector(mode = "list", length = n_years)
  for (i in 1:n_years){
    if (n_protection_stop_example[i] != 0){
      protection_stop_example_list[[i]]<-c(rep(0, (harmful_frost_event[i]-stop_harmful_example[i])), rep(1, stop_harmful_example[i]))}else{
        protection_stop_example_list[[i]]<-0}}
  protection_stop_example_matrix<-t(sapply(protection_stop_example_list, `length<-`, max(2,max(harmful_frost_event))))
  protection_stop_example_matrix_na_0<-protection_stop_example_matrix %>% replace(is.na(.), 0)
 
  #combine risks
  unsuccessful_protection_example<-matrix(NA, nrow = n_years, ncol=max(2,max(harmful_frost_event)))
  for (i in 1:nrow(unsuccessful_protection_example)) {
    for (j in 1:ncol(unsuccessful_protection_example)) {
      if(risk_1_matrix_na_0[i,j]==1 | risk_2_matrix_na_0[i,j]==1 |protection_stop_example_matrix_na_0 [i,j] == 1){
        unsuccessful_protection_example[i,j]<-1}else {unsuccessful_protection_example[i,j]<-0}
    }
  }
  #sum it up to calculate unsuccessful protection nights
  n_unsuccessful_protection_candles<-rowSums(unsuccessful_protection_example)
  
  