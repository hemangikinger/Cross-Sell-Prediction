---
title: "Data Science - Mid-term Project"
author: "T3 - Linear Boost"
date: "`r Sys.Date()`"
output:
  html_document:
  code_folding: hide
number_sections: true
toc: yes
toc_depth: 3
toc_float: yes
pdf_document:
  toc: yes
toc_depth: '3'
---
  

# The package "ezids" (EZ Intro to Data Science) includes a lot of the helper functions we developed for the course. 
# Some of the frequently used functions are loadPkg(), xkabledply(), xkablesummary(), uzscale(), etc.
# Once installed, load the library.
library(ezids)
library(tidyverse)


vehicle <- read.csv("train.csv")

veh <- vehicle

veh$Gender <- factor(vehicle$Gender)
veh$Driving_License <- factor(vehicle$Driving_License)
veh$Previously_Insured <- factor(vehicle$Previously_Insured)
veh$Vehicle_Damage <- factor(vehicle$Vehicle_Damage)
veh$Response <- factor(vehicle$Response)

veh$Vehicle_Age <- ifelse(vehicle$Vehicle_Age == '> 2 Years', 2,ifelse(vehicle$Vehicle_Age == '1-2 Year', 1, 0))

veh$Vehicle_Age <- factor(veh$Vehicle_Age)

################################################################################

ct <- table(veh$Gender,veh$Response)

xkabledply(ct, title="Contingency table for Gender vs Response")

paste0("Alpha value is set as 0.05 and p -value from Pearson's test is: ", chisq.test(ct)$p.value)

paste0(ifelse(chisq.test(ct)$p.value< 0.05, "Gender is not independent of response","Gender is independent of response"))

################################################################################

ct <- table(veh$Driving_License,veh$Response)

xkabledply(ct, title="Contingency table for Driving License vs Response")

paste0("Alpha value is set as 0.05 and p -value from Pearson's test is: ", chisq.test(ct)$p.value)

paste0(ifelse(chisq.test(ct)$p.value< 0.05, "Driving License is not independent of response","Driving License is independent of response"))

################################################################################

ct <- table(veh$Previously_Insured,veh$Response)

xkabledply(ct, title="Contingency table for Previously Insured vs Response")

paste0("Alpha value is set as 0.05 and p -value from Pearson's test is: ", chisq.test(ct)$p.value)

paste0(ifelse(chisq.test(ct)$p.value< 0.05, "Previously Insured is not independent of response","Previously Insured is independent of response"))

################################################################################

ct <- table(veh$Vehicle_Age,veh$Response)

xkabledply(ct, title="Contingency table for Vehicle Age vs Response")

paste0("Alpha value is set as 0.05 and p -value from Pearson's test is: ", chisq.test(ct)$p.value)

paste0(ifelse(chisq.test(ct)$p.value< 0.05, "Vehicle Age is not independent of response","Vehicle Age is independent of response"))

################################################################################

ct <- table(veh$Vehicle_Damage,veh$Response)

xkabledply(ct, title="Contingency table for Vehicle Damage vs Response")

paste0("Alpha value is set as 0.05 and p -value from Pearson's test is: ", chisq.test(ct)$p.value)

paste0(ifelse(chisq.test(ct)$p.value< 0.05, "Vehicle Damage is not independent of response","Vehicle Damage is independent of response"))

################################################################################



