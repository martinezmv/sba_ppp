library(tidyverse)
library(data.table)

SBA <- read_csv("./data/150K_slim.csv")

SBA%>%select(Lender)%>%distinct(Lender)%>%rename(sbaLender=Lender)->sba_banks
#Obtained list of OCC regulated banks from public OCC site
#https://www.occ.treas.gov/topics/charters-and-licensing/financial-institution-lists/index-financial-institution-lists.html
nat_banks<-read_csv("./data/nat_banks_occ_public.csv")
nat_banks%>%select(Lender, cert)%>%rename(natLender=Lender)->nat_names

# Creating an empty column
nat_banks$sbaLender <- ""   

#generate matches
for(i in 1:dim(nat_banks)[1]) 
{   x <- agrep(nat_names$natLender[i], sba_banks$sbaLender,             
               ignore.case=TRUE, value=TRUE,               
               max.distance = 0.01, useBytes = TRUE)   
  x <- paste0(x,"")   
  nat_banks$sbaLender[i] <- x }

write.csv(nat_banks, "D:/Monica/Documents/2020/SBA/sba-loan-analysis/R/data/matched_banks.csv" )


