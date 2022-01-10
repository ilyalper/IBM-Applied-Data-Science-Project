# Stat 564 Project 3 Group R-evolution
# Dilay Özkan & Alper Şener

#Important Notices: Please update the working directory in 14th row.
#Please run this script before running Covid_Shiny.R 
#Since we store our data in a relatively big data frame, it gives results in 4 to 5 minutes

rm(list=ls())

library(readr)
library(gapminder)
library(ggplot2)

setwd(getwd())
CovidData <- read_csv("owid-covid-data.csv")



listofcnt = unique(CovidData$location) #returns list of countries

uniq_dates = unique(CovidData$date); #returns list of dates

#We are storing the covid data for each country from start_date to end_date

bas=0;son=0;
start_date = as.Date("2020-03-01")
end_date = as.Date("2020-06-01")
uniq_dates_sorted = sort(uniq_dates);

for (i in 1:length(uniq_dates_sorted)) {
  if (uniq_dates_sorted[i]==start_date) { 
    bas=i;  }
  if (uniq_dates_sorted[i]==end_date) { 
    son=i;  }
}

inv_date <- vector()  # storing investigated dates
class(inv_date)="Date"
inv_date[1]=uniq_dates_sorted[bas-1];
for (i in bas:son) {
    inv_date[i-bas+2]=uniq_dates_sorted[i]
} 

# Creating the first line of the data frame

cov.data = data.frame(
  country = c ("Aruba"), 
  invest_date = inv_date[1],
  totalCases = 0, 
  newCases = 0,
  totalDeaths =0,
  newDeaths =0,
  totalCasesPerMillion =0,
  totalDeathsPerMillion =0
)
# Get the structure of the data frame.

for (i in 1:(length(listofcnt)-1)) { #last country "international" includes irrelevant data that is why we exclude last cty
  for (j in 1:length(inv_date)) {
    k=0;
    if (j==1&i==1) {k=1}
    
  #     we will not use the first rows for the countries in plotting, but we created one initial empty row to assign 
  # values to columns that include no data for the investigated day 
    if (j == 1 ) { cov.newrow = data.frame(
      country = listofcnt[i], 
      invest_date = inv_date[j],
      totalCases = 0, 
      newCases = 0,
      totalDeaths =0,
      newDeaths =0,
      totalCasesPerMillion =0,
      totalDeathsPerMillion =0
    )}
    else{ 
      #if there is no data for a day,we assume that an announcement is redundant and there is no update on COVID data, 
      if (length(CovidData[CovidData$location==listofcnt[i] & CovidData$date == inv_date[j],4]$total_cases) == 0){
        toplamVaka = cov.data[cov.data$country==listofcnt[i] & cov.data$invest_date == inv_date[j-1],3]
      } else{toplamVaka=CovidData[CovidData$location==listofcnt[i] & CovidData$date == inv_date[j],4]$total_cases }
      
      if (length(CovidData[CovidData$location==listofcnt[i] & CovidData$date == inv_date[j],5]$new_cases) == 0){
        yeniVaka = cov.data[cov.data$country==listofcnt[i] & cov.data$invest_date == inv_date[j-1],4]
      } else{yeniVaka=CovidData[CovidData$location==listofcnt[i] & CovidData$date == inv_date[j],5]$new_cases }
      
      if (length(CovidData[CovidData$location==listofcnt[i] & CovidData$date == inv_date[j],6]$total_deaths) == 0){
        toplamOlum = cov.data[cov.data$country==listofcnt[i] & cov.data$invest_date == inv_date[j-1],5]
      } else{toplamOlum=CovidData[CovidData$location==listofcnt[i] & CovidData$date == inv_date[j],6]$total_deaths }
      
      if (length(CovidData[CovidData$location==listofcnt[i] & CovidData$date == inv_date[j],7]$new_deaths) == 0){
        yeniOlum = cov.data[cov.data$country==listofcnt[i] & cov.data$invest_date == inv_date[j-1],6]
      } else{yeniOlum= CovidData[CovidData$location==listofcnt[i] & CovidData$date == inv_date[j],7]$new_deaths}
      
      if (length(CovidData[CovidData$location==listofcnt[i] & CovidData$date == inv_date[j],8]$total_cases_per_million) == 0){
        milyondaVaka = cov.data[cov.data$country==listofcnt[i] & cov.data$invest_date == inv_date[j-1],7]
      } else{milyondaVaka= CovidData[CovidData$location==listofcnt[i] 
                                     & CovidData$date == inv_date[j],8]$total_cases_per_million}
      
      if (length(CovidData[CovidData$location==listofcnt[i] & CovidData$date == inv_date[j],10]$total_deaths_per_million) == 0){
        milyondaOlum = cov.data[cov.data$country==listofcnt[i] & cov.data$invest_date == inv_date[j-1],8]
      } else{milyondaOlum= CovidData[CovidData$location==listofcnt[i] 
                                     & CovidData$date == inv_date[j],10]$total_deaths_per_million}
      
      
      #building the new row
      cov.newrow = data.frame(
      country = listofcnt[i], 
      invest_date = inv_date[j],
      totalCases = toplamVaka,
      newCases = yeniVaka,
      totalDeaths = toplamOlum,
      newDeaths = yeniOlum,
      totalCasesPerMillion = milyondaVaka,
      totalDeathsPerMillion = milyondaOlum
    )}
   
    k=0;
    if (j==1&i==1) {k=1} #Since we create the first row of the data frame by hand, we exclude j&i=1 in the loop
    if (k==0) {
      cov.data = rbind(cov.data,cov.newrow)
    }
  }
}







