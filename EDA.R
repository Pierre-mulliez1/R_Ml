#Author: Pierre Mulliez,
#Last edited: 17/06/2021
#Project: Covid prediction

######LOAD AND INSPECT FILES######
library(ggplot2)
library(data.table)  
library(fBasics)
library(dplyr)
library(leaflet)

getwd()
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

path_train = "data/train.csv"
path_location = "data/country_location.xlsx"
data = read.csv(path_train)
dat2 = read_excel(path_location)
dat2 = data.frame(dat2)
tail(data)
summary(data)


#######Visualisation#######


"different countries :"
unique(data["Country_Region"])
"Number of different countries: "
count(unique(data["Country_Region"]))

"Lets check the global pandemic trends: "
global_covid_f <- data.frame(data) %>%  group_by(Date) %>% summarise(Fatalities = sum(Fatalities))
global_covid_c <- data.frame(data) %>%  group_by(Date) %>% summarise(Cases = sum(ConfirmedCases))
#global_covid

ts.plot(global_covid_c)
ts.plot(global_covid_f)



######Mapping the pandemic ######



#hide country spaces in countries 
#benelux <- c("France", "Netherlands")

last_date <- data %>% group_by(Country_Region)%>% summarise(Fatalities = sum(Fatalities),Cases = sum(ConfirmedCases))
#the merge will remove some value - do not use the table in ML
last_date_merged <- merge(x = last_date,y = dat2,by.x = "Country_Region", by.y = "name",all = FALSE)
head(last_date_merged)


last_date_merged$size <- 10000
last_date_merged[last_date_merged$Cases > 5000,]$size <-  80000
last_date_merged[last_date_merged$Cases > 10000,]$size <-  150000

last_date_merged$col <- "blue"
last_date_merged[last_date_merged$Fatalities > 2000,]$col <-  "darkblue"
last_date_merged[last_date_merged$Fatalities > 10000,]$col <-  "red"



last_date_merged$countryC <- paste("country", ", Cases: " ,toString(0))
for (country in last_date_merged$Country_Region){
  (last_date_merged[last_date_merged$Country_Region == country,]$countryC
   <- paste(country, ", Cases: " ,toString(last_date_merged[last_date_merged$Country_Region == country,]$Cases)))
}

last_date_merged


cov_map <- (leaflet(data = last_date_merged) %>% addTiles() 
            %>% addCircles( lng = ~longitude, lat = ~latitude,popup = ~countryC, color = ~col, radius = ~size,fill = F))
cov_map <-  addLegend(cov_map,"bottomright", 
                  labels = c("<2000","2000+","10000+"), 
                  colors = c("blue","darkblue","red"),
                  title = "Fatalities",
                  opacity = 1) 
cov_map

