setwd("C://Users//19452//Desktop//Big Data Project//")

library(ggplot2)
library(dplyr)
library(tidyverse)
library(forcats)

rf <- read.csv("riskfactor.csv")
location <- read.csv("geolocation.csv")
trucks <- read.csv("trucks.csv")
truck_mileage <- read.csv("truck_mileage.csv")
driver_mileage <- read.csv("drivermileage.csv")
trucks_mg <- read.csv("trucks_mg.csv")

rf_model <- merge(x=rf,y=trucks[ , c("model","driverid")],by.x = c('riskfactor.driverid'), by.y = c('driverid'),all.x = TRUE)

total_mpg <- truck_mileage %>% group_by(truck_mileage.driverid) %>% summarise(totmiles = sum(truck_mileage.miles),
                                                            totgas = sum(truck_mileage.gas))

rf_model <- merge(x=rf_model,y=total_mpg,by.x = c('riskfactor.driverid'), by.y = c('truck_mileage.driverid'),all.x = TRUE)

rf_model = subset(rf_model, select = -c(riskfactor.driverid,totmiles,totgas) )
rf.lm <- lm(riskfactor.riskfactor~ ., data = rf_model)
summary(rf.lm)

rf_model$mpg <- rf_model$riskfactor.totmiles/rf_model$totgas

library(ggplot2)

# Basic scatter plot
ggplot(rf_model, aes(x=riskfactor.totmiles, y=riskfactor.riskfactor)) + geom_point() 
ggplot(rf_model, aes(x=riskfactor.events, y=riskfactor.riskfactor)) + geom_point() 
ggplot(rf_model, aes(x=mpg, y=riskfactor.riskfactor)) + geom_point() 
