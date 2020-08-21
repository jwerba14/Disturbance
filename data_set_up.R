
source("fix_chl.R")
source("Graphing_Set_Up.R")
library(lme4)
library(tidyquant)
library(emmeans)
library(gridExtra)
library(sjstats)
library(stringr)
library(lmerTest)



### Remove with ceriodaphnia since they didn't survive
dat <- newdat %>% filter(! treatment %in% c(5,6,9,10,13,14))
dim(dat)

##make a columns for disturbed/undisturbed and both/none/cladoceran/snail

dat$disturb <- "X"
  
for (i in 1:nrow(dat)){
  if (dat$treatment[i] ==2| dat$treatment[i] ==4| dat$treatment[i] ==8|
      dat$treatment[i] ==12){
    dat$disturb[i] <- "y"
  } else if (dat$treatment[i] ==1|dat$treatment[i] ==3|dat$treatment[i] ==7|dat$treatment[i] ==11){
    dat$disturb[i] <- "n"
  }}

dat$animal <- "X"

for (i in 1:nrow(dat)){
  if (dat$treatment[i] ==3| dat$treatment[i] ==4){
    dat$animal[i] <- "daphnia"
  } else if (dat$treatment[i] ==7|dat$treatment[i] ==8){
    dat$animal[i] <- "snail"
  } else if (dat$treatment[i] ==11|dat$treatment[i] ==12){
    dat$animal[i] <- "both"}
  else if (dat$treatment[i] ==1|dat$treatment[i] ==2){
    dat$animal[i] <- "none"}
}


# need dummy date variable for tq_mutate to work.... and for random effect of experimental block

Date <- paste(dat$Month, dat$Day,dat$Year, sep = "-")


betterDates <- as.Date(Date,
  format = "%B-%d-%Y")

dates <- data.frame(Dates1 = Date,
                    Dates2 = betterDates)

p <- dates %>% filter(is.na(Dates2))
which(is.na(dates$Dates2))
dates[c(390,400,409,418,421,440),2] <- "2019-03-02"
dates[419,2] <- "2019-01-31"
dates[498,2] <- "2019-02-03"
dates[(which(is.na(dates$Dates2))), 2] <- "2019-03-01"

dat$Date <- dates$Dates2

## add blocking variable
block <- dat %>% filter(ExptDay == 1) %>% dplyr::select(Date, ExptDay, TankNum)
block$block <- NA
for (i in 1:nrow(block)){
  if (block$Date[i] == "2019-01-21" ) {
    block$block[i] <- "A"
  } else if (block$Date[i] == "2019-02-21" ) {
     block$block[i] <- "B"
  } else if (block$Date[i] == "2019-01-22" ) {
     block$block[i] <- "C"
  } else if (block$Date[i] == "2019-01-25" ) {
     block$block[i] <- "D"
  } else if (block$Date[i] == "2019-03-02" ) {
     block$block[i] <- "E"
  } else if (block$Date[i] == "2019-01-28" ) {
     block$block[i] <- "F"  
  } else if (block$Date[i] == "2019-01-30" ) {
     block$block[i] <- "G"  
  } else if (block$Date[i] == "2019-03-01" ) {
     block$block[i] <- "H"   
  } else if (block$Date[i] == "2019-02-02" ) {
     block$block[i] <- "I" 
  } else if (block$Date[i] == "2019-02-04" ) {
     block$block[i] <- "J"  
  } else if (block$Date[i] == "2019-02-05" ) {
     block$block[i] <- "K"   
  } 
}

block <- block %>% dplyr::select(TankNum, block)
dat <- left_join(dat,block, by = "TankNum")
dim(dat)
dat$disturb <- factor(dat$disturb,levels = c("y", "n"))