library(tidyverse)
library(survival)
library(survminer)
library(lme4)

snail <- read.csv("snail_survival.csv")

snail <- snail %>% filter(!is.na(Snail))

## snail survival


ggplot(data = snail, aes(ExptDay, Snail)) + geom_point(aes(color=as.factor(TankNum))) + facet_wrap(~treatment)


snail <- snail %>% mutate(proportion = snail$Snail/4)

## for first pass convert any that were greater than 4 to 4

for(i in 1:nrow(snail)){
  if (snail$proportion[i] > 1) {
    snail$proportion[i] <- 1
  }
}

snail$treatment <- as.character(snail$treatment)

snail$expt2 <- snail$ExptDay/39
mod <- glmer(data = snail, proportion ~ ExptDay+treatment+(1|TankNum), family = "binomial")


##snail growth
