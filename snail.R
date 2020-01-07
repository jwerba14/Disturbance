library(tidyverse)
library(lme4)
library(emmeans)
library(coefplot)
source("Graphing_Set_Up.R")

snail <- read.csv("snail_survival.csv")

snail <- snail %>% filter(!is.na(Snail))

## snail survival


ggplot(data = snail, aes(ExptDay, Snail)) + geom_point(aes(color=as.factor(TankNum))) + facet_wrap(~treatment)


snail <- snail %>% mutate(proportion = snail$Snail/4,total=4)
## for first pass convert any that were greater than 4 to 4

for(i in 1:nrow(snail)){
  if (snail$proportion[i] > 1) {
    snail$proportion[i] <- 1
  }
}

snail$treatment <- as.character(snail$treatment)
snail$animal <- 0
snail$disturb <- 0

for (i in 1:nrow(snail)){
  if (snail$treatment[i] == 7) {
    snail$animal[i] <- "snail"
    snail$disturb[i] <- "n" } else
      if (snail$treatment[i] == 8) {
        snail$animal[i] <- "snail"
        snail$disturb[i] <- "y"
      } else if (snail$treatment[i] == 11){
        snail$animal[i] <- "daphnia"
        snail$disturb[i] <- "n"
      } else if (snail$treatment[i] == 12){
        snail$animal[i] <- "daphnia"
        snail$disturb[i] <- "y"
      }
  }



#snail <- snail %>% group_by(TankNum) %>% mutate(maxd = max(ExptDay))
#snail <- snail %>% mutate(expt2 = ExptDay/maxd)

snail2 <- snail %>% mutate(proportion = Snail/4, total=4, died=total-Snail) %>% filter(proportion<=1)
mod <- glmer(data = snail2, cbind(Snail,died) ~ ExptDay*(animal+disturb)+(1|TankNum), family = binomial(link = "logit"))


### plot
newdat <- expand.grid(
  proportion = 0,
  died = 0,
  ExptDay = seq(1,40,1),
  animal = c("snail", "daphnia"),
  disturb = c("y", "n"),
  TankNum = unique(snail$TankNum)
  
)

newdat$proportion <- predict(mod, newdata = newdat, type = "response")

snail3 <- snail2 %>% group_by(animal, disturb, ExptDay) %>% 
  summarize(prop = mean(proportion), sd = sd(proportion))

newdat2 <- newdat %>% group_by(animal, disturb, ExptDay) %>%
  summarize(prop = mean(proportion), upper = quantile(proportion, 0.975),
            lower = quantile(proportion, 0.025))

snail_pop <- (ggplot(data = snail3, aes(x=ExptDay, y=prop,color=animal,lty=disturb, shape = disturb)) 
  
  + geom_point()
  +geom_errorbar(aes(ymin = prop-sd, ymax=prop+sd), width = 0.3)
  + geom_line(data= newdat2, aes(color = animal,linetype = disturb)) 
  + geom_ribbon(data = newdat2, aes(ymin = lower, ymax = upper), alpha = 0.15) 
  + xlab("Day") + ylab("Proportion Surviving")
  + scale_color_discrete(name = "Herbivore Treatment", labels = c("Both", "Snail")) 
  + scale_linetype_discrete(name = "Disurbance") + scale_shape_discrete(name = "Disurbance")
)

print(snail_pop)
  

##snail growth


