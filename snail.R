library(tidyverse)
library(lme4)
library(emmeans)
library(coefplot)
library(gridExtra)
source("Graphing_Set_Up.R")

snail <- read.csv("snail_survival.csv")
size <- read.csv("snail_size.csv")

## check that counts match between two documents
check <- left_join(snail,size)
check <- check %>% 
  dplyr::select(TankNum,ExptDay, Snail, Snail.Count) %>% 
  filter(!is.na(Snail.Count))

p <- check[which(check$Snail != check$Snail.Count), ]



## snail survival of original cohert
snail <- snail %>% filter(!is.na(Snail))

ggplot(data = snail, aes(ExptDay, Snail)) + geom_point(aes(color=as.factor(TankNum))) + 
  facet_wrap(~treatment)


snail <- snail %>% mutate(proportion = snail$Snail/4,total=4)
## for first pass convert any that were greater than 4 to 4

## want to remove babies from count for this analysis-- 
##where babies were entered here all 4 original survived
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
## going through the data I just don't think it is consistent enough to include
size1 <- size %>% gather(key = "individual", value = "size", -c(Date, TankNum,treatment, ExptDay, Snail.Count, egg.mass, babies))
size2 <- size1[grep("\\[", size1$size, invert = T), ]
size2$size <- as.numeric(as.character(size2$size))
size2 <- size2 %>% filter(treatment %in% c(7,8,11,12))
size2$treatment <- as.character(size2$treatment)
size2$animal <- 0
size2$disturb <- 0

for (i in 1:nrow(size2)){
  if (size2$treatment[i] == 7) {
    size2$animal[i] <- "snail"
    size2$disturb[i] <- "n" } else
      if (size2$treatment[i] == 8) {
        size2$animal[i] <- "snail"
        size2$disturb[i] <- "y"
      } else if (size2$treatment[i] == 11){
        size2$animal[i] <- "daphnia"
        size2$disturb[i] <- "n"
      } else if (size2$treatment[i] == 12){
        size2$animal[i] <- "daphnia"
        size2$disturb[i] <- "y"
      }
}



size3 <- size2 %>% 
  group_by(TankNum,ExptDay,treatment, animal, disturb) %>% 
  filter(!is.na(size)) %>%
  summarize(size = mean(size), size_sd = sd(size) )




## something like this...Mike said since only 3 data points should have exptday not be continuous...
##also should i incorporate variation somehow-
## and deal with the fact that most tanks lost individuals
size_mod <- lmer(data = size4, size~(animal+disturb)*ExptDay + (1|TankNum) )


## eggmass presence
## while I have counts not sure about accuracy because hard to see so going to do presence/absence
## need to check all data sheets again and make sure nothing was overlooked/ all entered
eggm <- size2 %>% group_by(TankNum) %>% filter(ExptDay == max(ExptDay))
eggm <- eggm %>% mutate_each(funs(replace(., is.na(.), FALSE)),egg.mass,babies)
eggm$egg.mass.p <- as.numeric(0)
for (i in 1:nrow(eggm)) {
  if (eggm$egg.mass[i] == 0) {
    eggm$egg.mass.p[i] <- 0
  } else if (eggm$egg.mass[i] > 0) {
    eggm$egg.mass.p[i] <- 1
  }
}

eggmod <- glm(egg.mass.p ~ animal*disturb, family = binomial(link = logit), data = eggm)


eggmod_cs <- update(eggmod, contrasts=list(animal=contr.sum,disturb=contr.sum))


dd <- as.data.frame(emmeans(eggmod,~animal*disturb),type="response")

dd$treat <- paste(dd$animal,dd$disturb)

snail_eggg <- ggplot(data = dd, aes(treat, prob)) + geom_point(aes(color = disturb, shape = animal), size = 3) + geom_errorbar(aes(ymin= asymp.LCL, ymax=asymp.UCL, color= disturb), width = 0.3)+ 
  ylab("Probablity Snail Eggmass Present") + 
  xlab(" ")+
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank()) + scale_color_manual(values = c("black", "seashell4")) + 
  labs(color="Disturbance", shape = "Herbivore") + ggtitle("A.")



print(snail_eggg)

## juvenile count
## so few tanks with individuals

juv <- glm(babies~animal*disturb, family = poisson, data = eggm)
juv_cs <- update(juv, contrasts=list(animal=contr.sum,disturb=contr.sum))
#under/over dispersion of our error distribution by looking at the ratio of Pearson's residuals and the residual degrees of freedom \citep{bolker2008ecological}
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(juv)

dj <- as.data.frame(emmeans(juv,~animal*disturb),type="response")

dj$treat <- paste(dj$animal,dj$disturb)

snail_juv <- ggplot(data = dj, aes(treat, rate)) + geom_point(aes(color = disturb, shape = animal), size = 3) + 
  geom_errorbar(aes(ymin= asymp.LCL, ymax=asymp.UCL, color= disturb), width = 0.3)+ 
  ylab("Juveniles") + 
  xlab("Herbivore Treatment")+
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank()) + scale_color_manual(values = c("black", "seashell4")) + 
  labs(color="Disturbance", shape = "Herbivore") + ggtitle("B.")



print(snail_juv)

grid.arrange(snail_eggg, snail_juv)






