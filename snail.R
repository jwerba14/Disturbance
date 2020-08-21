library(tidyverse)
library(lme4)
library(emmeans)
library(coefplot)
library(gridExtra)
source("Graphing_Set_Up.R")
source("data_set_up.R")
snail <- read.csv("snail_survival.csv")
dim(snail)
size <- read.csv("snail_size.csv")

## check that counts match between two documents
check <- left_join(snail,size)
check <- check %>% 
  dplyr::select(TankNum,ExptDay, Snail, Snail.Count) %>% 
  filter(!is.na(Snail.Count))

p <- check[which(check$Snail != check$Snail.Count), ]




## snail survival of original cohert
snail <- snail %>% filter(!is.na(Snail))

#ggplot(data = snail, aes(ExptDay, Snail)) + geom_point(aes(color=as.factor(TankNum))) + 
 # facet_wrap(~treatment)


snail <- snail %>% mutate(proportion = snail$Snail/4,total=4)
##convert any that were greater than 4 to 4--to follow original cohort only

## want to remove babies from count for this analysis-- 
##where babies were entered here ALL 4 original survived  ## needs to be doubled checked in data-- to see if true
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
dim(snail2)

## add in block (starting date)
snail3 <- left_join(snail2, block)
snail3$block <- as.factor(snail3$block)
dim(snail3)
mod <- glmer(data = snail3, cbind(Snail,died) ~ -1 + ExptDay*(animal+disturb)+(1|TankNum)+(1|block), family = binomial(link = "logit"))
plot(resid(mod))
qqnorm(resid(mod))
### plot
newdat <- expand.grid(
  proportion = 0,
  died = 0,
  ExptDay = seq(1,40,1),
  animal = c("snail", "daphnia"),
  disturb = c("y", "n"),
  TankNum = unique(snail$TankNum),
  block = unique(snail3$block)
  
)

newdat$proportion <- predict(mod, newdata = newdat, type = "response")

snail4 <- snail3 %>% group_by(animal, disturb, ExptDay) %>% 
  summarize(prop = mean(proportion), upr = quantile(proportion, 0.975), 
            lwr = quantile(proportion, 0.025))

newdat2 <- newdat %>% group_by(animal, disturb, ExptDay) %>%
  summarize(prop = mean(proportion), upper = quantile(proportion, 0.975),
            lower = quantile(proportion, 0.025))

snail4$treat <- paste(snail4$animal, snail4$disturb)
newdat2$treat <- paste(newdat2$animal, newdat2$disturb)
snail_pop <- (ggplot(data = snail4, aes(x=ExptDay, y=prop))  + geom_point(aes(color=treat, shape=disturb), size = 2)
  +geom_errorbar(aes(ymin = lwr, ymax=upr, color = treat), width = 0.3)
  + geom_line(data= newdat2, aes(color = treat,linetype = disturb),size = 2) 
  + geom_ribbon(data = newdat2, aes(ymin = lower, ymax = upper, color = treat, fill = treat), alpha = 0.15, show.legend = FALSE) 
  + xlab("Day") + ylab("Proportion Surviving")
  + scale_color_discrete(name = "Treatment",
                         labels = str_wrap(c("Daphnia and Physa Not Disturbed", 
                                             "Daphnia and Physa Disturbed", "Physa Not Disturbed", "Physa Disturbed"),
                                           width = 10))
  + scale_linetype_discrete(guide = FALSE) + scale_shape_discrete(guide = FALSE) + 
  scale_fill_discrete(guide = FALSE)+
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank()) + 
  theme(legend.position = c(0.15,0.25), legend.direction = "vertical",
        legend.box.background = element_rect(colour = "black", fill = FALSE), legend.box = "vertical", legend.background = element_blank()
        )
)

print(snail_pop)
  

pdf(file = "snail_survival.pdf", width = 5.1, height = 5)
snail_pop
dev.off()

##snail growth
## going through the data I just don't think it was entered consistently enough to include
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


size_start <- size3 %>% ungroup()%>% filter(ExptDay == 1) %>% summarize(mean=mean(size), sd=sd(size))

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

dim(eggm)
eggm1 <- left_join(eggm, block)
dim(eggm1)
eggmod <- glmer(egg.mass.p ~ animal*disturb+(1|block), family = binomial(link = logit), data = eggm1)

## hmm don't quite understand scale of estimates....
dd <- as.data.frame(emmeans(eggmod,~animal*disturb),type="response")

dd$treat <- paste(dd$animal,dd$disturb)

snail_eggg <- ggplot(data = dd, aes(treat, prob)) + geom_point( size = 4) +
  geom_errorbar(aes(ymin= asymp.LCL, ymax=asymp.UCL), width = 0.3)+ 
  ylab(str_wrap("Probablity Eggmass Present", width = 20)) + 
  xlab(" ")+
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank()) + scale_color_manual(values = c("black", "seashell4")) + 
  labs(color="Disturbance", shape = "Herbivore") + theme(legend.position = "none") +
  scale_x_discrete(labels = str_wrap(c("Daphnia and Physa Not Disturbed",'Daphnia and Physa Disturbed','Physa Not Disturbed', 'Physa Disturbed'), width= 10))



print(snail_eggg)

pdf(file = "snail_eggmass_prob.pdf", width = 5, height = 4)
snail_eggg
dev.off()

## graph difference of differences
nd <- (contrast(regrid(emmeans(eggmod, ~animal*disturb)), interaction = "pairwise", by = "animal"))
nd1 <- data.frame(confint(nd))


snail_egg_diff <- ggplot(data = nd1, aes(estimate, animal)) + geom_point(size = 4) +
  geom_errorbarh(aes(xmin= asymp.LCL, xmax=asymp.UCL), height = 0.3)+ 
  xlab(str_wrap("Difference (logit scale) Snail Eggmass Present", width = 27)) + 
  ylab(" ")+
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank()) + 
    scale_y_discrete(labels = str_wrap(c('Daphnia and Physa','Physa Only'), width= 10))

print(snail_egg_diff)


pdf(file = "diff_eggmass.pdf", width = 5, height = 4)
snail_egg_diff
dev.off()



## juvenile count
## so few tanks with individuals

juv <- glm(babies~animal*disturb, family = poisson, data = eggm1) ## hmm can't currently add block
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

##hmm maybe because so few juveniles just want to show there are few so forget the model and just 
# graph raw data

eggm2 <- eggm1 %>% group_by(animal, disturb) %>% summarize(juv = mean(babies), 
                                                           lwr = quantile(babies, 0.025),
                                                           upr = quantile(babies, 0.975))

eggm2$treat <- paste(eggm2$animal,eggm2$disturb)
juv_snail_g <- ggplot(data = eggm2, aes(treat, juv)) + 
  geom_point(size= 3, aes(color=disturb, shape = animal))+
  geom_errorbar(aes(color=disturb, ymax=upr, ymin=lwr))+
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank()) +
  scale_color_manual(values = c("black", "seashell4")) + 
  labs(color="Disturbance", shape = "Herbivore") + ggtitle("D.") + 
  ylab(str_wrap("Juvenile Count", width = 10))+
  xlab(" ") + theme(legend.position = "none")

print(juv_snail_g)

gl3 <- list(snail_pop,snail_egg_diff,snail_eggg, juv_snail_g)
grid.arrange(
  grobs = gl3,
  widths = c(1, 1, 1,1),
  layout_matrix = rbind(c(1, 1, 2,2),
                        c(1, 1,3,3),
                        c(1,1,4,4))
)






