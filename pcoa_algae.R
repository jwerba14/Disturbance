###Data Set
algae_all <- read.csv("algae_all.csv")
source("fix_chl.R")
source("Graphing_Set_Up.R")
### additional packages 
library(vegan)
library(gridExtra)
library(ggplot2)

dat <- newdat %>% filter(! treatment %in% c(5,6,9,10,13,14))

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

## add treatment column to algae
treat <- dat %>% dplyr::select(animal,disturb, TankNum)
algal <- left_join(treat, algae_all)


###Make Three Data Frames For Rounds 1, 2, and 3
algal_day1 <- algal %>% filter(Round == 1)
algal_day2 <- algal %>% filter(Round == 2)
algal_day3 <- algal %>% filter(Round == 3)


### determine percent of each algal species by day
algal_day1_per <- algal_day1
algal_day1_per$total <- rowSums(algal_day1_per[,-c(1:4)])

algal_day1_per[, -c(1:4, ncol(algal_day1_per))] <- 
  algal_day1_per[, -c(1:4, ncol(algal_day1_per))] / algal_day1_per$total

summ_per_1 <- algal_day1_per %>% 
  group_by(animal, disturb) %>% 
  summarise_all(funs(mean)) %>% dplyr::select(-c(TankNum,Round))

summ_per_1 <- summ_per_1 %>% gather(key = "species", value = "percent", -c(animal,disturb))


summ_per_1_sd <- algal_day1_per %>% 
  group_by(animal, disturb) %>% 
  summarise_all(funs(sd)) %>% dplyr::select(-c(TankNum))
summ_per_1_sd <- summ_per_1_sd %>% gather(key = "species", value = "sd", -c(animal,disturb))
summ_per_1 <- left_join(summ_per_1, summ_per_1_sd)
summ_per_1 <- summ_per_1 %>% filter(percent > 0.05, species != "total")


(per1 <- ggplot(data = summ_per_1, aes(species,percent, color = animal, shape = disturb)) +
    geom_pointrange(aes(ymin = percent-sd, ymax = percent+sd), 
                    position=position_jitter(width=0.5)) + 
   xlab("Species") +
  ylab("Percent") + ggtitle("A. Starting Conditions")+
  theme(legend.position = "none")+ 
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank()))
 
  
## day 2
algal_day2_per <- algal_day2
algal_day2_per$total <- rowSums(algal_day2_per[,-c(1:4)])

algal_day2_per[, -c(1:4, ncol(algal_day2_per))] <- 
  algal_day2_per[, -c(1:4, ncol(algal_day2_per))] / algal_day2_per$total

summ_per_2 <- algal_day2_per %>% 
  group_by(animal, disturb) %>% 
  summarise_all(funs(mean)) %>% dplyr::select(-c(TankNum,Round))

summ_per_2 <- summ_per_2 %>% gather(key = "species", value = "percent", -c(animal,disturb))


summ_per_2_sd <- algal_day2_per %>% 
  group_by(animal, disturb) %>% 
  summarise_all(funs(sd)) %>% dplyr::select(-c(TankNum))
summ_per_2_sd <- summ_per_2_sd %>% gather(key = "species", value = "sd", -c(animal,disturb))
summ_per_2 <- left_join(summ_per_2, summ_per_2_sd)
summ_per_2 <- summ_per_2 %>% filter(percent > 0.05, species != "total")

(per2 <- ggplot(data = summ_per_2, aes(species,percent, color = animal, shape = disturb)) +
    geom_pointrange(aes(ymin = percent-sd, ymax = percent+sd), 
                    position=position_jitter(width=0.5)) + 
    xlab("Species") +
    ylab("Percent") + ggtitle("B. Mid-Point")+
    theme(legend.position = "none")+ 
    theme(axis.line = element_line(colour = "black"), panel.border = element_blank()))


## day 3
algal_day3_per <- algal_day3
algal_day3_per$total <- rowSums(algal_day3_per[,-c(1:4)])

algal_day3_per[, -c(1:4, ncol(algal_day3_per))] <- 
  algal_day3_per[, -c(1:4, ncol(algal_day3_per))] / algal_day3_per$total

summ_per_3 <- algal_day3_per %>% 
  group_by(animal, disturb) %>% 
  summarise_all(funs(mean)) %>% dplyr::select(-c(TankNum,Round))

summ_per_3 <- summ_per_3 %>% gather(key = "species", value = "percent", -c(animal,disturb))
summ_per_3 <- summ_per_3 %>% filter(percent > 0.05, species != "total")

summ_per_3_sd <- algal_day3_per %>% 
  group_by(animal, disturb) %>% 
  summarise_all(funs(sd)) %>% dplyr::select(-c(TankNum))
summ_per_3_sd <- summ_per_3_sd %>% gather(key = "species", value = "sd", -c(animal,disturb))
summ_per_3_sd <- summ_per_3_sd %>% filter(sd > 0.05, species != "total")
summ_per_3 <- left_join(summ_per_3, summ_per_3_sd)

(per3 <- ggplot(data = summ_per_3, aes(species,percent, color = animal, shape = disturb)) +
    geom_pointrange(aes(ymin = percent-sd, ymax = percent+sd), 
                    position=position_jitter(width=0.5)) + 
    xlab("Species") +
    ylab("Percent") + ggtitle("C. Final Conditions")+
    theme(legend.box="vertical", legend.position = c(0.8,0.95),legend.direction = "horizontal",
          legend.background = element_rect(fill = "transparent"))+
    labs(shape="Disturbance", color = "Herbivore")+
    theme(axis.line = element_line(colour = "black"), panel.border = element_blank()))
  
grid.arrange(per1, per2, per3)

###Remove All Rows That Has a Sum of 0 - need id dataframe and community dataframe
algal_day1_id <- algal_day1 %>% 
  filter(rowSums(algal_day1[,-c(1:4)]) > 0) %>%
  dplyr::select(animal,disturb,TankNum,Round) 
algal_day1_comm <- algal_day1 %>% 
  filter(rowSums(algal_day1[,-c(1:4)]) > 0) %>%
  dplyr::select(-c(animal,disturb,TankNum,Round) )
 

algal_day2_id <- algal_day2 %>% 
  filter(rowSums(algal_day2[,-c(1:4)]) > 0) %>%
  dplyr::select(animal,disturb,TankNum,Round) 
algal_day2_comm <- algal_day2 %>% 
  filter(rowSums(algal_day2[,-c(1:4)]) > 0) %>%
  dplyr::select(-c(animal,disturb,TankNum,Round) )


algal_day3_id <- algal_day3 %>% 
  filter(rowSums(algal_day3[,-c(1:4)]) > 0) %>%
  dplyr::select(animal,disturb,TankNum,Round)
algal_day3_comm <- algal_day3 %>% 
  filter(rowSums(algal_day3[,-c(1:4)]) > 0) %>%
  dplyr::select(-c(animal,disturb,TankNum,Round) )


  


###Remove All Columns That Has A Sum Of 0
algal_day1_comm <- algal_day1_comm[ , colSums(algal_day1_comm) > 0]

algal_day2_comm <- algal_day2_comm[ , colSums(algal_day2_comm) > 0]

algal_day3_comm <- algal_day3_comm[ , colSums(algal_day3_comm) > 0]

###Make Distance Matrix- Using Bray-Curtis
algal1_dist <- vegdist(algal_day1_comm, method = "bray")

algal2_dist <- vegdist(algal_day2_comm, method = "bray")

algal3_dist <- vegdist(algal_day3_comm, method = "bray")

###PCOA
day1_pcoa <- cmdscale(algal1_dist, k=3, eig = TRUE, add = FALSE)

day2_pcoa <- cmdscale(algal2_dist, k=3, eig = TRUE, add = FALSE)

day3_pcoa <- cmdscale(algal3_dist, k=3, eig = TRUE, add = FALSE)

###calculate Expected Variation From The First 2 Axes
(expvar1_day1 <- round(day1_pcoa$eig[1] / sum(day1_pcoa$eig), 3) * 100) # 63.5
(expvar2_day1 <- round(day1_pcoa$eig[2] / sum(day1_pcoa$eig), 3) * 100) # 17.7

(expvar1_day2 <- round(day2_pcoa$eig[1] / sum(day2_pcoa$eig), 3) * 100) # 45.5
(expvar2_day2 <- round(day2_pcoa$eig[2] / sum(day2_pcoa$eig), 3) * 100) # 17.3

(expvar1_day3 <- round(day3_pcoa$eig[1] / sum(day3_pcoa$eig), 3) * 100) # 37.7
(expvar2_day3 <- round(day3_pcoa$eig[2] / sum(day3_pcoa$eig), 3) * 100) #19.6



###GRAPHING PCOA
#Data Frame With PCOA Points We Need 
graph_dat_start <- data.frame(
  point1 = day1_pcoa$points[,1],
  point2 = day1_pcoa$points[,2],
  animal = algal_day1_id$animal,
  disturb = algal_day1_id$disturb) ## needs to be subset for the same tanks that we actually have data from
  

graph_dat_midpt <- data.frame(
  point1 = day2_pcoa$points[,1],
  point2 = day2_pcoa$points[,2],
  animal = algal_day2_id$animal,
  disturb = algal_day2_id$disturb)
  

graph_dat_end <- data.frame(
  point1 = day3_pcoa$points[,1],
  point2 = day3_pcoa$points[,2],
  animal = algal_day3_id$animal,
  disturb = algal_day3_id$disturb)
  

###Summarize The Data In Order To Graph 
graph_dat_start_sum <- graph_dat_start %>% 
  group_by(animal,disturb) %>%
  summarise(Axis1 = mean(point1), sd1 = sd(point1), Axis2 = mean(point2),
            sd2=sd(point2))

graph_dat_midpt_sum <- graph_dat_midpt %>% 
  group_by(animal,disturb) %>%
  summarise(Axis1 = mean(point1), sd1 = sd(point1), Axis2 = mean(point2),
            sd2=sd(point2))

graph_dat_end_sum <- graph_dat_end %>% 
  group_by(animal,disturb) %>%
  summarise(Axis1 = mean(point1), sd1 = sd(point1), Axis2 = mean(point2),
            sd2=sd(point2))


###Graphing 
# Start
(g1 <- ggplot(data = graph_dat_start_sum, aes(Axis1,Axis2)) + 
    geom_point(aes(color = animal, shape = disturb), size = 5) +
    geom_errorbar(aes(ymax= Axis2+sd2, ymin= Axis2-sd2)) + 
    geom_errorbarh(aes(xmax=Axis1+sd1, xmin=Axis1-sd1)) + xlab("Axis 1 (63%)") +
    ylab("Axis 2 (18%)") + ggtitle("A. Starting Conditions")+
    theme(legend.position = "none")+ scale_x_continuous(limits = c(-.75, 0.75))+
    theme(axis.line = element_line(colour = "black"), panel.border = element_blank()))

# Midpoint 
(g2 <- ggplot(data = graph_dat_midpt_sum, aes(Axis1,Axis2)) + 
    geom_point(aes(color = animal, shape = disturb), size = 5)  +
    geom_errorbar(aes(ymax= Axis2+sd2, ymin= Axis2-sd2)) + 
    geom_errorbarh(aes(xmax=Axis1+sd1, xmin=Axis1-sd1)) + xlab("Axis 1 (45%)") +
    ylab("Axis 2 (17%)")+ ggtitle("B. Mid-Point")+ 
    theme(axis.line = element_line(colour = "black"), panel.border = element_blank()) +
    theme(legend.position = "none")+scale_x_continuous(limits = c(-.75, 0.75)))


# Final day
(g3 <- ggplot(data = graph_dat_end_sum, aes(Axis1,Axis2)) + 
    geom_point(aes(color = animal, shape = disturb), size = 5)  +
    geom_errorbar(aes(ymax= Axis2+sd2, ymin= Axis2-sd2)) + 
    geom_errorbarh(aes(xmax=Axis1+sd1, xmin=Axis1-sd1)) + xlab("Axis 1 (38%)") +
    ylab("Axis 2 (17%)") + ggtitle("C. Final Day") + scale_x_continuous(limits = c(-.75, 0.75))+
    theme(axis.line = element_line(colour = "black"), panel.border = element_blank())+
    theme(legend.position=c(0.9,0.9), legend.direction = "vertical",
          legend.background = element_rect(fill = "transparent"), legend.box="horizontal") +
    labs(shape="Disturbance", color = "Herbivore")+
    guides(shape = guide_legend(order = 1)))

grid.arrange(g1,g2,g3)

#run PERMANOVA


ad <- adonis(algal[,-c(1:4)] ~ animal+disturb+Round, method = "bray",
                data=algal, perm=1000,parallel = getOption("mc.cores"))

saveRDS(ad, file = "permanova.RDS")
