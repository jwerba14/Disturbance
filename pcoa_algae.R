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
    labs(shape="Disturbance", color = "Grazer")+
    guides(shape = guide_legend(order = 1)))

grid.arrange(g1,g2,g3)




