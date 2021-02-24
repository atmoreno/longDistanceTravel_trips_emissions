pacman::p_load(dplyr,ggplot2,tidyr,reshape,RColorBrewer)


allTable <- data.frame(matrix(ncol = 1, nrow = 1))


for (i in 0:20){
  scenarioName = paste("C:/models/germanymodel-/output/populationSplit_byPurpose/", "p",i,"_summaryModeChoiceEmissions.csv", sep = "")
  summary = read.csv(scenarioName)
  allTable = bind_rows(allTable,summary)
}

colnames(allTable)
allTable = allTable[-1,]

#-----setup trip purpose and state-----
# after the selection, everything is the same for all trip purposes and states

trips = allTable %>%
  select(subpopulation,scenario = scenarioNoSeed,cost,distance,limSpeed,seed, 
         scenarioLabel = scenario.1 , 
         auto = overnight.leisure.auto.trips, 
         air = overnight.leisure.air.trips, 
         rail = overnight.leisure.rail.trips, 
         bus = overnight.leisure.bus.trips)

titleFigure = "Overnight leisure trips"

plotName =  "C:/models/germanymodel-/output/populationSplit_byPurpose/plots/overnight.leisure"


#-------run---------------------------------
summaryTrips = trips %>% group_by(scenario,seed) %>% 
  summarise(auto = sum(auto),
            air = sum(air),
            rail = sum(rail),
            bus = sum(bus))

averageByScenario = summaryTrips %>% group_by(scenario) %>%
  summarise(seed = 0,
            auto = mean(auto),
            air = mean(air),
            rail = mean(rail),
            bus = mean(bus))

scenarioSettings = trips %>%
  select(subpopulation,scenario,cost,distance,limSpeed,seed) 
scenarioSettings = subset(scenarioSettings, subpopulation == 5)
scenarioSettings = scenarioSettings %>%
  select(scenario,cost,distance,limSpeed,seed) 

tripsBySeed = merge(scenarioSettings, summaryTrips)


scenarioSettingsAverage = subset(scenarioSettings, seed == 1)
scenarioSettingsAverage$seed = 0
averageTripsBySeed = merge(scenarioSettingsAverage, averageByScenario)


rm(trips,averageByScenario,scenarioSettings,scenarioSettingsAverage,summaryTrips)

write.csv(tripsBySeed,paste(plotName,".trips.csv",sep = ""), row.names = FALSE)

#--------summary by distance--------------

trips_distance = tripsBySeed %>%
  filter(cost == 1 & limSpeed == 0) %>%
  select(-c(cost,limSpeed))

trips_distance_melt =  melt(trips_distance, id.vars = c("scenario","seed", "distance"))
trips_distance_melt$Mode = factor(trips_distance_melt$variable,levels = c("air","auto","bus","rail"))

averageTripsBySeed_distance = averageTripsBySeed %>%
  filter(cost == 1 & limSpeed == 0) %>%
  select(-c(cost,limSpeed,seed))

averageTripsBySeed_distance =  melt(averageTripsBySeed_distance, id.vars = c("scenario", "distance"))
averageTripsBySeed_distance$Mode = factor(averageTripsBySeed_distance$variable,levels = c("air","auto","bus","rail"))

tripsBase = averageTripsBySeed_distance %>%
  filter(distance==0)
totalTrips = sum(tripsBase$value)

mode_names = c("air" = "Air",
               "auto" = "Auto",
               "bus" = "Long distance bus",
               "rail" = "Long distance rail")

subtitleFigure = paste("100% sample, 8 random seeds. ", totalTrips, " trips", sep = "")

plot = ggplot(trips_distance_melt,aes(x=distance,y=value))+
  geom_point(size = 1.5, color = "grey70")+
  geom_line(data = averageTripsBySeed_distance, size = 0.5, color = "black")+
  xlab("Minimum air distance allowed (km)")+
  ylab("Number of trips")+
  xlim(0,1000)+
  theme_bw()+
  labs(title = titleFigure, subtitle = subtitleFigure)+
  theme(axis.title.x=element_text(face="bold",size=18),
        axis.text.x=element_text(size=18),
        axis.title.y=element_text(face="bold",size=18),
        axis.text.y=element_text(size=18),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16))+ 
  facet_wrap(vars(Mode), scales = "free",labeller = as_labeller((mode_names)))+
  theme(strip.text.x =element_text(size = 16))
plot 


ggsave(paste(plotName,".distance.tripsbig.jpeg",sep = ""))
ggsave(paste(plotName,".distance.tripsbig.pdf",sep = ""))

plot = ggplot(trips_distance_melt,aes(x=distance,y=value/totalTrips*100))+
  geom_point(size = 1.5, color = "grey70")+
  geom_line(data = averageTripsBySeed_distance, size = 0.5, color = "black")+
  xlab("Minimum air distance allowed (km)")+
  ylab("Mode share (%)")+
  xlim(0,1000)+
  theme_bw()+
  labs(title = titleFigure, subtitle = subtitleFigure)+
  theme(axis.title.x=element_text(face="bold",size=18),
        axis.text.x=element_text(size=18),
        axis.title.y=element_text(face="bold",size=18),
        axis.text.y=element_text(size=18),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16))+ 
  facet_wrap(vars(Mode), scales = "free",labeller = as_labeller((mode_names)))+
  theme(strip.text.x =element_text(size = 16))
plot 

ggsave(paste(plotName,".distance.share.jpeg",sep = ""))
ggsave(paste(plotName,".distance.share.pdf",sep = ""))

rm(averageTripsBySeed_distance,plot,trips_distance,trips_distance_melt,tripsBase)

#--------summary by cost--------------

trips_distance = tripsBySeed %>%
  filter(distance == 0 & limSpeed == 0) %>%
  select(-c(distance,limSpeed))

trips_distance_melt =  melt(trips_distance, id.vars = c("scenario","seed", "cost"))
trips_distance_melt$Mode = factor(trips_distance_melt$variable,levels = c("air","auto","bus","rail"))

averageTripsBySeed_distance = averageTripsBySeed %>%
  filter(distance == 0 & limSpeed == 0) %>%
  select(-c(distance,limSpeed,seed))

averageTripsBySeed_distance =  melt(averageTripsBySeed_distance, id.vars = c("scenario", "cost"))
averageTripsBySeed_distance$Mode = factor(averageTripsBySeed_distance$variable,levels = c("air","auto","bus","rail"))

tripsBase = averageTripsBySeed_distance %>%
  filter(cost==1)
totalTrips = sum(tripsBase$value)

mode_names = c("air" = "Air",
               "auto" = "Auto",
               "bus" = "Long distance bus",
               "rail" = "Long distance rail")

subtitleFigure = paste("100% sample, 8 random seeds. ", totalTrips, " trips", sep = "")

plot = ggplot(trips_distance_melt,aes(x=cost,y=value))+
  geom_point(size = 1.5, color = "grey70")+
  geom_line(data = averageTripsBySeed_distance, size = 0.5, color = "black")+
  xlab("Increase air fare: times more expensive")+
  ylab("Number of trips")+
  theme_bw()+
  labs(title = titleFigure, subtitle = subtitleFigure)+
  theme(axis.title.x=element_text(face="bold",size=18),
        axis.text.x=element_text(size=18),
        axis.title.y=element_text(face="bold",size=18),
        axis.text.y=element_text(size=18),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16))+ 
  facet_wrap(vars(Mode), scales = "free",labeller = as_labeller((mode_names)))+
  theme(strip.text.x =element_text(size = 16))
plot 


ggsave(paste(plotName,".cost.trips.jpeg",sep = ""))
ggsave(paste(plotName,".cost.trips.pdf",sep = ""))

plot = ggplot(trips_distance_melt,aes(x=cost,y=value/totalTrips*100))+
  geom_point(size = 1.5, color = "grey70")+
  geom_line(data = averageTripsBySeed_distance, size = 0.5, color = "black")+
  xlab("Increase air fare: times more expensive")+
  ylab("Mode share (%)")+
  theme_bw()+
  labs(title = titleFigure, subtitle = subtitleFigure)+
  theme(axis.title.x=element_text(face="bold",size=18),
        axis.text.x=element_text(size=18),
        axis.title.y=element_text(face="bold",size=18),
        axis.text.y=element_text(size=18),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16))+ 
  facet_wrap(vars(Mode), scales = "free",labeller = as_labeller((mode_names)))+
  theme(strip.text.x =element_text(size = 16))
plot 

ggsave(paste(plotName,".cost.share.jpeg",sep = ""))
ggsave(paste(plotName,".cost.share.pdf",sep = ""))

rm(averageTripsBySeed_distance,plot,trips_distance,trips_distance_melt,tripsBase)
#--------summary by cost--------------

trips_distance = tripsBySeed %>%
  filter(distance == 0 & cost == 1) %>%
  select(-c(distance,cost))

trips_distance_melt =  melt(trips_distance, id.vars = c("scenario","seed", "limSpeed"))
trips_distance_melt$Mode = factor(trips_distance_melt$variable,levels = c("air","auto","bus","rail"))
trips_distance_melt = trips_distance_melt %>%
  mutate(limSpeed1 = if_else (limSpeed == 0,"No", "Yes"))

averageTripsBySeed_distance = averageTripsBySeed %>%
  filter(distance == 0 & cost == 1) %>%
  select(-c(distance,cost,seed))

averageTripsBySeed_distance =  melt(averageTripsBySeed_distance, id.vars = c("scenario", "limSpeed"))
averageTripsBySeed_distance$Mode = factor(averageTripsBySeed_distance$variable,levels = c("air","auto","bus","rail"))
averageTripsBySeed_distance = averageTripsBySeed_distance %>%
  mutate(limSpeed1 = if_else (limSpeed == 0,"No", "Yes"))


tripsBase = averageTripsBySeed_distance %>%
  filter(limSpeed==1)
totalTrips = sum(tripsBase$value)

mode_names = c("air" = "Air",
               "auto" = "Auto",
               "bus" = "Long distance bus",
               "rail" = "Long distance rail")

subtitleFigure = paste("100% sample, 8 random seeds. ", totalTrips, " trips", sep = "")

plot = ggplot(trips_distance_melt,aes(x=limSpeed1,y=value))+
  geom_point(size = 1.5, color = "grey70")+
  geom_line(data = averageTripsBySeed_distance, size = 0.5, color = "black")+
  xlab("Limit air trips if any other mode is faster")+
  ylab("Number of trips")+
  theme_bw()+
  labs(title = titleFigure, subtitle = subtitleFigure)+
  theme(axis.title.x=element_text(face="bold",size=18),
        axis.text.x=element_text(size=18),
        axis.title.y=element_text(face="bold",size=18),
        axis.text.y=element_text(size=18),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16))+ 
  facet_wrap(vars(Mode), scales = "free",labeller = as_labeller((mode_names)))+
  theme(strip.text.x =element_text(size = 16))
plot 

ggsave(paste(plotName,".speed.trips.jpeg",sep = ""))
ggsave(paste(plotName,".speed.trips.pdf",sep = ""))

plot = ggplot(trips_distance_melt,aes(x=limSpeed1,y=value/totalTrips*100))+
  geom_point(size = 1.5, color = "grey70")+
  geom_line(data = averageTripsBySeed_distance, size = 0.5, color = "black")+
  xlab("Limit air trips if any other mode is faster")+
  ylab("Mode share (%)")+
  theme_bw()+
  labs(title = titleFigure, subtitle = subtitleFigure)+
  theme(axis.title.x=element_text(face="bold",size=18),
        axis.text.x=element_text(face="bold",size=18),
        axis.title.y=element_text(face="bold",size=18),
        axis.text.y=element_text(face="bold",size=18))+ 
  theme(axis.title.x=element_text(face="bold",size=18),
        axis.text.x=element_text(size=18),
        axis.title.y=element_text(face="bold",size=18),
        axis.text.y=element_text(size=18),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16))+ 
  facet_wrap(vars(Mode), scales = "free",labeller = as_labeller((mode_names)))+
  theme(strip.text.x =element_text(size = 16))
plot 

ggsave(paste(plotName,".speed.share.jpeg",sep = ""))
ggsave(paste(plotName,".speed.share.pdf",sep = ""))

rm(averageTripsBySeed,averageTripsBySeed_distance,plot,trips_distance,trips_distance_melt,tripsBase,tripsBySeed)

hospital_labeller <- function(variable,value){
  return(cost_names[value])
}

