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

co2 = allTable %>%
  mutate(auto = daytrip.private.auto.co2 + overnight.private.auto.co2 +
           daytrip.leisure.auto.co2 + overnight.leisure.auto.co2 +
           daytrip.business.auto.co2 + overnight.business.auto.co2,
         air = daytrip.private.air.co2 + overnight.private.air.co2 +
           daytrip.leisure.air.co2 + overnight.leisure.air.co2 +
           daytrip.business.air.co2 + overnight.business.air.co2,
         rail = daytrip.private.rail.co2 + overnight.private.rail.co2 +
           daytrip.leisure.rail.co2 + overnight.leisure.rail.co2 +
           daytrip.business.rail.co2 + overnight.business.rail.co2,
         bus = daytrip.private.bus.co2 + overnight.private.bus.co2 +
           daytrip.leisure.bus.co2 + overnight.leisure.bus.co2 +
           daytrip.business.bus.co2 + overnight.business.bus.co2,
         ground = auto + rail + bus,
         all = ground + air)%>%
  select(subpopulation,scenario = scenarioNoSeed,cost,distance,limSpeed,seed, 
         scenarioLabel = scenario.1 , 
         auto, air,rail,bus,ground,all)

titleFigure = "All trips"

plotName =  "C:/models/germanymodel-/output/populationSplit_byPurpose/plots/allEmissions"


#-------run---------------------------------
summaryTrips = co2 %>% group_by(scenario,seed) %>% 
  summarise(auto = sum(auto),
            air = sum(air),
            rail = sum(rail),
            bus = sum(bus),
            ground = sum(ground),
            all = sum(all))

averageByScenario = summaryTrips %>% group_by(scenario) %>%
  summarise(seed = 0,
            auto = mean(auto),
            air = mean(air),
            rail = mean(rail),
            bus = mean(bus),
            ground = mean(ground),
            all = mean(all))

scenarioSettings = co2 %>%
  select(subpopulation,scenario,cost,distance,limSpeed,seed) 
scenarioSettings = subset(scenarioSettings, subpopulation == 5)
scenarioSettings = scenarioSettings %>%
  select(scenario,cost,distance,limSpeed,seed) 

tripsBySeed = merge(scenarioSettings, summaryTrips)


scenarioSettingsAverage = subset(scenarioSettings, seed == 1)
scenarioSettingsAverage$seed = 0
averageTripsBySeed = merge(scenarioSettingsAverage, averageByScenario)


rm(co2,averageByScenario,scenarioSettings,scenarioSettingsAverage,summaryTrips)

write.csv(tripsBySeed,paste(plotName,".co2.csv",sep = ""), row.names = FALSE)

#--------summary by distance--------------

trips_distance = tripsBySeed %>%
  filter(cost == 1 & limSpeed == 0) %>%
  select(-c(cost,limSpeed,auto,bus,rail))

trips_distance_melt =  melt(trips_distance, id.vars = c("scenario","seed", "distance"))
trips_distance_melt$Mode = factor(trips_distance_melt$variable,levels = c("air","ground","all"))

averageTripsBySeed_distance = averageTripsBySeed %>%
  filter(cost == 1 & limSpeed == 0) %>%
  select(-c(cost,limSpeed,seed,auto,bus,rail))

averageTripsBySeed_distance =  melt(averageTripsBySeed_distance, id.vars = c("scenario", "distance"))
averageTripsBySeed_distance$Mode = factor(averageTripsBySeed_distance$variable,levels = c("air","ground","all"))

tripsBase = averageTripsBySeed_distance %>%
  filter(distance==0)
totalTrips = sum(tripsBase$value)

mode_names = c("air" = "Air",
               "ground" = "Ground modes",
               "all" = "All modes")

subtitleFigure = paste("100% sample, 8 random seeds. ", totalTrips, " trips", sep = "")

plot = ggplot(trips_distance_melt,aes(x=distance,y=value/1000000))+
  geom_point(size = 1.5, color = "grey70")+
  geom_line(data = averageTripsBySeed_distance, size = 0.5, color = "black")+
  xlab("Minimum air distance allowed (km)")+
  ylab("CO2 emissions (ton/day)")+
  xlim(0,1000)+
  theme_bw()+
  labs(title = titleFigure, subtitle = subtitleFigure)+
  theme(axis.title.x=element_text(face="bold",size=18),
        axis.text.x=element_text(size=16),
        axis.title.y=element_text(face="bold",size=18),
        axis.text.y=element_text(size=16),
        plot.title = element_text(face="bold",size = 20),
        plot.subtitle = element_text(size = 16))+ 
  facet_wrap(vars(Mode), scales = "free",labeller = as_labeller((mode_names)))+
  theme(strip.text.x =element_text(size = 16))
plot 


ggsave(paste(plotName,".distance.emissions.jpeg",sep = ""))
ggsave(paste(plotName,".distance.emissions.pdf",sep = ""))

rm(averageTripsBySeed_distance,plot,trips_distance,trips_distance_melt,tripsBase)

#--------summary by cost--------------

trips_distance = tripsBySeed %>%
  filter(distance == 0 & limSpeed == 0) %>%
  select(-c(distance,limSpeed,auto,bus,rail))

trips_distance = subset(trips_distance, cost != 1.5)

trips_distance_melt =  melt(trips_distance, id.vars = c("scenario","seed", "cost"))
trips_distance_melt$Mode = factor(trips_distance_melt$variable,levels = c("air","ground","all"))

averageTripsBySeed_distance = averageTripsBySeed %>%
  filter(distance == 0 & limSpeed == 0) %>%
  select(-c(distance,limSpeed,seed,auto,bus,rail))

averageTripsBySeed_distance = subset(averageTripsBySeed_distance, cost != 1.5)

averageTripsBySeed_distance =  melt(averageTripsBySeed_distance, id.vars = c("scenario", "cost"))
averageTripsBySeed_distance$Mode = factor(averageTripsBySeed_distance$variable,levels = c("air","ground","all"))

tripsBase = averageTripsBySeed_distance %>%
  filter(cost==1)
totalTrips = sum(tripsBase$value)

mode_names = c("air" = "Air",
               "ground" = "Ground modes",
               "all" = "All modes")

subtitleFigure = paste("100% sample, 8 random seeds. ", totalTrips, " trips", sep = "")

plot = ggplot(trips_distance_melt,aes(x=cost,y=value/1000000))+
  geom_point(size = 1.5, color = "grey70")+
  geom_line(data = averageTripsBySeed_distance, size = 0.5, color = "black")+
  xlab("Increase air fare: times more expensive")+
  ylab("CO2 emissions (ton/day)")+
  theme_bw()+
  labs(title = titleFigure, subtitle = subtitleFigure)+
  theme(axis.title.x=element_text(face="bold",size=18),
        axis.text.x=element_text(size=16),
        axis.title.y=element_text(face="bold",size=18),
        axis.text.y=element_text(size=16),
        plot.title = element_text(face="bold",size = 20),
        plot.subtitle = element_text(size = 16))+ 
  facet_wrap(vars(Mode), scales = "free",labeller = as_labeller((mode_names)))+
  theme(strip.text.x =element_text(size = 16))
plot 


ggsave(paste(plotName,".cost.emissions.jpeg",sep = ""))
ggsave(paste(plotName,".cost.emissions.pdf",sep = ""))


rm(averageTripsBySeed_distance,plot,trips_distance,trips_distance_melt,tripsBase)

#--------summary by speed--------------

trips_distance = tripsBySeed %>%
  filter(distance == 0 & cost == 1) %>%
  select(-c(distance,cost,auto,bus,rail))

trips_distance_melt =  melt(trips_distance, id.vars = c("scenario","seed", "limSpeed"))
trips_distance_melt$Mode = factor(trips_distance_melt$variable,levels = c("air","ground","all"))
trips_distance_melt = trips_distance_melt %>%
  mutate(limSpeed1 = if_else (limSpeed == 0,"No", "Yes"))

averageTripsBySeed_distance = averageTripsBySeed %>%
  filter(distance == 0 & cost == 1) %>%
  select(-c(distance,cost,seed,auto,bus,rail))

averageTripsBySeed_distance =  melt(averageTripsBySeed_distance, id.vars = c("scenario", "limSpeed"))
averageTripsBySeed_distance$Mode = factor(averageTripsBySeed_distance$variable,levels = c("air","ground","all"))
averageTripsBySeed_distance = averageTripsBySeed_distance %>%
  mutate(limSpeed1 = if_else (limSpeed == 0,"No", "Yes"))


tripsBase = averageTripsBySeed_distance %>%
  filter(limSpeed==1)
totalTrips = sum(tripsBase$value)

mode_names = c("air" = "Air",
               "ground" = "Ground modes",
               "all" = "All modes")

subtitleFigure = paste("100% sample, 8 random seeds. ", totalTrips, " trips", sep = "")

plot = ggplot(trips_distance_melt,aes(x=limSpeed1,y=value/1000000))+
  geom_point(size = 1.5, color = "grey70")+
  geom_line(data = averageTripsBySeed_distance, size = 0.5, color = "black")+
  xlab("Limit air trips if any other mode is faster")+
  ylab("CO2 emissions (ton/day)")+
  theme_bw()+
  labs(title = titleFigure, subtitle = subtitleFigure)+
  theme(axis.title.x=element_text(face="bold",size=18),
        axis.text.x=element_text(size=16),
        axis.title.y=element_text(face="bold",size=18),
        axis.text.y=element_text(size=16),
        plot.title = element_text(face="bold",size = 20),
        plot.subtitle = element_text(size = 16))+ 
  facet_wrap(vars(Mode), scales = "free",labeller = as_labeller((mode_names)))+
  theme(strip.text.x =element_text(size = 16))
plot 

ggsave(paste(plotName,".speed.emissions.jpeg",sep = ""))
ggsave(paste(plotName,".speed.emissions.pdf",sep = ""))

rm(averageTripsBySeed,averageTripsBySeed_distance,plot,trips_distance,trips_distance_melt,tripsBase,tripsBySeed)

hospital_labeller <- function(variable,value){
  return(cost_names[value])
}

