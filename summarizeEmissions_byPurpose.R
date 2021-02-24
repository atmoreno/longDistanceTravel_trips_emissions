pacman::p_load(dplyr,ggplot2,tidyr,reshape,RColorBrewer)


allTable <- data.frame(matrix(ncol = 1, nrow = 1))


for (i in 0:19){
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
         auto = daytrip.business.auto.co2, 
         air = daytrip.business.air.co2, 
         rail = daytrip.business.rail.co2, 
         bus = daytrip.business.bus.co2)

titleFigure = "Daytrip business trips"

plotName =  "C:/models/germanymodel-/output/populationSplit_byPurpose/emissionsPlots/daytrip.business"
#-------run---------------------------------
summaryTrips = trips %>% group_by(scenario,seed) %>% 
  summarise(auto = sum(auto),
            air = sum(air),
            rail = sum(rail),
            bus = sum(bus),
            ground = auto+rail+bus,
            all = ground + air)

averageByScenario = summaryTrips %>% group_by(scenario) %>%
  summarise(seed = 0,
            auto = mean(auto),
            air = mean(air),
            rail = mean(rail),
            bus = mean(bus),
            ground = auto + rail + bus,
            all = ground + air)

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

write.csv(tripsBySeed,paste(plotName,".emissions.csv",sep = ""), row.names = FALSE)

#--------summary by distance--------------

trips_distance = tripsBySeed %>%
  filter(cost == 1 & limSpeed == 0) %>%
  select(-c(cost,limSpeed,auto,rail,bus))

trips_distance_melt =  melt(trips_distance, id.vars = c("scenario","seed", "distance"))
trips_distance_melt$Mode = factor(trips_distance_melt$variable,levels = c("air","ground","all"))

averageTripsBySeed_distance = averageTripsBySeed %>%
  filter(cost == 1 & limSpeed == 0) %>%
  select(-c(cost,limSpeed,seed,auto,rail,bus))

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
        axis.text.x=element_text(size=18),
        axis.title.y=element_text(face="bold",size=18),
        axis.text.y=element_text(size=18),
        plot.title = element_text(size = 20),
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
  select(-c(distance,limSpeed,auto,rail,bus))
trips_distance = subset(trips_distance, cost != 1.5)

trips_distance_melt =  melt(trips_distance, id.vars = c("scenario","seed", "cost"))
trips_distance_melt$Mode = factor(trips_distance_melt$variable,levels = c("air","ground","all"))

averageTripsBySeed_distance = averageTripsBySeed %>%
  filter(distance == 0 & limSpeed == 0) %>%
  select(-c(distance,limSpeed,seed,auto,rail,bus))
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
        axis.text.x=element_text(size=18),
        axis.title.y=element_text(face="bold",size=18),
        axis.text.y=element_text(size=18),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16))+ 
  facet_wrap(vars(Mode), scales = "free",labeller = as_labeller((mode_names)))+
  theme(strip.text.x =element_text(size = 16))
plot 


ggsave(paste(plotName,".cost.emissions.jpeg",sep = ""))
ggsave(paste(plotName,".cost.emissions.pdf",sep = ""))

#---------------4 modes and total

trips_distance = tripsBySeed %>%
  filter(distance == 0 & limSpeed == 0) %>%
  select(-c(distance,limSpeed,ground))
trips_distance = subset(trips_distance, cost != 1.5)

trips_distance_melt =  melt(trips_distance, id.vars = c("scenario","seed", "cost"))
trips_distance_melt$Mode = factor(trips_distance_melt$variable,levels = c("air","auto","bus","rail","all"))

averageTripsBySeed_distance = averageTripsBySeed %>%
  filter(distance == 0 & limSpeed == 0) %>%
  select(-c(distance,limSpeed,seed,ground))
averageTripsBySeed_distance = subset(averageTripsBySeed_distance, cost != 1.5)

averageTripsBySeed_distance =  melt(averageTripsBySeed_distance, id.vars = c("scenario", "cost"))
averageTripsBySeed_distance$Mode = factor(averageTripsBySeed_distance$variable,levels = c("air","auto","bus","rail","all"))

tripsBase = averageTripsBySeed_distance %>%
  filter(cost==1)
totalTrips = sum(tripsBase$value)

mode_names = c("air" = "Air",
               "auto" = "Auto",
               "bus" = "Long distance bus",
               "rail" = "Long distance rail",
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
        axis.text.x=element_text(size=18),
        axis.title.y=element_text(face="bold",size=18),
        axis.text.y=element_text(size=18),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16))+ 
  facet_wrap(vars(Mode), scales = "free",labeller = as_labeller((mode_names)))+
  theme(strip.text.x =element_text(size = 16))
plot 


ggsave(paste(plotName,".cost.emissions4modes.jpeg",sep = ""))
ggsave(paste(plotName,".cost.emissions4modes.pdf",sep = ""))



hospital_labeller <- function(variable,value){
  return(cost_names[value])
}

