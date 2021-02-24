pacman::p_load(dplyr,ggplot2,tidyr,reshape,RColorBrewer)


allTable <- data.frame(matrix(ncol = 1, nrow = 1))


for (i in 0:20){
  scenarioName = paste("C:/models/germanymodel-/output/populationSplit_byPurpose/", "p",i,"_summaryModeChoiceEmissionsByDistance.csv", sep = "")
  summary = read.csv(scenarioName)
  allTable = bind_rows(allTable,summary)
}

colnames(allTable)
allTable = allTable[-1,]

#-----setup trip purpose and state-----
# after the selection, everything is the same for all trip purposes and states

trips  = allTable %>%
  select(subpopulation,scenario = scenarioNoSeed,cost,distance,limSpeed,seed, 
         scenarioLabel = scenario.1 , 
         auto50 = overnight.business.auto.50.co2,
         auto100 = overnight.business.auto.100.co2,
         auto200 = overnight.business.auto.200.co2,
         auto300 = overnight.business.auto.300.co2,
         auto400 = overnight.business.auto.400.co2,
         auto500 = overnight.business.auto.500.co2,
         auto750 = overnight.business.auto.750.co2,
         auto1000 = overnight.business.auto.1000.co2,
         auto5000 = overnight.business.auto.5000.co2,
         air50 = overnight.business.air.50.co2,
         air100 = overnight.business.air.100.co2,
         air200 = overnight.business.air.200.co2,
         air300 = overnight.business.air.300.co2,
         air400 = overnight.business.air.400.co2,
         air500 = overnight.business.air.500.co2,
         air750 = overnight.business.air.750.co2,
         air1000 = overnight.business.air.1000.co2,
         air5000 = overnight.business.air.5000.co2,
         rail50 = overnight.business.rail.50.co2,
         rail100 = overnight.business.rail.100.co2,
         rail200 = overnight.business.rail.200.co2,
         rail300 = overnight.business.rail.300.co2,
         rail400 = overnight.business.rail.400.co2,
         rail500 = overnight.business.rail.500.co2,
         rail750 = overnight.business.rail.750.co2,
         rail1000 = overnight.business.rail.1000.co2,
         rail5000 = overnight.business.rail.5000.co2,
         bus50 = overnight.business.bus.50.co2,
         bus100 = overnight.business.bus.100.co2,
         bus200 = overnight.business.bus.200.co2,
         bus300 = overnight.business.bus.300.co2,
         bus400 = overnight.business.bus.400.co2,
         bus500 = overnight.business.bus.500.co2,
         bus750 = overnight.business.bus.750.co2,
         bus1000 = overnight.business.bus.1000.co2,
         bus5000 = overnight.business.bus.5000.co2)

titleFigure = "Overnight business trips"

plotName =  "C:/models/germanymodel-/output/populationSplit_byPurpose/plotsByDistance/overnight.business"




#-------run---------------------------------
summaryTrips = trips %>% group_by(scenario,seed) %>% 
  summarise(auto50 = sum(auto50),
            auto100 = sum(auto100),
            auto200 = sum(auto200),
            auto300 = sum(auto300),
            auto400 = sum(auto400),
            auto500 = sum(auto500),
            auto750 = sum(auto750),
            auto1000 = sum(auto1000),
            auto5000 = sum(auto5000),
            air50 = sum(air50),
            air100 = sum(air100),
            air200 = sum(air200),
            air300 = sum(air300),
            air400 = sum(air400),
            air500 = sum(air500),
            air750 = sum(air750),
            air1000 = sum(air1000),
            air5000 = sum(air5000),
            rail50 = sum(rail50),
            rail100 = sum(rail100),
            rail200 = sum(rail200),
            rail300 = sum(rail300),
            rail400 = sum(rail400),
            rail500 = sum(rail500),
            rail750 = sum(rail750),
            rail1000 = sum(rail1000),
            rail5000 = sum(rail5000),
            bus50 = sum(bus50),
            bus100 = sum(bus100),
            bus200 = sum(bus200),
            bus300 = sum(bus300),
            bus400 = sum(bus400),
            bus500 = sum(bus500),
            bus750 = sum(bus750),
            bus1000 = sum(bus1000),
            bus5000 = sum(bus5000))

averageByScenario = summaryTrips %>% group_by(scenario) %>%
  summarise(seed = 0,
            auto_50 = mean(auto50),
            auto_100 = mean(auto100),
            auto_200 = mean(auto200),
            auto_300 = mean(auto300),
            auto_400 = mean(auto400),
            auto_500 = mean(auto500),
            auto_750 = mean(auto750),
            auto_1000 = mean(auto1000),
            auto_5000 = mean(auto5000),
            air_50 = mean(air50),
            air_100 = mean(air100),
            air_200 = mean(air200),
            air_300 = mean(air300),
            air_400 = mean(air400),
            air_500 = mean(air500),
            air_750 = mean(air750),
            air_1000 = mean(air1000),
            air_5000 = mean(air5000),
            rail_50 = mean(rail50),
            rail_100 = mean(rail100),
            rail_200 = mean(rail200),
            rail_300 = mean(rail300),
            rail_400 = mean(rail400),
            rail_500 = mean(rail500),
            rail_750 = mean(rail750),
            rail_1000 = mean(rail1000),
            rail_5000 = mean(rail5000),
            bus_50 = mean(bus50),
            bus_100 = mean(bus100),
            bus_200 = mean(bus200),
            bus_300 = mean(bus300),
            bus_400 = mean(bus400),
            bus_500 = mean(bus500),
            bus_750 = mean(bus750),
            bus_1000 = mean(bus1000),
            bus_5000 = mean(bus5000))

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


#--------summary for base--------------

trips_distance = averageTripsBySeed %>%
  filter(distance == 0 & limSpeed == 0 & cost == 1) %>%
  select(-c(distance,limSpeed,cost))


trips_base_melt = melt(trips_distance, id.vars = c("scenario", "seed"))

trips_base_melt$mode = sapply(strsplit(as.character(trips_base_melt$variable),"_"),"[",1)

trips_base_melt$distanceBin = sapply(strsplit(as.character(trips_base_melt$variable),"_"),"[",2)

trips_base_melt$mode2 = factor(trips_base_melt$mode,levels = c("auto", "air","rail","bus"))

trips_base_melt = subset(trips_base_melt, as.numeric(distanceBin) < 1200)

colors = c("grey40","steelblue2","red","palegreen3")

plot = ggplot(trips_base_melt,aes(x=as.factor(as.numeric(distanceBin)),y=value,fill=mode2))+
  geom_bar(stat="identity", position = position_fill())+
  scale_fill_manual(values = colors, name = "Mode")+
  xlab("Distance (km)")+
  ylab("Modal share")+
  theme_bw()+
  labs(title = titleFigure, subtitle = "Base scenario")
plot


plot = ggplot(trips_base_melt,aes(x=as.factor(as.numeric(distanceBin)),y=value/1000000,fill=mode2))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = colors, name = "Mode")+
  xlab("Distance (km)")+
  ylab("CO2 emissions (ton/day)")+
  theme_bw()+
  labs(title = titleFigure, subtitle = "Base scenario")
plot

ggsave(paste(plotName,".modalShareDistance.base123.jpeg",sep = ""))
ggsave(paste(plotName,".modalShareDistance.base123.pdf",sep = ""))


#--------summary by distance--------------

trips_distance = averageTripsBySeed %>%
  filter(cost == 1 & limSpeed == 0) %>%
  select(-c(cost,limSpeed))


trips_base_melt = melt(trips_distance, id.vars = c("scenario", "seed","distance"))

trips_base_melt$mode = sapply(strsplit(as.character(trips_base_melt$variable),"_"),"[",1)

trips_base_melt$distanceBin = sapply(strsplit(as.character(trips_base_melt$variable),"_"),"[",2)

trips_base_melt$mode2 = factor(trips_base_melt$mode,levels = c("auto", "air","rail","bus"))

trips_base_melt = subset(trips_base_melt, as.numeric(distanceBin) < 1200)

colors = c("grey40","steelblue2","red","palegreen3")

plot = ggplot(trips_base_melt,aes(x=as.factor(as.numeric(distanceBin)),y=value,fill=mode2))+
  geom_bar(stat="identity", position = position_fill())+
  scale_fill_manual(values = colors, name = "Mode")+
  xlab("Distance (km)")+
  ylab("Modal share")+
  theme_bw()+
  labs(title = titleFigure)+
  facet_wrap(vars(distance))
plot

ggsave(paste(plotName,".modalShareDistance.distance.jpeg",sep = ""))
ggsave(paste(plotName,".modalShareDistance.distance.pdf",sep = ""))


#--------summary by cost--------------

trips_distance = averageTripsBySeed %>%
  filter(distance == 0 & limSpeed == 0) %>%
  select(-c(distance,limSpeed))


trips_base_melt = melt(trips_distance, id.vars = c("scenario", "seed","cost"))

trips_base_melt$mode = sapply(strsplit(as.character(trips_base_melt$variable),"_"),"[",1)

trips_base_melt$distanceBin = sapply(strsplit(as.character(trips_base_melt$variable),"_"),"[",2)

trips_base_melt$mode2 = factor(trips_base_melt$mode,levels = c("auto", "air","rail","bus"))

trips_base_melt = subset(trips_base_melt, as.numeric(distanceBin) < 1200)

colors = c("grey40","steelblue2","red","palegreen3")

plot = ggplot(trips_base_melt,aes(x=as.factor(as.numeric(distanceBin)),y=value,fill=mode2))+
  geom_bar(stat="identity", position = position_fill())+
  scale_fill_manual(values = colors, name = "Mode")+
  xlab("Distance (km)")+
  ylab("Modal share")+
  theme_bw()+
  labs(title = titleFigure)+
  facet_wrap(vars(cost))
plot

ggsave(paste(plotName,".modalShareDistance.cost.jpeg",sep = ""))
ggsave(paste(plotName,".modalShareDistance.cost.pdf",sep = ""))

hospital_labeller <- function(variable,value){
  return(cost_names[value])
}

