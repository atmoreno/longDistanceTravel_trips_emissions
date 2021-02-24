pacman::p_load(dplyr,ggplot2,tidyr,reshape,RColorBrewer)



scenarios = c("d0_c1_trips","d300_c1_trips", "d500_c1_trips", "d700_c1_trips", "d900_c1_trips",
              "d0_c1.5_trips","d0_c2_trips","d0_c3_trips","d0_c5_trips",
              "d300_c1.5_trips", "d500_c1.5_trips", "d700_c1.5_trips", "d900_c1.5_trips",
              "d300_c2_trips", "d500_c2_trips", "d700_c2_trips", "d900_c2_trips",
              "d300_c3_trips", "d500_c3_trips", "d700_c3_trips", "d900_c3_trips",
              "d300_c5_trips", "d500_c5_trips", "d700_c5_trips", "d900_c5_trips")
outputNames = c("Base","limitAirBelow300", "limitAirBelow500", "limitAirBelow700", "limitAirBelow900",
                "50increaseAirCost","100increaseAirCost","300increaseAirCost","500increaseAirCost",
                "limitAirBelow300_50increaseAirCost", "limitAirBelow500_50increaseAirCost","limitAirBelow700_50increaseAirCost","limitAirBelow900_50increaseAirCost",
                "limitAirBelow300_100increaseAirCost", "limitAirBelow500_100increaseAirCost","limitAirBelow700_100increaseAirCost","limitAirBelow900_100increaseAirCost",
                "limitAirBelow300_300increaseAirCost", "limitAirBelow500_300increaseAirCost","limitAirBelow700_300increaseAirCost","limitAirBelow900_300increaseAirCost",
                "limitAirBelow300_500increaseAirCost", "limitAirBelow500_500increaseAirCost","limitAirBelow700_500increaseAirCost","limitAirBelow900_500increaseAirCost")

allmodesTable <- data.frame(matrix(ncol = 5, nrow = 1))
x <- c("tripMode", "tripCount","CO2emissions_tonn", "averageDistance","scenario")
colnames(allmodesTable) <- x
airGroundTable <- data.frame(matrix(ncol = 5, nrow = 1))
y <- c("modeTwo", "tripCount","CO2emissions_tonn", "averageDistance","scenario")
colnames(airGroundTable) <-y
all <- data.frame(matrix(ncol = 4, nrow = 1))
z <- c("tripCount","CO2emissions_tonn", "averageDistance","scenario")
colnames(all) <-z
for (i in 1:25){
  scenarioName = paste("C:/models/germanymodel-/output/10_1000_1/", "8_",scenarios[i],".csv", sep = "")
  trips = read.csv(scenarioName)
  trips <- trips[!grepl("away", trips$tripState),]
  trips = trips %>%
    mutate(modeTwo = if_else(tripMode == "air","air", "ground"))
  test <- trips %>%
    dplyr::group_by(tripMode) %>%
    dplyr::summarize(tripCount = n(), CO2emissions_tonn = sum(CO2emissions_kg) / 1000,
                     averageDistance = mean(travelDistance_km))
  test$scenario <-outputNames[i]
  allmodesTable = bind_rows(allmodesTable,test)
  test2 <- trips %>%
    dplyr::group_by(modeTwo) %>%
    dplyr::summarize(tripCount = n(), CO2emissions_tonn = sum(CO2emissions_kg) / 1000,
                     averageDistance = mean(travelDistance_km))
  test2$scenario <-outputNames[i]
  airGroundTable = bind_rows(airGroundTable,test2)
  test3 <- trips %>%
    dplyr::summarize(tripCount = n(), CO2emissions_tonn = sum(CO2emissions_kg) / 1000,
                     averageDistance = mean(travelDistance_km))
  test3$scenario <-outputNames[i]
  all = bind_rows(all,test3)
}
allmodesTable = allmodesTable[-1,]
airGroundTable = airGroundTable[-1,]
all = all[-1,]
write.csv(allmodesTable,"C:/models/germanymodel-/output/paper/10_10k_summary4Modes_8.csv")
write.csv(airGroundTable,"C:/models/germanymodel-/output/paper/10_10k_summary2Modes_8.csv")
write.csv(all,"C:/models/germanymodel-/output/paper/10_10k_summary_8.csv")


rm(test, test2,test3,trips)

#For data visualization---------------------------

tt = read.csv("C:/models/germanymodel-/output/paper/summary20airGround.csv")

plotTitle = "20 % sample. Average of 8 random seeds"
# tt = read.csv("C:/models/germanymodel-/output/paper/summary10population10000destinationChoiceNO50.csv")
# tt = read.csv("C:/models/germanymodel-/output/paper/seed5no50.csv")

tt$differenceCO2 = tt$differenceCO2*5
colnames(tt)

colors = c("steelblue2","tan3","grey40")



tt_distance = subset(tt,type_scen=="distance")
tt_distance$Mode = factor(tt_distance$mode,levels = c("air","ground","all"))
plot = ggplot(tt_distance,aes(x=distance,y=differenceCO2,fill=Mode))+
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values = colors)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Difference in CO2 emissions (ton/day)")+
  theme_bw()+
  ggtitle(plotTitle)
plot

tt_cost = subset(tt,type_scen=="cost")
tt_cost$Mode = factor(tt_cost$mode,levels = c("air","ground","all"))
plot = ggplot(tt_cost,aes(x=as.factor(cost),y=differenceCO2,fill=Mode))+
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values = colors)+
  xlab("Times more expensive air fare")+
  ylab("Difference in CO2 emissions (ton/day)")+
  theme_bw()+
  ggtitle(plotTitle)
plot

tt_Distancecost = subset(tt,type_scen=="distance_cost")
tt_Distancecost$Mode = factor(tt_Distancecost$mode,levels = c("air","ground","all"))
cost_names = c(`1.5` = "Air fare 1.5 times more expensive",
               `2` = "Air fare 2 times more expensive",
               `3` = "Air fare 3 times more expensive",
               `5` = "Air fare 5 times more expensive")
plot = ggplot(tt_Distancecost,aes(x=distance,y=differenceCO2,fill=Mode))+
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values = colors)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Difference in CO2 emissions (ton/day)")+
  theme_bw()+
  facet_wrap(~cost, labeller = as_labeller((cost_names)))+
  ggtitle(plotTitle)
plot

hospital_labeller <- function(variable,value){
  return(cost_names[value])
}


#-----------------------------------------------------------
#  Analyze 5% population results MODAL SHARE
#-----------------------------------------------------------


tt4modes = read.csv("C:/models/germanymodel-/output/paper/summary5pop10000dc20aggregate_4modes.csv")

plotTitle = "5 % sample. 10 random seeds"

tt4 = subset(tt4modes, seed == 0)
tt4 = subset(tt4,tripMode != "null")

colors = c("steelblue2","grey40","green","red")
tt_base = subset(tt4,type_scen == "base")
tt_base$Mode = factor(tt_base$tripMode,levels = c("air","auto","bus", "rail"))

tt4 = subset(tt4modes,tripMode != "null")
tt4$Mode = factor(tt4$tripMode,levels = c("air","auto","bus", "rail"))

colors = c("steelblue2","grey40","green","red")
tt_base = subset(tt4,type_scen == "base")
tt_base$Mode = factor(tt_base$tripMode,levels = c("air","auto","bus", "rail"))

tt_distance = subset(tt4,type_scen=="distance")

colorsSeeds = c("black", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70")
sizeSeeds = c(1.5,0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)

plotTitle = "5 % sample. 10 random seeds"
tt_distance$Mode = factor(tt_distance$tripMode,levels = c("air","auto","bus", "rail"))
tt_distance = rbind(tt_distance,tt_base)
plot = ggplot(tt_distance,aes(x=distance,y=tripCount/2592.9,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Mode share (%)")+
  theme_bw()+
  ggtitle(plotTitle)+
  facet_wrap(vars(Mode), scales = "free")
plot 

tt_cost = subset(tt4,type_scen=="cost")
tt_cost = rbind(tt_cost,tt_base)
plot = ggplot(tt_cost,aes(x=cost,y=tripCount/2592.9,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Times more expensive air fare")+
  ylab("Difference in CO2 emissions (ton/day)")+
  theme_bw()+
  ggtitle(plotTitle)+
  facet_wrap(vars(Mode), scales = "free")
plot

tt_Distancecost = subset(tt4,type_scen=="distance_cost")


plotTitle = "Air fare 1.5 times more expensive"
tt_Distancecost50 = subset(tt_Distancecost,cost ==50)
tt_Distancecost50 = rbind(tt_Distancecost50,tt_base)
plot = ggplot(tt_Distancecost50,aes(x=distance,y=tripCount/2592.9,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Mode share (%)")+
  theme_bw()+
  facet_wrap(~Mode,  scales = "free")+
  ggtitle(plotTitle)
plot

plotTitle = "Air fare 2 times more expensive"
tt_Distancecost100 = subset(tt_Distancecost,cost ==100)
tt_Distancecost100 = rbind(tt_Distancecost100,tt_base)
plot = ggplot(tt_Distancecost100,aes(x=distance,y=tripCount/2592.9,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Mode share (%)")+
  theme_bw()+
  facet_wrap(~Mode,  scales = "free")+
  ggtitle(plotTitle)
plot

plotTitle = "Air fare 3 times more expensive"
tt_Distancecost300 = subset(tt_Distancecost,cost ==300)
tt_Distancecost300 = rbind(tt_Distancecost300,tt_base)
plot = ggplot(tt_Distancecost300,aes(x=distance,y=tripCount/2592.9,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Mode share (%)")+
  theme_bw()+
  facet_wrap(~Mode,  scales = "free")+
  ggtitle(plotTitle)
plot

plotTitle = "Air fare 5 times more expensive"
tt_Distancecost500 = subset(tt_Distancecost,cost ==500)
tt_Distancecost500 = rbind(tt_Distancecost500,tt_base)
plot = ggplot(tt_Distancecost500,aes(x=distance,y=tripCount/2592.9,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Mode share (%)")+
  theme_bw()+
  facet_wrap(~Mode,  scales = "free")+
  ggtitle(plotTitle)
plot


hospital_labeller <- function(variable,value){
  return(cost_names[value])
}


#-----------------------------------------------------------
#  Analyze 10% population results MODAL SHARE
#-----------------------------------------------------------


tt4modes = read.csv("C:/models/germanymodel-/output/paper/summary20pop10000dc20aggregate_4modes_tripTotal.csv")


colorsSeeds = c("black", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70")
sizeSeeds = c(1.5,0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)


tt4 = subset(tt4modes,tripMode != "null")
tt4$Mode = factor(tt4$tripMode,levels = c("air","auto","bus", "rail"))

tt_base = subset(tt4,type_scen == "base")

tt_distance = subset(tt4,type_scen=="distance")
tt_distance = rbind(tt_distance,tt_base)
plotTitle = "20 % sample. 8 random seeds"
plot = ggplot(tt_distance,aes(x=distance,y=tripCount/10478.31,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Mode share (%)")+
  theme_bw()+
  ggtitle(plotTitle)+
  facet_wrap(vars(Mode), scales = "free")
plot 

tt_cost = subset(tt4,type_scen=="cost")
tt_cost = rbind(tt_cost,tt_base)
plotTitle = "20 % sample. 8 random seeds"
plot = ggplot(tt_cost,aes(x=cost,y=tripCount/10478.31,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Times more expensive air fare")+
  ylab("Difference in CO2 emissions (ton/day)")+
  theme_bw()+
  ggtitle(plotTitle)+
  facet_wrap(vars(Mode), scales = "free")
plot

tt_Distancecost = subset(tt4,type_scen=="distance_cost")
plotTitle = "Air fare 1.5 times more expensive"
tt_Distancecost50 = subset(tt_Distancecost,cost ==50)
tt_Distancecost50 = rbind(tt_Distancecost50,tt_base)
plot = ggplot(tt_Distancecost50,aes(x=distance,y=tripCount/10478.31,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Mode share (%)")+
  theme_bw()+
  facet_wrap(~Mode,  scales = "free")+
  ggtitle(plotTitle)
plot

plotTitle = "Air fare 2 times more expensive"
tt_Distancecost100 = subset(tt_Distancecost,cost ==100)
tt_Distancecost100 = rbind(tt_Distancecost100,tt_base)
plot = ggplot(tt_Distancecost100,aes(x=distance,y=tripCount/10478.31,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Mode share (%)")+
  theme_bw()+
  facet_wrap(~Mode,  scales = "free")+
  ggtitle(plotTitle)
plot

plotTitle = "Air fare 3 times more expensive"
tt_Distancecost300 = subset(tt_Distancecost,cost ==300)
tt_Distancecost300 = rbind(tt_Distancecost300,tt_base)
plot = ggplot(tt_Distancecost300,aes(x=distance,y=tripCount/10478.31,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Mode share (%)")+
  theme_bw()+
  facet_wrap(~Mode,  scales = "free")+
  ggtitle(plotTitle)
plot

plotTitle = "Air fare 5 times more expensive"
tt_Distancecost500 = subset(tt_Distancecost,cost ==500)
tt_Distancecost500 = rbind(tt_Distancecost500,tt_base)
plot = ggplot(tt_Distancecost500,aes(x=distance,y=tripCount/10478.31,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Mode share (%)")+
  theme_bw()+
  facet_wrap(~Mode,  scales = "free")+
  ggtitle(plotTitle)
plot


#-----------------------------------------------------------
#  Analyze higher sample by grouping some lower sample results
#-----------------------------------------------------------


tt4modes = read.csv("C:/models/germanymodel-/output/paper/summary10pop10000dc20aggregate_4modes_1.csv")


colorsSeeds = c("black", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70")
sizeSeeds = c(1.5,0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)


tt4 = subset(tt4modes,tripMode != "null")
tt4$Mode = factor(tt4$tripMode,levels = c("air","auto","bus", "rail"))

tt_base = subset(tt4,type_scen == "base")

tt_distance = subset(tt4,type_scen=="distance")
tt_distance = rbind(tt_distance,tt_base)
plotTitle = "20 % sample (add two 10% sample). 8 random seeds"
plot = ggplot(tt_distance,aes(x=distance,y=tripCount,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Mode share (%)")+
  theme_bw()+
  ggtitle(plotTitle)+
  facet_wrap(vars(Mode), scales = "free")
plot 

tt_cost = subset(tt4,type_scen=="cost")
tt_cost = rbind(tt_cost,tt_base)
plotTitle = "20 % sample (add two 10% sample). 8 random seeds"
plot = ggplot(tt_cost,aes(x=cost,y=tripCount/10475.46,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Times more expensive air fare")+
  ylab("Difference in CO2 emissions (ton/day)")+
  theme_bw()+
  ggtitle(plotTitle)+
  facet_wrap(vars(Mode), scales = "free")
plot

tt_Distancecost = subset(tt4,type_scen=="distance_cost")
plotTitle = "Air fare 1.5 times more expensive"
tt_Distancecost50 = subset(tt_Distancecost,cost ==50)
tt_Distancecost50 = rbind(tt_Distancecost50,tt_base)
plot = ggplot(tt_Distancecost50,aes(x=distance,y=tripCount/10475.46,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Mode share (%)")+
  theme_bw()+
  facet_wrap(~Mode,  scales = "free")+
  ggtitle(plotTitle)
plot

plotTitle = "Air fare 2 times more expensive"
tt_Distancecost100 = subset(tt_Distancecost,cost ==100)
tt_Distancecost100 = rbind(tt_Distancecost100,tt_base)
plot = ggplot(tt_Distancecost100,aes(x=distance,y=tripCount/10475.46,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Mode share (%)")+
  theme_bw()+
  facet_wrap(~Mode,  scales = "free")+
  ggtitle(plotTitle)
plot

plotTitle = "Air fare 3 times more expensive"
tt_Distancecost300 = subset(tt_Distancecost,cost ==300)
tt_Distancecost300 = rbind(tt_Distancecost300,tt_base)
plot = ggplot(tt_Distancecost300,aes(x=distance,y=tripCount/10475.46,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Mode share (%)")+
  theme_bw()+
  facet_wrap(~Mode,  scales = "free")+
  ggtitle(plotTitle)
plot

plotTitle = "Air fare 5 times more expensive"
tt_Distancecost500 = subset(tt_Distancecost,cost ==500)
tt_Distancecost500 = rbind(tt_Distancecost500,tt_base)
plot = ggplot(tt_Distancecost500,aes(x=distance,y=tripCount/10475.46,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Mode share (%)")+
  theme_bw()+
  facet_wrap(~Mode,  scales = "free")+
  ggtitle(plotTitle)
plot




#-----------------------------------------------------------
#  Analyze 10% population results TRIP DIFFERENCE WITH BASE
#-----------------------------------------------------------


tt4modes = read.csv("C:/models/germanymodel-/output/paper/summary20pop10000dc20aggregate_4modes_tripTotal.csv")


colorsSeeds = c("black", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70", "grey70")
sizeSeeds = c(1.5,0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)


tt4 = subset(tt4modes,tripMode != "null")
tt4$Mode = factor(tt4$tripMode,levels = c("air","auto","bus", "rail"))

tt_base = subset(tt4,type_scen == "base")

tt_distance = subset(tt4,type_scen=="distance")
tt_distance = rbind(tt_distance,tt_base)
plotTitle = "20 % sample. 8 random seeds"
plot = ggplot(tt_distance,aes(x=distance,y=tripDiff/2,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds, name = "Seed")+
  scale_size_manual(values = sizeSeeds, name = "Seed")+
  xlab("Minimum air distance allowed (km)")+
  ylab("Difference on trips respect to base scenario (scaled to 100% sample)")+
  theme_bw()+
  ggtitle(plotTitle)+
  facet_wrap(vars(Mode), scales = "free")
plot 

tt_cost = subset(tt4,type_scen=="cost")
tt_cost = rbind(tt_cost,tt_base)
plotTitle = "20 % sample. 8 random seeds"
plot = ggplot(tt_cost,aes(x=cost,y=tripDiff,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Times more expensive air fare")+
  ylab("Difference on trips respect to base scenario (scaled to 100% sample)")+
  theme_bw()+
  ggtitle(plotTitle)+
  facet_wrap(vars(Mode), scales = "free")
plot

tt_Distancecost = subset(tt4,type_scen=="distance_cost")
plotTitle = "Air fare 1.5 times more expensive"
tt_Distancecost50 = subset(tt_Distancecost,cost ==50)
tt_Distancecost50 = rbind(tt_Distancecost50,tt_base)
plot = ggplot(tt_Distancecost50,aes(x=distance,y=tripDiff,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Difference on trips respect to base scenario (scaled to 100% sample)")+
  theme_bw()+
  facet_wrap(~Mode,  scales = "free")+
  ggtitle(plotTitle)
plot

plotTitle = "Air fare 2 times more expensive"
tt_Distancecost100 = subset(tt_Distancecost,cost ==100)
tt_Distancecost100 = rbind(tt_Distancecost100,tt_base)
plot = ggplot(tt_Distancecost100,aes(x=distance,y=tripDiff,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Difference on trips respect to base scenario (scaled to 100% sample)")+
  theme_bw()+
  facet_wrap(~Mode,  scales = "free")+
  ggtitle(plotTitle)
plot

plotTitle = "Air fare 3 times more expensive"
tt_Distancecost300 = subset(tt_Distancecost,cost ==300)
tt_Distancecost300 = rbind(tt_Distancecost300,tt_base)
plot = ggplot(tt_Distancecost300,aes(x=distance,y=tripDiff,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Difference on trips respect to base scenario (scaled to 100% sample)")+
  theme_bw()+
  facet_wrap(~Mode,  scales = "free")+
  ggtitle(plotTitle)
plot

plotTitle = "Air fare 5 times more expensive"
tt_Distancecost500 = subset(tt_Distancecost,cost ==500)
tt_Distancecost500 = rbind(tt_Distancecost500,tt_base)
plot = ggplot(tt_Distancecost500,aes(x=distance,y=tripDiff,group = as.factor(seed), color=as.factor(seed), size = as.factor(seed)))+
  geom_line()+
  scale_color_manual(values = colorsSeeds)+
  scale_size_manual(values = sizeSeeds)+
  xlab("Minimum air distance allowed (km)")+
  ylab("Difference on trips respect to base scenario (scaled to 100% sample)")+
  theme_bw()+
  facet_wrap(~Mode,  scales = "free")+
  ggtitle(plotTitle)
plot









scenarios = c("1_L0_d0_c1","1_L1_d0_c1", "2_L1_d0_c1", "3_L1_d0_c1", "4_L1_d0_c1",
              "5_L1_d0_c1","6_L1_d0_c1","7_L1_d0_c1","8_L1_d0_c1",
              "9_L1_d0_c1")
outputNames = c("Base","speed1", "speed2", "speed3", "speed4",
                "speed5","speed6","speed7","speed8",
                "speed9")

allmodesTable <- data.frame(matrix(ncol = 5, nrow = 1))
x <- c("tripMode", "tripCount","CO2emissions_tonn", "averageDistance","scenario")
colnames(allmodesTable) <- x
airGroundTable <- data.frame(matrix(ncol = 5, nrow = 1))
y <- c("modeTwo", "tripCount","CO2emissions_tonn", "averageDistance","scenario")
colnames(airGroundTable) <-y
all <- data.frame(matrix(ncol = 4, nrow = 1))
z <- c("tripCount","CO2emissions_tonn", "averageDistance","scenario")
colnames(all) <-z

for (i in 1:10){
  scenarioName = paste("C:/models/germanymodel/output/5_100_limSpeed/", scenarios[i],"_trips.csv", sep = "")
  trips = read.csv(scenarioName)
  trips <- trips[!grepl("away", trips$tripState),]
  trips = trips %>%
     mutate(modeTwo = if_else(tripMode == "air","air", "ground"))
  test <- trips %>%
     dplyr::group_by(tripMode) %>%
     dplyr::summarize(tripCount = n(), CO2emissions_tonn = sum(CO2emissions_kg) / 1000,
                      averageDistance = mean(travelDistance_km))
  test$scenario <-outputNames[i]
  allmodesTable = bind_rows(allmodesTable,test)
   test2 <- trips %>%
     dplyr::group_by(modeTwo) %>%
     dplyr::summarize(tripCount = n(), CO2emissions_tonn = sum(CO2emissions_kg) / 1000,
                      averageDistance = mean(travelDistance_km))
   test2$scenario <-outputNames[i]
  airGroundTable = bind_rows(airGroundTable,test2)
  test3 <- trips %>%
    dplyr::summarize(tripCount = n(), CO2emissions_tonn = sum(CO2emissions_kg) / 1000,
                     averageDistance = mean(travelDistance_km))
  test3$scenario <-outputNames[i]
  all = bind_rows(all,test3)
}
allmodesTable = allmodesTable[-1,]
airGroundTable = airGroundTable[-1,]
all = all[-1,]
write.csv(allmodesTable,"C:/models/germanymodel/output/paper/5_speedLim_summary4Modes.csv")
write.csv(airGroundTable,"C:/models/germanymodel/output/paper/5_speedLim_summary2Modes.csv")
write.csv(all,"C:/models/germanymodel/output/paper/5_speedLim_summary.csv")

